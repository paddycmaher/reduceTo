// [[Rcpp::depends(RcppParallel)]]

#include <Rcpp.h>
#include <RcppParallel.h>
#include <algorithm>
#include <vector>
#include <cmath>
#include <chrono>
#include <iomanip>
#include <thread>

using namespace Rcpp;
using namespace RcppParallel;

// ============================================================================
//  CONFIG
// ============================================================================
#define BAR_WIDTH 30
#define BAR_START "}"   // Start line
#define BAR_FILL  "~"   // Completed blocks
#define BAR_EMPTY " "   // Remaining blocks
#define BAR_END   "{"   // Finish line
#define NA_BYTE   255  // The 8-bit representation of NA
// Minimum time between progress-bar redraws. ~30fps is well past the point
// of looking perceptibly "instant/smooth" to a human eye, but each redraw
// costs a real, blocking std::flush() -- printing far above this rate (the
// old code redrew on every batch boundary, easily hundreds of times/sec on
// fast runs) burns real compute on I/O nobody can actually see.
#define MIN_UPDATE_INTERVAL_MS 33.0

// Shared 32-bit -> 8-bit packing logic (values already expected to fit 0-254,
// with 255 / NA_INTEGER treated as the NA sentinel). Used both by the
// exhaustive-search entry point and the standalone compression export used
// by the R-side beam search so the packing loop isn't duplicated.
static inline void pack_bytes(const int* src_ptr, uint8_t* dst_ptr, size_t total_cells) {
  for (size_t i = 0; i < total_cells; ++i) {
    int val = src_ptr[i];
    if (val == 255 || val == NA_INTEGER) {
      dst_ptr[i] = NA_BYTE;
    } else {
      dst_ptr[i] = (uint8_t)val;
    }
  }
}

// ============================================================================
//  COMBINATORICS
// ============================================================================

#define TIMESTAMP timestamps.push_back(std::chrono::high_resolution_clock::now())

double calculate_combinations_double(int n, int k) {
  if (k < 0 || k > n) return 0;
  if (k == 0 || k == n) return 1;
  if (k > n - k) k = n - k;
  double result = 1;
  for (int i = 0; i < k; i++) result = result * (n - i) / (i + 1);
  return result;
}

void get_combination_nth(long long target_index, int n, int r, std::vector<int>& result) {
  long long c = 0; 
  for (int i = 0; i < r; ++i) {
    int start_num = (i == 0) ? 0 : result[i-1] + 1;
    for (int j = start_num; j < n; ++j) {
      double nchoosek = calculate_combinations_double(n - j - 1, r - i - 1);
      if (c + nchoosek > target_index) {
        result[i] = j;
        break;
      }
      c += (long long)nchoosek;
    }
  }
}

inline void next_combination(std::vector<int>& indices, int n, int k) {
  for (int i = k - 1; i >= 0; --i) {
    if (indices[i] < n - k + i) {
      indices[i]++;
      for (int j = i + 1; j < k; ++j) {
        indices[j] = indices[i] + (j - i);
      }
      return;
    }
  }
}

// ============================================================================
//  TEMPLATED WORKER (The Efficiency Engine)
// ============================================================================
// We template bool NA_RM so the 'if' statement is removed at compile time.

template <bool NA_RM>
struct ComboWorker : public Worker {
  const uint8_t* data_ptr; // Points to 8-bit compressed data
  const int n_rows;
  const RVector<double> target;
  const int n_items;
  const int num_choose_from; 
  
  std::vector<float>& results;
  
  ComboWorker(const std::vector<uint8_t>& packed_data,
              const NumericVector& target,
              int n_items,
              int num_choose_from,
              std::vector<float>& results)
    : data_ptr(packed_data.data()), n_rows(target.size()), target(target),
      n_items(n_items), num_choose_from(num_choose_from), 
      results(results) {}
  
  void operator()(std::size_t begin, std::size_t end) {
    std::vector<int> current_combo(n_items);
    std::vector<const uint8_t*> col_ptrs(n_items);
    
    get_combination_nth(begin, num_choose_from, n_items, current_combo);
    
    for (std::size_t combo = begin; combo < end; ++combo) {
      if (combo > begin) next_combination(current_combo, num_choose_from, n_items);
      
      // Update pointers (Fast 8-bit pointer arithmetic)
      for(int i = 0; i < n_items; ++i) {
        col_ptrs[i] = data_ptr + (current_combo[i] * n_rows);
      }
      
      double sum_scores = 0.0, sum_scores_sq = 0.0;
      double sum_target = 0.0, sum_target_sq = 0.0;
      double sum_prod = 0.0;
      int valid_n = 0;
      
      for (int row = 0; row < n_rows; ++row) {
        double t = target[row];
        if (NumericVector::is_na(t)) continue; 
        
        int row_sum = 0;
        int valid_items = 0;
        bool possible = true;
        
        // --- INNER LOOP (OPTIMIZED) ---
        for (int i = 0; i < n_items; ++i) {
          uint8_t val = col_ptrs[i][row]; // Load 1 byte (Fast!)
          
          if (val == NA_BYTE) {
            if (NA_RM) {
              // NA_RM is known at compile time, logic is streamlined
              continue; 
            } else {
              possible = false; 
              break; // Fail fast
            }
          }
          
          row_sum += val;
          valid_items++;
        }
        
        if (!possible) continue;
        
        // Score Calculation
        float score;
        if (NA_RM) {
          if (valid_items > 0) score = ((float)row_sum / valid_items) * n_items;
          else continue;
        } else {
          score = (float)row_sum;
        }
        
        sum_scores += score;
        sum_scores_sq += score * score;
        sum_target += t;
        sum_target_sq += t * t;
        sum_prod += score * t;
        valid_n++;
      }
      
      float r = NA_REAL;
      if (valid_n > 1) {
        double ms = sum_scores / valid_n;
        double mt = sum_target / valid_n;
        double vs = (sum_scores_sq / valid_n) - ms * ms;
        double vt = (sum_target_sq / valid_n) - mt * mt;
        double cv = (sum_prod / valid_n) - ms * mt;
        if (vs > 0 && vt > 0) r = (float)(cv / std::sqrt(vs * vt));
      }
      results[combo] = r;
    }
  }
};

// Helpers for UI
std::string format_num(long long num) {
  std::string s = std::to_string(num);
  int n = s.length() - 3;
  while (n > 0) { s.insert(n, ","); n -= 3; }
  return s;
}

std::string format_time(double seconds) {
  if (seconds < 60) {
    return std::to_string((int)seconds) + "s";
  } else if (seconds < 3600) {
    int m = (int)(seconds / 60);
    int s = (int)(seconds) % 60;
    return std::to_string(m) + "m " + std::to_string(s) + "s";
  } else if (seconds < 86400) { // Less than 1 day
    int h = (int)(seconds / 3600);
    int m = (int)((long long)seconds % 3600) / 60;
    return std::to_string(h) + "h " + std::to_string(m) + "m";
  } else if (seconds < 31536000) { // Less than 1 year (365 days)
    int d = (int)(seconds / 86400);
    int h = (int)((long long)seconds % 86400) / 3600;
    return std::to_string(d) + "d " + std::to_string(h) + "h";
  } else { // Years
    int y = (int)(seconds / 31536000);
    int d = (int)((long long)seconds % 31536000) / 86400;
    return std::to_string(y) + "y " + std::to_string(d) + "d";
  }
}

// ============================================================================
//  EXACT PARALLEL TOP-K SELECTION
// ============================================================================
// Splits the combination range into a small FIXED number of equal chunks and
// runs them via parallelFor (the same low-overhead primitive already used
// for scoring) -- each chunk does exactly one partial_sort over its own
// range, writing into its own pre-allocated slot (no shared state, no
// per-split heap allocation). A final serial merge over the (small, bounded)
// set of per-chunk survivors -- at most N_TOPK_CHUNKS * keep_top candidates
// -- produces the true top-K in a single cheap partial_sort.
//
// This is exact for the same reason a tree-reduction top-K is: if a
// combination is truly in the global top-K, fewer than K combinations beat
// it ANYWHERE, so fewer than K beat it within its own chunk either -- its
// local rank is always <= K, so it always survives its own chunk's
// selection and is present when the final merge runs. No chunking scheme or
// data distribution can cause a genuine top-K result to be dropped.
//
// (Two earlier versions tried here, both correct but similarly slow on huge
// n_combos: RcppParallel's parallelReduce with a tree-of-joins design, and a
// chunked parallelFor design that materialized each chunk as (score, index)
// pairs and ran partial_sort over the whole chunk. Both effectively touch
// and reorder the ENTIRE chunk just to keep the top handful -- partial_sort
// on millions of elements to keep 100 is still real, non-trivial work no
// matter how the elements are laid out. Fixed below with a genuine
// single-pass streaming top-K per chunk: a small (size keep_top) min-heap
// that only gets touched when a candidate actually beats the current worst
// kept item. The overwhelming majority of combinations fail that one
// comparison and cost nothing further -- no allocation, no reordering, no
// touching the heap at all -- so total work is close to a single sequential
// read of results[], not an O(n log k) sort of it.
#define N_TOPK_CHUNKS 256

struct ScoreIdx {
  float score;  // NA already mapped to -1 by the caller -- always sorts worst
  int idx;
};

// std::greater-style comparator -> push_heap/pop_heap maintain a MIN-heap,
// so heap.front() is always the current worst kept item (the one a new
// candidate must beat to be worth inserting).
static inline bool score_idx_min_heap_cmp(const ScoreIdx& a, const ScoreIdx& b) {
  return a.score > b.score;
}

struct ChunkTopKWorker : public Worker {
  const std::vector<float>& results;
  const int n_top;
  const long long n_combos;
  const int num_chunks;
  std::vector<std::vector<ScoreIdx>>& chunk_results;

  ChunkTopKWorker(const std::vector<float>& results, int n_top, long long n_combos,
                   int num_chunks, std::vector<std::vector<ScoreIdx>>& chunk_results)
    : results(results), n_top(n_top), n_combos(n_combos),
      num_chunks(num_chunks), chunk_results(chunk_results) {}

  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t c = begin; c < end; ++c) {
      long long chunk_begin = (long long)c * n_combos / num_chunks;
      long long chunk_end = (long long)(c + 1) * n_combos / num_chunks;
      int local_n = (int)(chunk_end - chunk_begin);
      int k = std::min(n_top, local_n);

      std::vector<ScoreIdx> heap;
      heap.reserve(k);

      const float* src = results.data() + chunk_begin;
      for (int i = 0; i < local_n; ++i) {
        float raw = src[i];
        float mag = std::isnan(raw) ? -1.0f : std::abs(raw);

        if ((int)heap.size() < k) {
          heap.push_back({mag, (int)chunk_begin + i});
          std::push_heap(heap.begin(), heap.end(), score_idx_min_heap_cmp);
        } else if (mag > heap.front().score) {
          std::pop_heap(heap.begin(), heap.end(), score_idx_min_heap_cmp);
          heap.back() = {mag, (int)chunk_begin + i};
          std::push_heap(heap.begin(), heap.end(), score_idx_min_heap_cmp);
        }
      }

      chunk_results[c] = std::move(heap);
    }
  }
};

// Runs ChunkTopKWorker over [0, n_combos) and returns the true top-K indices
// (sorted best-first, size <= keep_top).
std::vector<int> parallel_top_k(const std::vector<float>& results, int n_combos, int keep_top) {
  int n_top = std::min(keep_top, n_combos);
  int num_chunks = std::min((long long)N_TOPK_CHUNKS, (long long)n_combos);
  if (num_chunks < 1) num_chunks = 1;

  std::vector<std::vector<ScoreIdx>> chunk_results(num_chunks);
  ChunkTopKWorker chunk_worker(results, n_top, n_combos, num_chunks, chunk_results);
  parallelFor(0, num_chunks, chunk_worker);

  std::vector<ScoreIdx> merged;
  merged.reserve((size_t)num_chunks * n_top);
  for (auto& cr : chunk_results) merged.insert(merged.end(), cr.begin(), cr.end());

  // Same comparator as the per-chunk heap (a.score > b.score): here it gives
  // descending / best-first order, which is what partial_sort needs.
  int final_k = std::min(n_top, (int)merged.size());
  std::partial_sort(merged.begin(), merged.begin() + final_k, merged.end(), score_idx_min_heap_cmp);
  merged.resize(final_k);

  std::vector<int> idx(final_k);
  for (int i = 0; i < final_k; ++i) idx[i] = merged[i].idx;
  return idx;
}

// [[Rcpp::export]]
List process_all_combinations_cpp_parallel_float(
    IntegerMatrix data,
    int n_items,
    int num_choose_from,
    bool na_rm,
    NumericVector target,
    IntegerVector original_indices,
    int keep_top = 100,
    bool show_progress = true
) 
{
  double n_combos_d = calculate_combinations_double(num_choose_from, n_items);
  if (n_combos_d > 2000000000) stop("Too many combinations (>2B).");
  int n_combos = (int)n_combos_d;
  int n_rows = data.nrow();
  int n_cols = data.ncol();
  
  std::vector<std::chrono::high_resolution_clock::time_point> timestamps;
  TIMESTAMP; // 1
  
  // --- DATA COMPRESSION STEP ---
  // Convert 32-bit IntegerMatrix to 8-bit vector.
  // This reduces memory bandwidth by 4x during the heavy loops.
  size_t total_cells = (size_t)n_rows * n_cols;
  std::vector<uint8_t> packed_data(total_cells);
  pack_bytes(&data[0], packed_data.data(), total_cells);

  TIMESTAMP; // 2
  
  std::vector<float> results(n_combos);
  
  auto start_time = std::chrono::high_resolution_clock::now();
  
  // --- DISPATCHER ---
  // Create the correct worker type based on na_rm
  // This enables the "Templated Worker" optimization
  
  if (na_rm) {
    ComboWorker<true> worker(packed_data, target, n_items, num_choose_from, results);
    
    // UI / Batch Loop (Duplicate logic, but necessary for templating)
    if (!show_progress) {
      parallelFor(0, n_combos, worker);
    } else {
      int batch_size = std::max(1000, n_combos / 60);  // dispatch granularity -- decoupled from print rate (throttled separately) above
      double last_print_ms = -MIN_UPDATE_INTERVAL_MS;
      for (int start = 0; start < n_combos; start += batch_size) {
        int end = std::min(start + batch_size, n_combos);
        parallelFor(start, end, worker);
        Rcpp::checkUserInterrupt();

        // Progress Bar Update -- throttled to a real, human-perceptible
        // refresh rate rather than redrawing on every batch boundary
        auto now = std::chrono::high_resolution_clock::now();
        double ms_since_start = std::chrono::duration_cast<std::chrono::milliseconds>(now - start_time).count();
        bool is_last_batch = (end == n_combos);
        if (ms_since_start - last_print_ms >= MIN_UPDATE_INTERVAL_MS || is_last_batch) {
          last_print_ms = ms_since_start;
          double seconds = std::max(ms_since_start, 1.0) / 1000.0;
          double pct = (double)end / n_combos;
          double speed = end / seconds;
          double eta = (n_combos - end) / speed;
          int filled = (int)(BAR_WIDTH * pct);
          std::string bar = BAR_START;
          for (int i = 0; i < BAR_WIDTH; ++i) bar += (i < filled ? BAR_FILL : BAR_EMPTY);
          bar += BAR_END;
          Rcpp::Rcout << "\r" << bar << " " << std::fixed << std::setprecision(0) << (pct * 100) << "%"
                      << " | " << format_num((long long)speed) << "/s"
                      << " | ETA: " << format_time(eta) << "   " << std::flush;
        }
      }
    }

  } else {
    // Exact same logic, but for the <false> template
    ComboWorker<false> worker(packed_data, target, n_items, num_choose_from, results);

    if (!show_progress) {
      parallelFor(0, n_combos, worker);
    } else {
      int batch_size = std::max(1000, n_combos / 60);  // dispatch granularity -- decoupled from print rate (throttled separately) above
      double last_print_ms = -MIN_UPDATE_INTERVAL_MS;
      for (int start = 0; start < n_combos; start += batch_size) {
        int end = std::min(start + batch_size, n_combos);
        parallelFor(start, end, worker);
        Rcpp::checkUserInterrupt();

        // Progress Bar Update (Code duplication unavoidable without complex wrapper)
        auto now = std::chrono::high_resolution_clock::now();
        double ms_since_start = std::chrono::duration_cast<std::chrono::milliseconds>(now - start_time).count();
        bool is_last_batch = (end == n_combos);
        if (ms_since_start - last_print_ms >= MIN_UPDATE_INTERVAL_MS || is_last_batch) {
          last_print_ms = ms_since_start;
          double seconds = std::max(ms_since_start, 1.0) / 1000.0;
          double pct = (double)end / n_combos;
          double speed = end / seconds;
          double eta = (n_combos - end) / speed;
          int filled = (int)(BAR_WIDTH * pct);
          std::string bar = BAR_START;
          for (int i = 0; i < BAR_WIDTH; ++i) bar += (i < filled ? BAR_FILL : BAR_EMPTY);
          bar += BAR_END;
          Rcpp::Rcout << "\r" << bar << " " << std::fixed << std::setprecision(0) << (pct * 100) << "%"
                      << " | " << format_num((long long)speed) << "/s"
                      << " | ETA: " << format_time(eta) << "   " << std::flush;
        }
      }
    }
  }
  
  if (show_progress) Rcpp::Rcout << "\r" << std::string(80, ' ') << "\r";
  
  TIMESTAMP; // 3
  
  // Sorting & Output -- exact parallel top-K (see parallel_top_k above)
  // instead of a single-threaded partial_sort: on large runs the old sort
  // was a serial bottleneck with no progress indication.
  int n_top = std::min(keep_top, n_combos);
  std::vector<int> idx = parallel_top_k(results, n_combos, keep_top);

  TIMESTAMP; // 4
  
  CharacterVector comb_out(n_top);
  NumericVector r_out(n_top);
  IntegerVector idx_out(n_top);
  std::vector<int> temp_combo(n_items);
  
  for (int i = 0; i < n_top; ++i) {
    int combo_id = idx[i];
    get_combination_nth(combo_id, num_choose_from, n_items, temp_combo);
    std::string s;
    for(int k=0; k<n_items; ++k) {
      if(k) s += ",";
      s += std::to_string(original_indices[temp_combo[k]]);
    }
    comb_out[i] = s;
    r_out[i] = results[combo_id];
    idx_out[i] = combo_id;
  }
  
  TIMESTAMP; // 5
  
  NumericVector timings(timestamps.size() - 1);
  for (size_t i = 0; i < timestamps.size() - 1; i++) {
    auto d = std::chrono::duration_cast<std::chrono::milliseconds>(timestamps[i+1] - timestamps[i]);
    timings[i] = d.count() / 1000.0;
  }
  
  return List::create(
    Named("combination") = comb_out,
    Named("r") = r_out,
    Named("combo_indices") = idx_out,
    Named("timings_cpp") = timings
  );
}

// ============================================================================
//  GRAM-MATRIX WORKER (Fast path: scores combinations from precomputed
//  column moments instead of re-reading raw rows per combination)
// ============================================================================
// Only valid when the data behind gram/col_sums/col_target_dots is complete
// (no missingness) over the n_valid rows used to build them -- the caller
// is responsible for imputing or excluding rows before precomputing these
// inputs. Reproduces the same statistic ComboWorker computes on complete
// data: with no missing values, ComboWorker's per-row score degenerates to
// the plain sum of the k selected columns (valid_items == n_items always),
// which is exactly what the moment decomposition below computes.
//
// Every combination's r depends on just two totals: T, the sum of its
// items' covariances with the target, and Q, the sum of every pairwise
// covariance among its items (own variances included) -- r = T / sqrt(Q *
// var_target). The previous version rebuilt both from scratch for every
// combination, a k x k double loop (225 lookups at k = 15), even though
// neighbouring combinations share almost all their items.
//
// This version walks the combinations as a tree instead: ABCD, ABCE and
// ABCF all hang off the same ABC branch. It keeps running T/Q subtotals for
// the branch it's holding, plus a running tally of how much every remaining
// item covaries with that branch as a whole -- so finishing a combination
// (e.g. adding D to ABC) takes three lookups whatever k is:
//   T = T_ABC + t_D,   Q = Q_ABC + C_DD + 2 * tally_D
// Updating the tally when moving to a new branch is shared by every
// combination under it, so the cost per combination is a small constant
// rather than O(k^2): measured 50-78x faster at final-stage sizes (k = 9-15)
// on real data, with identical results.
//
// The top-K is kept on the fly in a small per-thread min-heap (a candidate
// only touches it when it beats the worst kept one) rather than storing
// every combination's r first -- so memory no longer grows with the number
// of combinations (the old results array was 4 bytes per combination) and
// there's no 2-billion combination cap on this path. Exact ties are broken
// towards the lexicographically smaller combination, so the kept set never
// depends on how threads happened to split the work.
//
// Each item's best |r| over EVERY combination scored is also tracked on the
// way (the best finish under a branch is credited once to that branch's
// items), which Synergistic RFE uses to rank items directly instead of
// reconstructing it from a truncated top-K list.

struct GramDFSWorker : public Worker {
  const std::vector<double>& C;     // p x p centered cross-products, column-major
  const std::vector<double>& diagC; // C(j, j)
  const std::vector<double>& t;     // centered item-target cross-products
  const double ssy;                 // centered target sum of squares
  const int p, k, D, K;
  const std::vector<int>& tasks;    // flattened branch prefixes (length D each)

  struct HeapEntry { double key; double r; int slot; };
  std::vector<HeapEntry> heap;      // min-heap: front() is the worst kept entry
  std::vector<int> slots;           // K x k item indices, one row per kept entry
  std::vector<double> item_best;    // best key (T^2 / Q) per item (-1 = never scored)

  // per-worker scratch, reused across calls
  std::vector<double> R;            // k x p running tallies, R[d * p + j]
  std::vector<double> T, Q;
  std::vector<int> idx;

  GramDFSWorker(const std::vector<double>& C, const std::vector<double>& diagC,
                const std::vector<double>& t, double ssy, int p, int k, int D, int K,
                const std::vector<int>& tasks)
    : C(C), diagC(diagC), t(t), ssy(ssy), p(p), k(k), D(D), K(K), tasks(tasks),
      slots((size_t)K * k), item_best(p, -1.0) { heap.reserve(K); }

  GramDFSWorker(const GramDFSWorker& o, Split)
    : C(o.C), diagC(o.diagC), t(o.t), ssy(o.ssy), p(o.p), k(o.k), D(o.D), K(o.K), tasks(o.tasks),
      slots((size_t)o.K * o.k), item_best(o.p, -1.0) { heap.reserve(o.K); }

  // true if (key_a, combo_a) should rank above (key_b, combo_b)
  inline bool better(double key_a, const int* a, double key_b, const int* b) const {
    if (key_a != key_b) return key_a > key_b;
    for (int d = 0; d < k; ++d) if (a[d] != b[d]) return a[d] < b[d];
    return false;
  }
  inline const int* combo_of(const HeapEntry& e) const { return &slots[(size_t)e.slot * k]; }

  struct HeapCmp {
    const GramDFSWorker* w;
    bool operator()(const HeapEntry& a, const HeapEntry& b) const {
      return w->better(a.key, w->combo_of(a), b.key, w->combo_of(b));
    }
  };

  inline double threshold() const { return ((int)heap.size() < K) ? -2.0 : heap.front().key; }

  // Offer a candidate whose items are in `cand` (length k)
  inline void offer(double key, double r, const int* cand) {
    HeapCmp cmp{this};
    if ((int)heap.size() < K) {
      int slot = (int)heap.size();
      std::copy(cand, cand + k, slots.begin() + (size_t)slot * k);
      heap.push_back({key, r, slot});
      std::push_heap(heap.begin(), heap.end(), cmp);
    } else if (better(key, cand, heap.front().key, combo_of(heap.front()))) {
      std::pop_heap(heap.begin(), heap.end(), cmp);
      HeapEntry& e = heap.back();
      std::copy(cand, cand + k, slots.begin() + (size_t)e.slot * k);
      e.key = key; e.r = r;
      std::push_heap(heap.begin(), heap.end(), cmp);
    }
  }

  // Extend the branch of length d by idx[d]: update T/Q and the tallies of
  // every later item against the branch
  inline void push(int d) {
    const int i = idx[d];
    T[d + 1] = T[d] + t[i];
    Q[d + 1] = Q[d] + diagC[i] + 2.0 * R[(size_t)d * p + i];
    const double* Ci = &C[(size_t)i * p];   // column i == row i (symmetric)
    const double* Rd = &R[(size_t)d * p];
    double* Rn = &R[(size_t)(d + 1) * p];
    for (int j = i + 1; j < p; ++j) Rn[j] = Rd[j] + Ci[j];
  }

  // Branch of length k - 1 is set: finish it with every remaining item.
  // Everything is compared on key = T^2 / Q (proportional to r^2, so it
  // ranks by |r|), cross-multiplied so the common case -- a combination that
  // beats neither its items' bests nor the heap -- costs no division or sqrt
  inline void finish_branch(int start) {
    const int d = k - 1;
    const double Tp = T[d], Qp = Q[d];
    const double* Rd = &R[(size_t)d * p];
    double branch_best = -1.0;
    double thr = threshold();
    for (int j = start; j < p; ++j) {
      const double Tj = Tp + t[j];
      const double Qj = Qp + diagC[j] + 2.0 * Rd[j];
      if (Qj > 0) {
        const double T2 = Tj * Tj;
        if (T2 > item_best[j] * Qj) item_best[j] = T2 / Qj;
        if (T2 > branch_best * Qj) branch_best = T2 / Qj;
        if (T2 >= thr * Qj) {
          idx[d] = j;
          offer(T2 / Qj, Tj / std::sqrt(Qj * ssy), idx.data());
          thr = threshold();
        }
      } else if (thr <= -1.0) {             // invalid (zero-variance) combination:
        idx[d] = j;                         // only kept while the heap isn't full
        offer(-1.0, NA_REAL, idx.data());
        thr = threshold();
      }
    }
    for (int dd = 0; dd < d; ++dd) if (branch_best > item_best[idx[dd]]) item_best[idx[dd]] = branch_best;
  }

  void walk(int d, int start) {           // branch of length d is set
    if (d == k - 1) { finish_branch(start); return; }
    for (int i = start; i <= p - (k - d); ++i) {
      idx[d] = i; push(d); walk(d + 1, i + 1);
    }
  }

  void operator()(std::size_t begin, std::size_t end) {
    if (R.empty()) {
      R.assign((size_t)k * p, 0.0); T.assign(k + 1, 0.0); Q.assign(k + 1, 0.0); idx.assign(k, 0);
    }
    std::vector<int> cur(D, -1);
    for (std::size_t task = begin; task < end; ++task) {
      const int* pre = D > 0 ? &tasks[task * D] : nullptr;
      int same = 0;                          // consecutive tasks share leading items -- reuse them
      while (same < D && cur[same] == pre[same]) ++same;
      for (int d = same; d < D; ++d) { idx[d] = pre[d]; cur[d] = pre[d]; push(d); }
      walk(D, D > 0 ? pre[D - 1] + 1 : 0);
    }
  }

  void join(const GramDFSWorker& o) {
    for (int j = 0; j < p; ++j) if (o.item_best[j] > item_best[j]) item_best[j] = o.item_best[j];
    for (const HeapEntry& e : o.heap) offer(e.key, e.r, o.combo_of(e));
  }
};

static void gen_branch_prefixes(int p, int k, int D, int pos, int start,
                                std::vector<int>& cur, std::vector<int>& out) {
  if (pos == D) { out.insert(out.end(), cur.begin(), cur.end()); return; }
  for (int i = start; i <= p - (k - pos); ++i) {
    cur[pos] = i; gen_branch_prefixes(p, k, D, pos + 1, i + 1, cur, out);
  }
}

// [[Rcpp::export]]
List process_all_combinations_cpp_gram(
    NumericMatrix gram,
    NumericVector col_sums,
    NumericVector col_target_dots,
    double sum_target,
    double sum_target_sq,
    double n_valid,
    int n_items,
    int num_choose_from,
    IntegerVector original_indices,
    int keep_top = 100,
    bool show_progress = true
) {
  const int p = num_choose_from, k = n_items;
  if (k < 1 || k > p) stop("n_items must be between 1 and num_choose_from.");
  double n_combos_d = calculate_combinations_double(p, k);

  std::vector<std::chrono::high_resolution_clock::time_point> timestamps;
  TIMESTAMP; // 1

  // Center the moments once, so r = T / sqrt(Q * ssy) with no per-combination
  // mean corrections
  std::vector<double> C((size_t)p * p), diagC(p), t(p);
  for (int j = 0; j < p; ++j) {
    for (int i = 0; i < p; ++i) C[(size_t)j * p + i] = gram(i, j) - col_sums[i] * col_sums[j] / n_valid;
    diagC[j] = C[(size_t)j * p + j];
    t[j] = col_target_dots[j] - col_sums[j] * sum_target / n_valid;
  }
  double ssy = sum_target_sq - sum_target * sum_target / n_valid;
  if (!(n_valid > 1 && ssy > 0)) ssy = NA_REAL;

  // Split the tree into branches deep enough that no single branch holds
  // more than a small share of the work (keeps every thread busy even when
  // the first branch is by far the biggest, e.g. 15 of 26)
  int n_threads = std::max(1u, std::thread::hardware_concurrency());
  int D = 0;
  while (D < k - 1 && calculate_combinations_double(p - D, k - D) > n_combos_d / (8.0 * n_threads) &&
         calculate_combinations_double(p, D + 1) <= 5e6) ++D;
  if (D == 0 && k > 1) D = 1;
  std::vector<int> tasks, cur(D);
  if (D > 0) gen_branch_prefixes(p, k, D, 0, 0, cur, tasks);
  int n_tasks = D > 0 ? (int)(tasks.size() / D) : 1;

  auto branch_size = [&](int task) {
    return D > 0 ? calculate_combinations_double(p - 1 - tasks[(size_t)task * D + D - 1], k - D) : n_combos_d;
  };

  int n_top = (int)std::min((double)keep_top, n_combos_d);
  GramDFSWorker master(C, diagC, t, ssy, p, k, D, n_top, tasks);

  auto start_time = std::chrono::high_resolution_clock::now();
  // Work in batches of branches: lets the user interrupt, and drives the
  // progress bar (dispatch granularity -- decoupled from print rate below)
  double batch_target = std::max(n_combos_d / 60.0, 2e7);
  double last_print_ms = -MIN_UPDATE_INTERVAL_MS;
  double done = 0;
  int b0 = 0;
  while (b0 < n_tasks) {
    int b1 = b0; double batch_combos = 0;
    while (b1 < n_tasks && (batch_combos < batch_target || b1 == b0)) batch_combos += branch_size(b1++);

    GramDFSWorker worker(C, diagC, t, ssy, p, k, D, n_top, tasks);
    parallelReduce(b0, b1, worker, 1);
    master.join(worker);
    done += batch_combos;
    b0 = b1;
    Rcpp::checkUserInterrupt();

    if (show_progress) {
      auto now = std::chrono::high_resolution_clock::now();
      double ms_since_start = std::chrono::duration_cast<std::chrono::milliseconds>(now - start_time).count();
      bool is_last_batch = (b0 == n_tasks);
      if (ms_since_start - last_print_ms >= MIN_UPDATE_INTERVAL_MS || is_last_batch) {
        last_print_ms = ms_since_start;
        double seconds = std::max(ms_since_start, 1.0) / 1000.0;
        double pct = done / n_combos_d;
        double speed = done / seconds;
        double eta = (n_combos_d - done) / speed;
        int filled = (int)(BAR_WIDTH * pct);
        std::string bar = BAR_START;
        for (int i = 0; i < BAR_WIDTH; ++i) bar += (i < filled ? BAR_FILL : BAR_EMPTY);
        bar += BAR_END;
        Rcpp::Rcout << "\r" << bar << " " << std::fixed << std::setprecision(0) << (pct * 100) << "%"
                    << " | " << format_num((long long)speed) << "/s"
                    << " | ETA: " << format_time(eta) << "   " << std::flush;
      }
    }
  }

  if (show_progress) Rcpp::Rcout << "\r" << std::string(80, ' ') << "\r";

  TIMESTAMP; // 2

  // Best-first order (same tie-breaking as the heap)
  GramDFSWorker::HeapCmp cmp{&master};
  std::vector<GramDFSWorker::HeapEntry> kept = master.heap;
  std::sort(kept.begin(), kept.end(), cmp);

  TIMESTAMP; // 3

  n_top = (int)kept.size();
  CharacterVector comb_out(n_top);
  NumericVector r_out(n_top);
  NumericVector idx_out(n_top);

  for (int i = 0; i < n_top; ++i) {
    const int* cmb = master.combo_of(kept[i]);
    std::string s;
    double rank = 0;   // lexicographic combination index, as before
    for (int d = 0; d < k; ++d) {
      if (d) s += ",";
      s += std::to_string(original_indices[cmb[d]]);
      for (int j = (d == 0 ? 0 : cmb[d - 1] + 1); j < cmb[d]; ++j) rank += calculate_combinations_double(p - j - 1, k - d - 1);
    }
    comb_out[i] = s;
    r_out[i] = kept[i].r;
    idx_out[i] = rank;
  }

  NumericVector item_best(p);
  for (int j = 0; j < p; ++j) item_best[j] = master.item_best[j] < 0 ? NA_REAL : std::sqrt(master.item_best[j] / ssy);

  TIMESTAMP; // 4

  NumericVector timings(timestamps.size() - 1);
  for (size_t i = 0; i < timestamps.size() - 1; i++) {
    auto d = std::chrono::duration_cast<std::chrono::milliseconds>(timestamps[i+1] - timestamps[i]);
    timings[i] = d.count() / 1000.0;
  }

  return List::create(
    Named("combination") = comb_out,
    Named("r") = r_out,
    Named("combo_indices") = idx_out,
    Named("item_best") = item_best,
    Named("timings_cpp") = timings
  );
}
