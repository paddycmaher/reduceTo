// [[Rcpp::depends(RcppParallel)]]

#include <Rcpp.h>
#include <RcppParallel.h>
#include <algorithm>
#include <vector>
#include <cmath>
#include <chrono>
#include <iomanip>
#include <thread>
#include <cstring>
#include <cstdint>

#include "gram_engine.h"   // progress-bar settings, the Gram tree-walk worker and its branch splitter
#include "parallel_guard.h"   // parallel loops that fall back to one thread in a fork()'d child

// The Rcpp and RcppParallel names this file uses unqualified; everything else is written in full (Rcpp::stop, ...)
using Rcpp::NumericVector;
using Rcpp::NumericMatrix;
using Rcpp::IntegerVector;
using Rcpp::IntegerMatrix;
using Rcpp::CharacterVector;
using Rcpp::List;
using Rcpp::Named;
using RcppParallel::Worker;
using RcppParallel::RVector;
using RcppParallel::RMatrix;


// ============================================================================
//  CONFIG
// ============================================================================
constexpr uint8_t NA_BYTE = 255;   // The 8-bit representation of NA

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

// Marks the end of a timed step: each entry point keeps a list of these, reported as seconds per step
using Clock = std::chrono::high_resolution_clock;
static inline void stamp(std::vector<Clock::time_point>& t) { t.push_back(Clock::now()); }


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
// We template bool NA_RM so the 'if' statement is removed at compile time, and
// FIXED for searches where some items belong to every combination (the knockout search's
// kept items under speed = "conservative"): their per-row sums and counts are
// computed once and start every row's total.

template <bool NA_RM, bool FIXED>
struct ComboWorker : public Worker {
  const uint8_t* data_ptr; // Points to 8-bit compressed data
  const int n_rows;
  const RVector<double> target;
  const int n_items;
  const int num_choose_from; 
  const int n_fixed;
  const int* base_sum;     // per row: sum / count of the fixed items present,
  const int* base_valid;   // and whether all of them are (na.rm = FALSE)
  const char* base_ok;
  
  std::vector<float>& results;
  
  ComboWorker(const std::vector<uint8_t>& packed_data,
              const NumericVector& target,
              int n_items,
              int num_choose_from,
              int n_fixed, const int* base_sum, const int* base_valid, const char* base_ok,
              std::vector<float>& results)
    : data_ptr(packed_data.data()), n_rows(target.size()), target(target),
      n_items(n_items), num_choose_from(num_choose_from), n_fixed(n_fixed),
      base_sum(base_sum), base_valid(base_valid), base_ok(base_ok),
      results(results) {}
  
  void operator()(std::size_t begin, std::size_t end) {
    std::vector<int> current_combo(n_items);
    std::vector<const uint8_t*> col_ptrs(n_items);
    
    get_combination_nth(begin, num_choose_from, n_items, current_combo);
    
    for (std::size_t combo = begin; combo < end; ++combo) {
      if (combo > begin) next_combination(current_combo, num_choose_from, n_items);
      
      // Update pointers (Fast 8-bit pointer arithmetic)
      for(int i = 0; i < n_items; ++i) {
        col_ptrs[i] = data_ptr + (size_t)current_combo[i] * n_rows;
      }
      
      double sum_scores = 0.0, sum_scores_sq = 0.0;
      double sum_target = 0.0, sum_target_sq = 0.0;
      double sum_prod = 0.0;
      int valid_n = 0;
      
      for (int row = 0; row < n_rows; ++row) {
        double t = target[row];
        if (NumericVector::is_na(t)) continue; 
        if (FIXED && !NA_RM && !base_ok[row]) continue;
        
        int row_sum = FIXED ? base_sum[row] : 0;
        int valid_items = FIXED ? base_valid[row] : 0;
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
          if (valid_items > 0) score = ((float)row_sum / valid_items) * (n_items + (FIXED ? n_fixed : 0));
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
constexpr int N_TOPK_CHUNKS = 256;

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
  par_for(0, num_chunks, chunk_worker);

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

// Scores every combination with one worker type, in batches that let the user
// interrupt (with or without the progress bar: one unbatched call couldn't be
// stopped until it had scored everything)
template <bool NA_RM, bool FIXED>
static void run_combo_search(const std::vector<uint8_t>& packed_data, const NumericVector& target, int n_items, int num_choose_from,
                             int n_fixed, const std::vector<int>& base_sum, const std::vector<int>& base_valid,
                             const std::vector<char>& base_ok, std::vector<float>& results, int n_combos, bool show_progress) {
  ComboWorker<NA_RM, FIXED> worker(packed_data, target, n_items, num_choose_from, n_fixed,
                                   base_sum.data(), base_valid.data(), base_ok.data(), results);

  auto start_time = std::chrono::high_resolution_clock::now();
  int batch_size = std::min(1000, n_combos);  // first batch only -- then sized from measured throughput to take ~TARGET_BATCH_MS
  double last_print_ms = -MIN_UPDATE_INTERVAL_MS;
  for (int start = 0, end = 0; start < n_combos; start = end) {
    end = start + batch_size;
    par_for(start, end, worker);
    Rcpp::checkUserInterrupt();
    double ms_done = std::chrono::duration<double, std::milli>(std::chrono::high_resolution_clock::now() - start_time).count();
    batch_size = (int)std::min(std::max(end / std::max(ms_done, 1.0) * TARGET_BATCH_MS, 1000.0), (double)(n_combos - end));

    // Progress Bar Update -- throttled to a real, human-perceptible
    // refresh rate rather than redrawing on every batch boundary
    if (!show_progress) continue;
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
  if (show_progress) Rcpp::Rcout << "\r" << std::string(80, ' ') << "\r";
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
    bool show_progress = true,
    int n_fixed = 0           // the last n_fixed columns belong to every combination
) 
{
  if (n_fixed > 0 && data.ncol() != num_choose_from + n_fixed) Rcpp::stop("data must hold the num_choose_from candidates followed by the n_fixed fixed items.");
  double n_combos_d = calculate_combinations_double(num_choose_from, n_items);
  if (n_combos_d > 2000000000) Rcpp::stop("Too many combinations (>2B).");
  int n_combos = (int)n_combos_d;
  int n_rows = data.nrow();
  int n_cols = data.ncol();
  
  std::vector<Clock::time_point> timestamps;
  stamp(timestamps); // 1
  
  // --- DATA COMPRESSION STEP ---
  // Convert 32-bit IntegerMatrix to 8-bit vector.
  // This reduces memory bandwidth by 4x during the heavy loops.
  size_t total_cells = (size_t)n_rows * n_cols;
  std::vector<uint8_t> packed_data(total_cells);
  pack_bytes(&data[0], packed_data.data(), total_cells);

  stamp(timestamps); // 2
  
  std::vector<float> results(n_combos);

  // Fixed items (the last n_fixed columns): per-row sums, counts and completeness, computed once
  std::vector<int> base_sum, base_valid; std::vector<char> base_ok;
  if (n_fixed > 0) {
    base_sum.assign(n_rows, 0); base_valid.assign(n_rows, 0); base_ok.assign(n_rows, 1);
    for (int f = 0; f < n_fixed; ++f) {
      const uint8_t* col = packed_data.data() + (size_t)(num_choose_from + f) * n_rows;
      for (int row = 0; row < n_rows; ++row) {
        if (col[row] == NA_BYTE) base_ok[row] = 0; else { base_sum[row] += col[row]; ++base_valid[row]; }
      }
    }
  }

  // --- DISPATCHER ---
  // Create the correct worker type based on na_rm and fixed items
  // This enables the "Templated Worker" optimization
  if (na_rm) {
    if (n_fixed > 0) run_combo_search<true, true>(packed_data, target, n_items, num_choose_from, n_fixed, base_sum, base_valid, base_ok, results, n_combos, show_progress);
    else run_combo_search<true, false>(packed_data, target, n_items, num_choose_from, n_fixed, base_sum, base_valid, base_ok, results, n_combos, show_progress);
  } else {
    if (n_fixed > 0) run_combo_search<false, true>(packed_data, target, n_items, num_choose_from, n_fixed, base_sum, base_valid, base_ok, results, n_combos, show_progress);
    else run_combo_search<false, false>(packed_data, target, n_items, num_choose_from, n_fixed, base_sum, base_valid, base_ok, results, n_combos, show_progress);
  }
  
  stamp(timestamps); // 3
  
  // Sorting & Output -- exact parallel top-K (see parallel_top_k above)
  // instead of a single-threaded partial_sort: on large runs the old sort
  // was a serial bottleneck with no progress indication.
  int n_top = std::min(keep_top, n_combos);
  std::vector<int> idx = parallel_top_k(results, n_combos, keep_top);

  stamp(timestamps); // 4
  
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
  
  stamp(timestamps); // 5
  
  // Seconds per section (microsecond resolution: whole milliseconds rounded short sections to 0)
  NumericVector timings(timestamps.size() - 1);
  for (size_t i = 0; i < timestamps.size() - 1; i++) {
    auto d = std::chrono::duration_cast<std::chrono::microseconds>(timestamps[i+1] - timestamps[i]);
    timings[i] = d.count() / 1e6;
  }
  timings.attr("names") = CharacterVector::create("pack", "search", "top_k", "format");
  
  return List::create(
    Named("combination") = comb_out,
    Named("r") = r_out,
    Named("combo_indices") = idx_out,
    Named("timings_cpp") = timings
  );
}

// ============================================================================
//  GRAM-MATRIX SEARCH (the worker and branch splitter live in gram_engine.h)
// ============================================================================

// max_seconds > 0 estimates the whole search's time in about that long instead of running it (est_seconds,
// see below; `covered`: the combinations searched on the way). screen_ratio: where the bound tests start
// paying (see gram_engine.h; 8 times the knockout's searches of close calls)
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
    bool show_progress = true,
    double max_seconds = 0,
    double screen_ratio = 4
) {
  const int p = num_choose_from, k = n_items;
  if (k < 1 || k > p) Rcpp::stop("n_items must be between 1 and num_choose_from.");
  double n_combos_d = calculate_combinations_double(p, k);

  std::vector<Clock::time_point> timestamps;
  stamp(timestamps); // 1

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

  // Each branch runs on one thread, so keep every branch small: at most a
  // small share of the whole search, and at most ~5M combinations (a bigger
  // one leaves the other threads idle at the end of every ~TARGET_BATCH_MS
  // batch). Every batch also holds several branches per thread
  int n_threads = std::max(1u, std::thread::hardware_concurrency());
  BranchGenerator gen(p, k, std::min(n_combos_d / (8.0 * n_threads), 5e6));
  const int MIN_BRANCHES_PER_BATCH = 4 * n_threads;
  std::vector<int> task_items, task_off;

  int n_top = (int)std::min((double)keep_top, n_combos_d);
  GramSearch search(C, diagC, t, ssy, p, k, n_top, 0, 0, screen_ratio);
  if (search.hY > 0) gen.set_block(p - search.hY);
  GramSearchWorker master(search, task_items, task_off);

  auto start_time = std::chrono::high_resolution_clock::now();
  // Work in batches of branches: lets the user interrupt, and drives the
  // progress bar. The first batch is a small fixed size; every later one is
  // sized from measured throughput to take ~TARGET_BATCH_MS
  double batch_target = 2e7;
  double last_print_ms = -MIN_UPDATE_INTERVAL_MS;
  double done = 0;
  bool finished = false;
  while (!finished) {
    task_items.clear(); task_off.assign(1, 0);
    double batch_combos = 0;
    while (batch_combos < batch_target || (int)task_off.size() - 1 < MIN_BRANCHES_PER_BATCH) {
      double size = gen.next(task_items);
      if (size == 0) { finished = true; break; }
      task_off.push_back((int)task_items.size());
      batch_combos += size;
    }
    if (task_off.size() == 1) break;

    GramSearchWorker worker(search, task_items, task_off);
    par_reduce(0, task_off.size() - 1, worker, 1);
    master.join(worker);
    done += batch_combos;
    Rcpp::checkUserInterrupt();

    auto now = std::chrono::high_resolution_clock::now();
    double ms_since_start = std::chrono::duration<double, std::milli>(now - start_time).count();
    batch_target = std::max(done / std::max(ms_since_start, 1.0) * TARGET_BATCH_MS, 1e5);
    if (max_seconds > 0) {
      // estimating: the first half of the time on the strongest branches, from the start (as the search
      // itself), each batch sized to end by then
      if (ms_since_start >= 500.0 * max_seconds) break;
      batch_target = std::min(batch_target, std::max(done / std::max(ms_since_start, 1.0) * (500.0 * max_seconds - ms_since_start), 1e5));
    }

    if (show_progress) {
      bool is_last_batch = finished || done >= n_combos_d;
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

  // Estimating the whole search: the strongest branches (above) are the slowest -- the threshold is still
  // rising and the least can be skipped -- so the rest is timed on a sample: small branches around
  // combinations drawn evenly from those not yet covered (by rank, a fixed sequence), searched in parallel
  // with the threshold the first half reached. Seconds = first half + the sample's time per combination x the rest
  double est_seconds = NA_REAL;
  if (max_seconds > 0) {
    const double ms1 = std::max(std::chrono::duration<double, std::milli>(std::chrono::high_resolution_clock::now() - start_time).count(), 1e-3);
    if (done >= n_combos_d) est_seconds = ms1 / 1000.0;
    else if (k >= 2) {
      const double rest = n_combos_d - done;
      const double want = std::max(done / ms1 * 500.0 * max_seconds, 1e5);    // combinations the second half has time for
      const int m = 8 * n_threads;
      const double piece = std::min(std::max(want / m, 1e3), 5e6);
      const int y0 = search.hY > 0 ? p - search.hY : p;
      std::vector<int> comb(k);
      uint64_t rng = 0x9E3779B97F4A7C15ull;
      task_items.clear(); task_off.assign(1, 0);
      double sampled = 0;
      for (int s = 0; s < m; ++s) {
        rng = rng * 6364136223846793005ull + 1442695040888963407ull;
        double R = done + (double)(rng >> 11) * (1.0 / 9007199254740992.0) * rest;  // a rank among those left
        int prev = -1;
        for (int d = 0; d < k; ++d) {                       // unrank: the R-th combination in lexicographic order
          int c = prev + 1;
          for (; c < p - (k - d); ++c) { const double cnt = calculate_combinations_double(p - 1 - c, k - 1 - d); if (R < cnt) break; R -= cnt; }
          comb[d] = c; prev = c;
        }
        for (int d = 0; d <= k - 2; ++d) {                  // the branch around it, as the splitter would cut it
          if (d >= 1 && comb[d] >= y0) {
            task_items.insert(task_items.end(), comb.begin(), comb.begin() + d); task_items.push_back(-(y0 + 1));
            sampled += calculate_combinations_double(p - y0, k - d); break;
          }
          const double size = calculate_combinations_double(p - 1 - comb[d], k - d - 1);
          if (size <= piece || d == k - 2) { task_items.insert(task_items.end(), comb.begin(), comb.begin() + d + 1); sampled += size; break; }
        }
        task_off.push_back((int)task_items.size());
      }
      auto t2 = std::chrono::high_resolution_clock::now();
      GramSearchWorker worker(search, task_items, task_off);
      par_reduce(0, task_off.size() - 1, worker, 1);
      const double ms2 = std::chrono::duration<double, std::milli>(std::chrono::high_resolution_clock::now() - t2).count();
      est_seconds = (ms1 + ms2 / std::max(sampled, 1.0) * rest) / 1000.0;
    }
  }

  stamp(timestamps); // 2

  // Best-first order (same tie-breaking as the heap)
  GramSearchWorker::HeapCmp cmp{&master};
  std::vector<GramSearchWorker::HeapEntry> kept = master.heap;
  std::sort(kept.begin(), kept.end(), cmp);

  stamp(timestamps); // 3

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


  stamp(timestamps); // 4

  // Seconds per section (microsecond resolution: whole milliseconds rounded short sections to 0)
  NumericVector timings(timestamps.size() - 1);
  for (size_t i = 0; i < timestamps.size() - 1; i++) {
    auto d = std::chrono::duration_cast<std::chrono::microseconds>(timestamps[i+1] - timestamps[i]);
    timings[i] = d.count() / 1e6;
  }
  timings.attr("names") = CharacterVector::create("search", "sort", "format");

  return List::create(
    Named("combination") = comb_out,
    Named("r") = r_out,
    Named("combo_indices") = idx_out,
    Named("timings_cpp") = timings,
    Named("covered") = done,
    Named("est_seconds") = est_seconds
  );
}

// ============================================================================
// EXACT RE-SCORING OF THE TOP COMBINATIONS
// ============================================================================
// Both search engines work on approximated data (mean-imputed / 8-bit), so
// the top combinations are re-scored exactly from the true data. One thread
// per combination builds its sum scores straight from the data -- never the
// N x keep_top score matrices the R version built (~7 GB at N = 300,000 with
// a binary target's 1,000 sets) -- then its pairwise-complete r and, for
// binary targets, the cutoff search and AUC (previously a single-core R loop
// that dominated large-N runs).

// Sorts doubles (no NaN) via up to 4 passes of 16-bit LSD radix on an
// order-preserving integer mapping: O(N) where std::sort is O(N log N).
// Passes where every key shares the digit are skipped -- integer-valued
// sum scores have all-zero low mantissa bits, so they usually need one
static void radix_sort_doubles(std::vector<double>& v, std::vector<uint64_t>& a,
                               std::vector<uint64_t>& b, std::vector<uint32_t>& count) {
  const size_t n = v.size();
  a.resize(n); b.resize(n); count.resize(65536);
  const uint64_t SIGN = (uint64_t)1 << 63;
  for (size_t i = 0; i < n; ++i) {
    uint64_t u; std::memcpy(&u, &v[i], sizeof(u));
    a[i] = (u & SIGN) ? ~u : (u | SIGN);
  }
  for (int shift = 0; shift < 64; shift += 16) {
    std::fill(count.begin(), count.end(), 0);
    for (size_t i = 0; i < n; ++i) ++count[(a[i] >> shift) & 0xFFFF];
    if (n == 0 || count[(a[0] >> shift) & 0xFFFF] == n) continue;  // all keys share this digit
    uint32_t sum = 0;
    for (int d = 0; d < 65536; ++d) { uint32_t c = count[d]; count[d] = sum; sum += c; }
    for (size_t i = 0; i < n; ++i) b[count[(a[i] >> shift) & 0xFFFF]++] = a[i];
    a.swap(b);
  }
  for (size_t i = 0; i < n; ++i) {
    uint64_t u = (a[i] & SIGN) ? (a[i] & ~SIGN) : ~a[i];
    std::memcpy(&v[i], &u, sizeof(u));
  }
}

// Pearson r over pairwise-complete rows, two-pass like cor()
static double pairwise_pearson(const std::vector<double>& x, const RVector<double>& y) {
  const int n = (int)x.size();
  long double sx = 0, sy = 0; int m = 0;
  for (int i = 0; i < n; ++i) if (!std::isnan(x[i]) && !std::isnan(y[i])) { sx += x[i]; sy += y[i]; ++m; }
  if (m < 2) return NA_REAL;
  const long double mx = sx / m, my = sy / m;
  long double sxy = 0, sxx = 0, syy = 0;
  for (int i = 0; i < n; ++i) if (!std::isnan(x[i]) && !std::isnan(y[i])) {
    const long double dx = x[i] - mx, dy = y[i] - my;
    sxy += dx * dy; sxx += dx * dx; syy += dy * dy;
  }
  if (!(sxx > 0 && syy > 0)) return NA_REAL;
  const long double n1 = m - 1;
  sxx /= n1; syy /= n1; sxy /= n1;
  return std::max(-1.0, std::min(1.0, (double)(sxy / (std::sqrt(sxx) * std::sqrt(syy)))));
}

// AUC (Mann-Whitney U, ties counted half) from each class's sorted scores
static double auc_sorted(const std::vector<double>& pos, const std::vector<double>& neg) {
  long double u = 0;                        // sum over positives of (#neg below + half #neg tied)
  for (size_t ip = 0, in_lo = 0, in_hi = 0; ip < pos.size(); ++ip) {
    while (in_lo < neg.size() && neg[in_lo] < pos[ip]) ++in_lo;
    if (in_hi < in_lo) in_hi = in_lo;
    while (in_hi < neg.size() && neg[in_hi] == pos[ip]) ++in_hi;
    u += in_lo + 0.5 * (in_hi - in_lo);
  }
  return (double)(u / ((double)pos.size() * (double)neg.size()));
}

struct RescoreWorker : public Worker {
  const RMatrix<double> X;     // N x p data (NA = missing)
  const RVector<double> y;     // target
  const RMatrix<int> sets;     // k x n_sets, 0-based column indices
  const RVector<double> fixed_cut; // holdout scoring: each set's training cutoff (empty = search)
  const int n, k;
  const bool na_rm, binary, search_phi, fixed, int_cuts;
  RVector<double> r_out, cut_out, phi_out, youden_out, auc_out;

  RescoreWorker(const NumericMatrix& X, const NumericVector& y, const IntegerMatrix& sets,
                const NumericVector& fixed_cut, bool na_rm, bool binary, bool search_phi, bool int_cuts,
                NumericVector r_out, NumericVector cut_out, NumericVector phi_out,
                NumericVector youden_out, NumericVector auc_out)
    : X(X), y(y), sets(sets), fixed_cut(fixed_cut), n(X.nrow()), k(sets.nrow()), na_rm(na_rm), binary(binary),
      search_phi(search_phi), fixed(fixed_cut.length() > 0), int_cuts(int_cuts), r_out(r_out), cut_out(cut_out), phi_out(phi_out),
      youden_out(youden_out), auc_out(auc_out) {}

  void operator()(std::size_t begin, std::size_t end) {
    std::vector<double> sum(n), score(n), pred(fixed ? n : 0), pos, neg, search, cuts;
    std::vector<int> cnt(n);
    std::vector<uint64_t> ra, rb; std::vector<uint32_t> rc;
    for (std::size_t s = begin; s < end; ++s) {
      // Sum scores: rowMeans(na.rm = TRUE) * k pro-rates over the items
      // present (NA if none); na.rm = FALSE needs every item present
      std::fill(sum.begin(), sum.end(), 0.0); std::fill(cnt.begin(), cnt.end(), 0);
      for (int d = 0; d < k; ++d) {
        const int j = sets(d, s);
        for (int i = 0; i < n; ++i) { const double v = X(i, j); if (!std::isnan(v)) { sum[i] += v; ++cnt[i]; } }
      }
      for (int i = 0; i < n; ++i)
        score[i] = na_rm ? (cnt[i] > 0 ? (sum[i] / cnt[i]) * k : NA_REAL) : (cnt[i] == k ? sum[i] : NA_REAL);

      r_out[s] = pairwise_pearson(score, y);
      if (!binary) continue;

      if (fixed) {
        // Holdout: predict 1 where score >= the TRAINING cutoff; binarised r
        // is the predictions' correlation with the target (as cor() of the
        // 0/1 predictions), Youden's J and AUC from the holdout rows
        const double cut = fixed_cut[s];
        double tp = 0, fn = 0, tn = 0, fp = 0;
        pos.clear(); neg.clear();
        for (int i = 0; i < n; ++i) {
          pred[i] = (std::isnan(score[i]) || std::isnan(cut)) ? NA_REAL : (score[i] >= cut ? 1.0 : 0.0);
          if (std::isnan(score[i]) || std::isnan(y[i])) continue;
          if (y[i] == 1) pos.push_back(score[i]); else if (y[i] == 0) neg.push_back(score[i]);
          if (std::isnan(pred[i])) continue;
          if (y[i] == 1) { if (pred[i] == 1) ++tp; else ++fn; }
          else if (y[i] == 0) { if (pred[i] == 1) ++fp; else ++tn; }
        }
        cut_out[s] = cut;
        phi_out[s] = pairwise_pearson(pred, y);
        youden_out[s] = (tp + fn == 0 || tn + fp == 0) ? NA_REAL : tp / (tp + fn) + tn / (tn + fp) - 1;
        if (pos.empty() || neg.empty()) { auc_out[s] = NA_REAL; continue; }
        radix_sort_doubles(pos, ra, rb, rc);
        radix_sort_doubles(neg, ra, rb, rc);
        auc_out[s] = auc_sorted(pos, neg);
        continue;
      }

      // Binary target: cutoff search on THIS combination's own integer grid
      // floor(min)..ceiling(max) (so no trivial out-of-range cutoff can win a
      // tie; score >= cutoff predicts 1), Youden's J / phi per cutoff, and
      // AUC (Mann-Whitney U, ties counted half). Equal Youden's J / |phi| at
      // different cutoffs is common, so near-ties (within 1e-9) count as ties
      // and the smallest cutoff wins -- deterministic across computation paths.
      // The counts change only where the cutoff passes a score, so only the
      // first whole number past each score (and floor(min)) is tried: every
      // other cutoff repeats the one before it, and the smallest wins ties
      // anyway. That is the same answer, in time and memory proportional to
      // the rows rather than to the scores' range (items in the millions
      // made that range -- and the old per-cutoff arrays -- enormous).
      // Without the whole-number grid (int_cuts false: standardised scores),
      // every observed score is a cutoff -- the best of all thresholds
      pos.clear(); neg.clear();
      double lo = R_PosInf, hi = R_NegInf;
      for (int i = 0; i < n; ++i) {
        if (std::isnan(score[i]) || std::isnan(y[i])) continue;
        lo = std::min(lo, score[i]); hi = std::max(hi, score[i]);
        if (y[i] == 1) pos.push_back(score[i]); else if (y[i] == 0) neg.push_back(score[i]);
      }
      cut_out[s] = phi_out[s] = youden_out[s] = auc_out[s] = NA_REAL;
      if (!(lo <= hi)) continue;
      const double c_lo = std::floor(lo), c_hi = std::ceil(hi);
      const double n_pos = (double)pos.size(), n_neg = (double)neg.size();
      if (n_pos == 0 || n_neg == 0) { cut_out[s] = (c_lo + c_hi) / 2; phi_out[s] = 0; continue; }

      radix_sort_doubles(pos, ra, rb, rc);
      radix_sort_doubles(neg, ra, rb, rc);

      auc_out[s] = auc_sorted(pos, neg);

      cuts.clear(); if (int_cuts) cuts.push_back(c_lo);
      for (size_t a = 0, b = 0; a < pos.size() || b < neg.size(); ) {   // the scores in order (both lists are sorted)
        const double v = (b >= neg.size() || (a < pos.size() && pos[a] <= neg[b])) ? pos[a++] : neg[b++];
        const double c = int_cuts ? std::floor(v) + 1 : v;
        if ((!int_cuts || c <= c_hi) && (cuts.empty() || c > cuts.back())) cuts.push_back(c);
      }
      const int n_cut = (int)cuts.size();
      search.assign(n_cut, NA_REAL);
      std::vector<double> youden_c(n_cut), phi_c(n_cut);
      double best = R_NegInf;
      for (int c = 0, ip = 0, in = 0; c < n_cut; ++c) {
        const double cut = cuts[c];
        while (ip < (int)pos.size() && pos[ip] < cut) ++ip;
        while (in < (int)neg.size() && neg[in] < cut) ++in;
        const double tp = n_pos - ip, fp = n_neg - in, tn = n_neg - fp, fn = n_pos - tp;
        youden_c[c] = tp / n_pos + tn / n_neg - 1;
        const double denom = std::sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn));
        phi_c[c] = denom == 0 ? NA_REAL : (tp * tn - fp * fn) / denom;
        search[c] = search_phi ? std::fabs(phi_c[c]) : youden_c[c];
        if (!std::isnan(search[c]) && search[c] > best) best = search[c];
      }
      phi_out[s] = 0;                           // (as in R when no cutoff has a defined phi)
      for (int c = 0; c < n_cut; ++c) {        // first cutoff within 1e-9 of the best
        if (!std::isnan(search[c]) && search[c] >= best - 1e-9) {
          cut_out[s] = cuts[c];
          phi_out[s] = std::isnan(phi_c[c]) ? 0 : phi_c[c];
          youden_out[s] = youden_c[c];
          break;
        }
      }
    }
  }
};

// [[Rcpp::export]]
List rescore_sets_cpp(NumericMatrix data, NumericVector target, IntegerMatrix sets,
                      bool na_rm, bool binary, bool search_phi,
                      NumericVector fixed_cutoffs = NumericVector(0), bool integer_cutoffs = true) {
  const int n_sets = sets.ncol();
  if (fixed_cutoffs.length() > 0 && fixed_cutoffs.length() != n_sets) Rcpp::stop("fixed_cutoffs must have one cutoff per set.");
  NumericVector r(n_sets), cutoff(n_sets), binarised_r(n_sets), youden_j(n_sets), auc(n_sets);
  RescoreWorker worker(data, target, sets, fixed_cutoffs, na_rm, binary, search_phi, integer_cutoffs, r, cutoff, binarised_r, youden_j, auc);
  par_for(0, n_sets, worker, 1);
  return List::create(Named("r") = r, Named("cutoff") = cutoff, Named("binarised_r") = binarised_r,
                      Named("youden_j") = youden_j, Named("auc") = auc);
}
