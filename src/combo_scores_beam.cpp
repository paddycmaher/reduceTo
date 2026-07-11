// [[Rcpp::depends(RcppParallel)]]

#include <Rcpp.h>
#include <RcppParallel.h>
#include <algorithm>
#include <vector>
#include <cmath>
#include <chrono>
#include <iomanip>

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
      int batch_size = std::max(1000, n_combos / 500); 
      for (int start = 0; start < n_combos; start += batch_size) {
        int end = std::min(start + batch_size, n_combos);
        parallelFor(start, end, worker);
        Rcpp::checkUserInterrupt();
        
        // Progress Bar Update
        auto now = std::chrono::high_resolution_clock::now();
        double seconds = std::chrono::duration_cast<std::chrono::milliseconds>(now - start_time).count() / 1000.0;
        if (seconds > 0.05) {
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
      int batch_size = std::max(1000, n_combos / 500); 
      for (int start = 0; start < n_combos; start += batch_size) {
        int end = std::min(start + batch_size, n_combos);
        parallelFor(start, end, worker);
        Rcpp::checkUserInterrupt();
        
        // Progress Bar Update (Code duplication unavoidable without complex wrapper)
        auto now = std::chrono::high_resolution_clock::now();
        double seconds = std::chrono::duration_cast<std::chrono::milliseconds>(now - start_time).count() / 1000.0;
        if (seconds > 0.05) {
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
  
  // Sorting & Output (Same as before)
  std::vector<int> idx(n_combos);
  std::iota(idx.begin(), idx.end(), 0);
  int n_top = std::min(keep_top, n_combos);
  
  std::partial_sort(idx.begin(), idx.begin() + n_top, idx.end(),
                    [&results](int i1, int i2) {
                      bool na1 = std::isnan(results[i1]);
                      bool na2 = std::isnan(results[i2]);
                      if (na1 && na2) return false;
                      if (na1) return false;
                      if (na2) return true;
                      return std::abs(results[i1]) > std::abs(results[i2]);
                    });
  
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
// which is exactly what the moment decomposition below computes in O(k^2)
// instead of O(n_rows * k) per combination.

struct GramComboWorker : public Worker {
  const RMatrix<double> gram;         // pool_size x pool_size, Sum(x_i * x_j)
  const RVector<double> col_sums;     // pool_size, Sum(x_i)
  const RVector<double> col_target_dots; // pool_size, Sum(x_i * target)
  const double sum_target;
  const double sum_target_sq;
  const double n_valid;
  const int n_items;
  const int num_choose_from;

  std::vector<float>& results;

  GramComboWorker(const NumericMatrix& gram,
                   const NumericVector& col_sums,
                   const NumericVector& col_target_dots,
                   double sum_target,
                   double sum_target_sq,
                   double n_valid,
                   int n_items,
                   int num_choose_from,
                   std::vector<float>& results)
    : gram(gram), col_sums(col_sums), col_target_dots(col_target_dots),
      sum_target(sum_target), sum_target_sq(sum_target_sq), n_valid(n_valid),
      n_items(n_items), num_choose_from(num_choose_from), results(results) {}

  void operator()(std::size_t begin, std::size_t end) {
    std::vector<int> current_combo(n_items);
    get_combination_nth(begin, num_choose_from, n_items, current_combo);

    for (std::size_t combo = begin; combo < end; ++combo) {
      if (combo > begin) next_combination(current_combo, num_choose_from, n_items);

      double sum_scores = 0.0, sum_scores_sq = 0.0, sum_prod = 0.0;

      for (int i = 0; i < n_items; ++i) {
        int ci = current_combo[i];
        sum_scores += col_sums[ci];
        sum_prod += col_target_dots[ci];
        for (int j = 0; j < n_items; ++j) {
          sum_scores_sq += gram(ci, current_combo[j]);
        }
      }

      float r = NA_REAL;
      if (n_valid > 1) {
        double ms = sum_scores / n_valid;
        double mt = sum_target / n_valid;
        double vs = (sum_scores_sq / n_valid) - ms * ms;
        double vt = (sum_target_sq / n_valid) - mt * mt;
        double cv = (sum_prod / n_valid) - ms * mt;
        if (vs > 0 && vt > 0) r = (float)(cv / std::sqrt(vs * vt));
      }
      results[combo] = r;
    }
  }
};

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
  double n_combos_d = calculate_combinations_double(num_choose_from, n_items);
  if (n_combos_d > 2000000000) stop("Too many combinations (>2B).");
  int n_combos = (int)n_combos_d;

  std::vector<std::chrono::high_resolution_clock::time_point> timestamps;
  TIMESTAMP; // 1

  std::vector<float> results(n_combos);

  auto start_time = std::chrono::high_resolution_clock::now();

  GramComboWorker worker(gram, col_sums, col_target_dots, sum_target, sum_target_sq,
                          n_valid, n_items, num_choose_from, results);

  if (!show_progress) {
    parallelFor(0, n_combos, worker);
  } else {
    int batch_size = std::max(1000, n_combos / 500);
    for (int start = 0; start < n_combos; start += batch_size) {
      int end = std::min(start + batch_size, n_combos);
      parallelFor(start, end, worker);
      Rcpp::checkUserInterrupt();

      auto now = std::chrono::high_resolution_clock::now();
      double seconds = std::chrono::duration_cast<std::chrono::milliseconds>(now - start_time).count() / 1000.0;
      if (seconds > 0.05) {
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

  if (show_progress) Rcpp::Rcout << "\r" << std::string(80, ' ') << "\r";

  TIMESTAMP; // 2

  std::vector<int> idx(n_combos);
  std::iota(idx.begin(), idx.end(), 0);
  int n_top = std::min(keep_top, n_combos);

  std::partial_sort(idx.begin(), idx.begin() + n_top, idx.end(),
                    [&results](int i1, int i2) {
                      bool na1 = std::isnan(results[i1]);
                      bool na2 = std::isnan(results[i2]);
                      if (na1 && na2) return false;
                      if (na1) return false;
                      if (na2) return true;
                      return std::abs(results[i1]) > std::abs(results[i2]);
                    });

  TIMESTAMP; // 3

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
    Named("timings_cpp") = timings
  );
}

// [[Rcpp::export]]
RawVector compress_matrix_cpp(IntegerMatrix data) {
  int n_rows = data.nrow();
  int n_cols = data.ncol();
  size_t total_cells = (size_t)n_rows * n_cols;
  RawVector packed((R_xlen_t)total_cells);
  pack_bytes(&data[0], reinterpret_cast<uint8_t*>(RAW(packed)), total_cells);
  return packed;
}

// ============================================================================
//  BEAM SEARCH WORKER (Evaluates specific sets provided by R)
// ============================================================================

template <bool NA_RM>
struct BeamWorker : public Worker {
  const uint8_t* data_ptr;
  const int n_rows;
  const RMatrix<int> combinations; // Thread-safe read matrix from R
  const RVector<double> target;
  const int k_items;
  
  std::vector<float>& results;
  
  BeamWorker(const uint8_t* data_ptr,
             const IntegerMatrix& combos,
             const NumericVector& target,
             std::vector<float>& results)
    : data_ptr(data_ptr),
      n_rows(target.size()),
      combinations(combos),
      target(target),
      k_items(combos.ncol()),
      results(results) {}
  
  void operator()(std::size_t begin, std::size_t end) {
    std::vector<const uint8_t*> col_ptrs(k_items);
    
    for (std::size_t combo = begin; combo < end; ++combo) {
      
      // Update pointers for this specific combination
      // NOTE: R passes 1-based indices. We subtract 1 for C++ 0-based array access.
      for(int i = 0; i < k_items; ++i) {
        int col_idx = combinations(combo, i) - 1; 
        col_ptrs[i] = data_ptr + (col_idx * n_rows);
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
        
        // --- INNER LOOP ---
        for (int i = 0; i < k_items; ++i) {
          uint8_t val = col_ptrs[i][row]; 
          
          if (val == NA_BYTE) {
            if (NA_RM) {
              continue; 
            } else {
              possible = false; 
              break; 
            }
          }
          
          row_sum += val;
          valid_items++;
        }
        
        if (!possible) continue;
        
        // Score Calculation
        float score;
        if (NA_RM) {
          if (valid_items > 0) score = ((float)row_sum / valid_items) * k_items;
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

// [[Rcpp::export]]
NumericVector evaluate_beam_cpp(
    RawVector packed_data,
    IntegerMatrix combinations,
    NumericVector target,
    bool na_rm
) {
  // packed_data is pre-compressed once by the caller (compress_matrix_cpp)
  // and reused across beam-search iterations, since the underlying item
  // data doesn't change between calls -- avoids re-deriving the same
  // byte buffer on every iteration of the beam search.
  const uint8_t* data_ptr = reinterpret_cast<const uint8_t*>(RAW(packed_data));
  int n_combos = combinations.nrow();

  std::vector<float> results(n_combos);

  // --- DISPATCHER ---
  if (na_rm) {
    BeamWorker<true> worker(data_ptr, combinations, target, results);
    parallelFor(0, n_combos, worker);
  } else {
    BeamWorker<false> worker(data_ptr, combinations, target, results);
    parallelFor(0, n_combos, worker);
  }
  
  // Convert back to Rcpp format
  NumericVector out(n_combos);
  for(int i = 0; i < n_combos; ++i) {
    out[i] = results[i];
  }
  
  return out;
}