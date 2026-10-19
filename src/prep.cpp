// [[Rcpp::depends(RcppParallel)]]

// ============================================================================
//  DATA PREPARATION
// ============================================================================
// The passes over the raw data that run before any search: column checks, the
// ranges and means they give for free, reverse-keying, and the cross-products
// (Gram matrix) of the mean-imputed data. At 300 items x 300,000 rows these
// took ~8 s as R code (apply/sweep/rep), mostly in full-size copies; here each
// is one pass over contiguous columns, with no full-size temporaries.

#include <Rcpp.h>
#include <RcppParallel.h>
#include <algorithm>
#include <cmath>
#include <vector>
#include "parallel_guard.h"   // parallel loops that fall back to one thread in a fork()'d child

// The Rcpp and RcppParallel names this file uses unqualified; everything else is written in full (Rcpp::stop, ...)
using Rcpp::NumericVector;
using Rcpp::NumericMatrix;
using Rcpp::IntegerVector;
using Rcpp::List;
using Rcpp::Named;
using RcppParallel::Worker;
using RcppParallel::RVector;
using RcppParallel::RMatrix;


namespace {

// Rows to visit: all of them (rows empty) or the given 1-based rows
struct RowSel {
  const int* r; const int m;
  RowSel(const IntegerVector& rows, int n) : r(rows.length() ? &rows[0] : nullptr), m(rows.length() ? (int)rows.length() : n) {}
  inline int at(int q) const { return r ? r[q] - 1 : q; }
};

// 1-based indices must be in 1..n (an NA or out-of-range one would read outside the data)
void check_indices(const IntegerVector& idx, int n, const char* what) {
  for (R_xlen_t q = 0; q < idx.length(); ++q)
    if (idx[q] == NA_INTEGER || idx[q] < 1 || idx[q] > n) Rcpp::stop("%s index out of range.", what);
}

// Per-column non-missing count, min, max, sum and whether any value is infinite
struct ScanWorker : public Worker {
  const RMatrix<double> X; const RowSel rows; const int* cols;
  RVector<double> mn, mx, sm, cnt; RVector<int> inf;
  ScanWorker(const NumericMatrix& X, const RowSel& rows, const int* cols, NumericVector& mn, NumericVector& mx, NumericVector& sm, NumericVector& cnt, IntegerVector& inf)
    : X(X), rows(rows), cols(cols), mn(mn), mx(mx), sm(sm), cnt(cnt), inf(inf) {}
  void operator()(std::size_t b, std::size_t e) {
    for (std::size_t q = b; q < e; ++q) {
      const double* c = &X(0, cols ? cols[q] - 1 : (int)q);
      double lo = INFINITY, hi = -INFINITY, s = 0; int k = 0, bad = 0;
      for (int i = 0; i < rows.m; ++i) {
        const double v = c[rows.at(i)];
        if (std::isnan(v)) continue;
        if (std::isinf(v)) { bad = 1; continue; }
        if (v < lo) lo = v;
        if (v > hi) hi = v;
        s += v; ++k;
      }
      mn[q] = lo; mx[q] = hi; sm[q] = s; cnt[q] = k; inf[q] = bad;
    }
  }
};

// Copies one tile of mean-imputed data (m x p, column-major), centred on each column's mean, into buf
struct FillWorker : public Worker {
  const RMatrix<double> X; const int* rows; const int m; const double* mu; double* buf;
  FillWorker(const NumericMatrix& X, const int* rows, int m, const double* mu, double* buf) : X(X), rows(rows), m(m), mu(mu), buf(buf) {}
  void operator()(std::size_t b, std::size_t e) {
    for (std::size_t j = b; j < e; ++j) {
      const double* c = &X(0, (int)j); double* o = buf + j * (size_t)m; const double f = mu[j];
      for (int r = 0; r < m; ++r) { const double v = c[rows[r]]; o[r] = std::isnan(v) ? 0.0 : v - f; }
    }
  }
};

// One tile's share of the cross-products: for each column j (one task each), the dot products of the
// tile's column j with columns 0..j (the upper triangle of G, column-major), with the centred target
// (td) and with ones (cs). Each dot product keeps eight running sums -- independent chains the processor
// can overlap and the compiler can vectorise (in place of BLAS, which tied the package to a Fortran runtime)
struct TileGramWorker : public Worker {
  const double* buf; const int m, p; const double* tv; double* G; double* td; double* cs;
  TileGramWorker(const double* buf, int m, int p, const double* tv, double* G, double* td, double* cs)
    : buf(buf), m(m), p(p), tv(tv), G(G), td(td), cs(cs) {}
  static inline double dot(const double* a, const double* b, int m) {
    double s0 = 0, s1 = 0, s2 = 0, s3 = 0, s4 = 0, s5 = 0, s6 = 0, s7 = 0; int r = 0;
    for (; r + 8 <= m; r += 8) {
      s0 += a[r] * b[r]; s1 += a[r + 1] * b[r + 1]; s2 += a[r + 2] * b[r + 2]; s3 += a[r + 3] * b[r + 3];
      s4 += a[r + 4] * b[r + 4]; s5 += a[r + 5] * b[r + 5]; s6 += a[r + 6] * b[r + 6]; s7 += a[r + 7] * b[r + 7];
    }
    for (; r < m; ++r) s0 += a[r] * b[r];
    return ((s0 + s1) + (s2 + s3)) + ((s4 + s5) + (s6 + s7));
  }
  void operator()(std::size_t b, std::size_t e) {
    for (std::size_t jj = b; jj < e; ++jj) {
      const int j = (int)jj; const double* cj = buf + (size_t)j * m; double* g = G + (size_t)j * p;
      for (int i = 0; i <= j; ++i) g[i] += dot(buf + (size_t)i * m, cj, m);
      if (tv) td[j] += dot(cj, tv, m);
      double s = 0; for (int r = 0; r < m; ++r) s += cj[r]; cs[j] += s;
    }
  }
};

// out = X with the chosen columns reversed (bound - x) and every other column copied
struct FlipWorker : public Worker {
  const RMatrix<double> X; RMatrix<double> out; const std::vector<double>& bound; const std::vector<char>& flip;
  FlipWorker(const NumericMatrix& X, NumericMatrix& out, const std::vector<double>& bound, const std::vector<char>& flip) : X(X), out(out), bound(bound), flip(flip) {}
  void operator()(std::size_t b, std::size_t e) {
    const int n = X.nrow();
    for (std::size_t j = b; j < e; ++j) {
      const double* s = &X(0, (int)j); double* d = &out(0, (int)j);
      if (flip[j]) { const double bd = bound[j]; for (int i = 0; i < n; ++i) d[i] = bd - s[i]; }
      else std::copy(s, s + n, d);
    }
  }
};

} // namespace

// One pass over the columns (all of them, or the 1-based `cols`) and the rows (all, or the 1-based `rows`):
// non-missing count, min, max, sum and whether any value is infinite. Missing values (NA, NaN) are skipped.
// A column is usable when n > 0, nothing is infinite and max > min (equivalent to sd > 0)
// [[Rcpp::export]]
List column_scan_cpp(NumericMatrix X, IntegerVector rows = IntegerVector(0), IntegerVector cols = IntegerVector(0)) {
  check_indices(rows, X.nrow(), "Row"); check_indices(cols, X.ncol(), "Column");
  const int pc = cols.length() ? cols.length() : X.ncol();
  NumericVector mn(pc), mx(pc), sm(pc), cnt(pc); IntegerVector inf(pc);
  ScanWorker w(X, RowSel(rows, X.nrow()), cols.length() ? &cols[0] : nullptr, mn, mx, sm, cnt, inf);
  par_for(0, pc, w, 1);
  return List::create(Named("min") = mn, Named("max") = mx, Named("sum") = sm, Named("n") = cnt, Named("has_inf") = inf);
}

// The cross-products of mean-imputed data, tile by tile with no full-size copy: each tile of rows is
// filled (missing -> `mu`, the column's mean) and its cross-products added in parallel (TileGramWorker). Only rows with a target value
// (and, if given, among the 1-based `rows`) count; with no target (length 0) every such row counts.
// Returns what the Gram engine takes: gram = t(Y) %*% Y, col_sums, col_target_dots, sum_target,
// sum_target_sq and n_valid, for Y the data minus `mu` and the target minus its mean. Every use of them
// (covariances: gram - outer(col_sums, col_sums) / n_valid, ...) is unchanged by such shifts, but
// uncentred sums lose the covariances to rounding when the values sit far from zero (items around 10^7
// kept two digits; around 10^8, none)
// [[Rcpp::export]]
List fused_gram_cpp(NumericMatrix X, NumericVector targ, NumericVector mu, IntegerVector rows = IntegerVector(0)) {
  const int n = X.nrow(), p = X.ncol(); const bool has_t = targ.length() > 0;
  if (has_t && targ.length() != n) Rcpp::stop("The target must have one value per row.");
  if (mu.length() != p) Rcpp::stop("`mu` needs one mean per column.");
  check_indices(rows, n, "Row");
  const RowSel sel(rows, n);
  std::vector<int> valid; valid.reserve(sel.m);
  for (int q = 0; q < sel.m; ++q) { const int i = sel.at(q); if (!has_t || !std::isnan(targ[i])) valid.push_back(i); }
  const int nv = valid.size();
  long double tsum = 0; if (has_t) for (int i : valid) tsum += targ[i];
  const double tbar = nv > 0 ? (double)(tsum / nv) : 0.0;   // the target is centred too
  const int tile = (int)std::max(256.0, std::min(4096.0, 4.0e6 / std::max(p, 1)));
  std::vector<double> G((size_t)p * p, 0.0), buf((size_t)std::min(tile, std::max(nv, 1)) * p), tv(tile), cdv(p, 0.0), csv(p, 0.0);
  NumericVector cs(p), cd(p); double sum_t = 0, sum_t2 = 0;
  for (int r0 = 0; r0 < nv; r0 += tile) {
    const int m = std::min(tile, nv - r0);
    FillWorker w(X, valid.data() + r0, m, &mu[0], buf.data()); par_for(0, p, w, 1);
    if (has_t) for (int r = 0; r < m; ++r) { tv[r] = targ[valid[r0 + r]] - tbar; sum_t += tv[r]; sum_t2 += tv[r] * tv[r]; }
    TileGramWorker g(buf.data(), m, p, has_t ? tv.data() : nullptr, G.data(), cdv.data(), csv.data()); par_for(0, p, g, 1);
    Rcpp::checkUserInterrupt();
  }
  for (int j = 0; j < p; ++j) { cs[j] = csv[j]; cd[j] = cdv[j]; }
  NumericMatrix gram(p, p);
  for (int j = 0; j < p; ++j) for (int i = 0; i <= j; ++i) { gram(i, j) = G[(size_t)j * p + i]; gram(j, i) = gram(i, j); }
  if (X.hasAttribute("dimnames")) {
    List dn = X.attr("dimnames");
    if (dn.length() == 2 && !Rf_isNull(dn[1])) { gram.attr("dimnames") = List::create(dn[1], dn[1]); cs.attr("names") = dn[1]; cd.attr("names") = dn[1]; }
  }
  return List::create(Named("gram") = gram, Named("col_sums") = cs, Named("col_target_dots") = cd,
                      Named("sum_target") = sum_t, Named("sum_target_sq") = sum_t2, Named("n_valid") = nv);
}

// A copy of X with the 1-based columns `cols` reversed as bound - x (x is left alone: nothing is changed in place)
// [[Rcpp::export]]
NumericMatrix flip_columns_cpp(NumericMatrix X, IntegerVector cols, NumericVector bounds) {
  const int n = X.nrow(), p = X.ncol();
  if (cols.length() != bounds.length()) Rcpp::stop("`cols` and `bounds` must have the same length.");
  std::vector<char> flip(p, 0); std::vector<double> bound(p, 0.0);
  for (int q = 0; q < cols.length(); ++q) { const int j = cols[q] - 1; if (j < 0 || j >= p) Rcpp::stop("Column index out of range."); flip[j] = 1; bound[j] = bounds[q]; }
  NumericMatrix out(Rcpp::no_init(n, p));
  FlipWorker w(X, out, bound, flip); par_for(0, p, w, 1);
  if (X.hasAttribute("dimnames")) out.attr("dimnames") = X.attr("dimnames");
  return out;
}
