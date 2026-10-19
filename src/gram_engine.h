// Shared by the exhaustive search (combo_scores.cpp) and the knockout search (knockout.cpp): the Gram-matrix
// tree walk that finds the top combinations of a block of items (O(1) per combination, skipping branches
// that provably can't reach the top), its branch splitter, and the progress-bar settings and formatting
// helpers both use.
#ifndef REDUCETO_GRAM_ENGINE_H
#define REDUCETO_GRAM_ENGINE_H

#include <Rcpp.h>
#include <RcppParallel.h>
#include <algorithm>
#include <vector>
#include <cmath>
#include <string>
#include <atomic>
#include <utility>
#include <cstdint>
#if defined(__aarch64__) && defined(__ARM_NEON) && !defined(RT_NO_NEON)
#include <arm_neon.h>
#define RT_NEON 1
#endif

// Progress bars: width, and the characters that draw them
constexpr int BAR_WIDTH = 30;
constexpr const char* BAR_START = "}";   // Start line
constexpr const char* BAR_FILL  = "~";   // Completed blocks
constexpr const char* BAR_EMPTY = " ";   // Remaining blocks
constexpr const char* BAR_END   = "{";   // Finish line
// Minimum time between progress-bar redraws. ~30fps is well past the point
// of looking perceptibly "instant/smooth" to a human eye, but each redraw
// costs a real, blocking std::flush() -- printing far above this rate (the
// old code redrew on every batch boundary, easily hundreds of times/sec on
// fast runs) burns real compute on I/O nobody can actually see.
constexpr double MIN_UPDATE_INTERVAL_MS = 33.0;
// Target wall time per dispatch batch. Every batch boundary stalls all threads
// (the batch can't finish until its slowest piece does) for ~1.5-2.5ms, so
// batches are sized from measured throughput rather than a fixed combination
// count: ~100ms gives a steady ~10fps progress/interrupt check on any run size
// for ~2% overhead (fixed 2e7-combination batches cost ~15-20% on the Gram
// engine, while 1/60-of-total batches refreshed only every few seconds on huge runs).
constexpr double TARGET_BATCH_MS = 100.0;

// Defined here (inline) rather than in combo_scores.cpp so every source file compiles on its own,
// as it must under Rcpp::sourceCpp(), which builds each file as a separate library
inline double calculate_combinations_double(int n, int k) {
  if (k < 0 || k > n) return 0;
  if (k == 0 || k == n) return 1;
  if (k > n - k) k = n - k;
  double result = 1;
  for (int i = 0; i < k; i++) result = result * (n - i) / (i + 1);
  return result;
}

// Helpers for the progress display
inline std::string format_num(long long num) {
  std::string s = std::to_string(num);
  int n = s.length() - 3;
  while (n > 0) { s.insert(n, ","); n -= 3; }
  return s;
}

inline std::string format_time(double seconds) {
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
//  GRAM-MATRIX SEARCH (Fast path: scores combinations from precomputed
//  column moments instead of re-reading raw rows per combination)
// ============================================================================
// Only valid when the data behind the moments is complete (no missingness)
// over the rows used to build them -- the caller is responsible for imputing
// or excluding rows first. Reproduces the same statistic ComboWorker computes
// on complete data: with no missing values, ComboWorker's per-row score
// degenerates to the plain sum of the k selected columns.
//
// Every combination's r depends on just two totals: T, the sum of its
// items' covariances with the target, and Q, the sum of every pairwise
// covariance among its items (own variances included) -- r = T / sqrt(Q *
// var_target). Rebuilding both from scratch for every combination is a k x k
// double loop (225 lookups at k = 15), even though neighbouring combinations
// share almost all their items.
//
// So the combinations are walked as a tree instead: ABCD, ABCE and ABCF all
// hang off the same ABC branch. The worker keeps running T/Q subtotals for
// the branch it's holding, plus u_j = C_jj + 2 * (how much every remaining
// item j covaries with the branch as a whole) -- so finishing a combination
// (e.g. adding D to ABC) takes three lookups whatever k is:
//   T = T_ABC + t_D,   Q = Q_ABC + u_D
// The last two levels are finished together: Q for {branch, j1, j2} is
// Q + u_j1 + u_j2 + 2 C(j1, j2), so the second-to-last level needs no update.
//
// Items that belong to every combination (the knockout search's kept items) are carried
// in the starting totals T0/Q0, with their cross-products folded into each
// candidate's diagonal entry, so they cost nothing per combination.
//
// The top-K is kept on the fly in a small per-thread min-heap (a candidate
// only touches it when it beats the worst kept one) rather than storing
// every combination's r first -- so memory doesn't grow with the number
// of combinations and there's no 2-billion combination cap on this path.
// Exact ties are broken towards the lexicographically smaller combination, so
// the kept set never depends on how threads happened to split the work.
//
// BRANCH AND BOUND. Most branches can't reach the top K at all, and that can be
// shown before walking them. A branch of length d (totals T, Q) with m items still
// to add, A, from the remaining ones has, for every completion,
//   T_A = T + sum_A t_j,   Q_A = Q + V,   V = sum_A u_j + 2 * sum_{pairs in A} C,
// and is in the top K only if T_A^2 >= thr * Q_A (thr: the K-th best key so far),
// i.e. |T_A| >= sqrt(thr * (Q + V)). sqrt is concave, so on any interval [Vlo, Vhi]
// holding every completion's V it lies above its chord a + b V (b >= 0). Lower
// bounds on the pairs make V's lower bound a sum over A's items, so
//   |T + sum_A t_j| < a + b * sum_A (u_j + pair bound_j)   for every A
// holds -- and the whole branch is dead -- if it holds for the m items with the
// largest t_j - b (u_j + pair bound_j): a sort-free top-m sum. Pairs are bounded
// by each item's smallest covariances (LG, LR below). Three tests use this:
//  - per child, O(1) from suffix top-sums (child-to-rest pairs via LR), run inside
//    the one backward pass that builds those sums, which lists the children passing;
//  - per surviving child, an O(n) test that builds the child's u exactly on the
//    way (only pairs among the rest are bounded);
//  - per row of the last two levels (one pair left, bounded by LR), after a check
//    of all the rows at once (most branches there have no row left).
// Everything a test removes is provably below the K-th best, so the results are
// the same as walking every combination. Items are walked strongest first (by
// their own key), so the threshold rises early; workers share it across threads.
// Tests only run where they pay: not in searches whose last two levels hold
// fewer than ~4 combinations per branch (~8 in the knockout's searches of close
// calls, which prune less), not under branches too small to repay them (every
// branch below is smaller still), and not at a depth where they have stopped
// removing branches (probed now and then).
//
// PLAIN WALKS (no tests) are written for bushy trees, e.g. 15 of 32, where a branch
// of the last two levels holds only a few combinations and the bookkeeping per
// branch, not the scoring, is most of the cost. The branch stack is walked in one
// loop, without recursion. And when the search is large, the SPLIT WALK takes over:
// the last 16 items form block Y, and every subset of Y, of every size, is listed
// once per search with its own totals. The walk enumerates subsets A of the other
// items only; each combination is then A plus one subset B of Y. With B split into
// a high part h (Y's last 6 items) and a low part l (its first 10), and th the
// threshold,
//   T^2 - th Q = G(h) + 2 Th t(l) + E(l) - th QB(h, l),
// where G depends only on A and h, E only on A and l (both set up once per A, E
// kept up to date item by item as A grows), and QB(h, l), B's own Q, is in the
// list: two multiply-adds and a maximum per combination. The lists with the same
// low part sizes are scanned four at a time, sharing their loads.
//
// SCREENS IN SINGLE PRECISION. The split walk's screen, and the plain walk's check
// of a whole branch of the last two levels at once (does any pair reach the
// threshold?), run in single precision, four numbers per instruction. Everything
// within a proven rounding allowance of the threshold passes; whatever passes is
// checked again in double and, if it gets that far, re-scored exactly (below), so
// the results don't change. On ARM processors (Apple silicon) these screens and the
// bound tests' running top lists use the processor's vector instructions directly;
// elsewhere the same work is written as plain loops for the compiler to vectorise.
//
// Re-scoring: the walk adds things up in its own order, so every combination
// offered to the heap (only those near or above the K-th best) is re-scored in
// the caller's order, and the screening threshold is lowered by a relative 1e-12
// so a combination tied with the K-th up to rounding is still offered. Keys, r
// and the order of ties therefore don't depend on the walk order or the threads.

#if defined(__GNUC__) || defined(__clang__)
#define RT_INLINE inline __attribute__((always_inline))
#else
#define RT_INLINE inline
#endif

// Bit counts: one instruction with GCC and clang (the compilers R builds packages with), plain loops elsewhere
#if defined(__GNUC__) || defined(__clang__)
RT_INLINE int rt_popcount(uint32_t x) { return __builtin_popcount(x); }
RT_INLINE int rt_ctz(uint32_t x) { return __builtin_ctz(x); }           // x != 0
#else
inline int rt_popcount(uint32_t x) { int c = 0; for (; x; x &= x - 1) ++c; return c; }
inline int rt_ctz(uint32_t x) { int c = 0; while (!(x & 1u)) { x >>= 1; ++c; } return c; }
#endif

constexpr int LEAF_BLOCK = 8;          // pairs screened per block in the last level
// Bound tests run only when the last two levels hold at least this many combinations per branch,
// relative to k (k - 1) (see GramSearch); below it, plain walks are faster. Exhaustive searches prune
// well enough from 4; the knockout's searches of close calls only from 8
constexpr double SCREEN_RATIO_EXHAUSTIVE = 4, SCREEN_RATIO_POOLS = 8;
constexpr double SPLIT_MIN_COMBOS = 5e5;   // the split walk's tables pay for themselves from this size
constexpr int SPLIT_HY = 16;               // block Y of the split walk: the last 16 items (low 10, high 6)
constexpr int SPLIT_HL = 10;
constexpr int SPLIT_MIN_P = 20;
constexpr uint32_t SPLIT_TABLE_MIN = 100;  // a list of B shorter than this sums each cross term directly
constexpr int MAX_BOUND_ITEMS = 24;    // bound tests need the m smallest / largest of a few values: done for m <= this
constexpr double GATE_NODE = 32;       // test a branch's children only if the branch holds >= this many combinations
constexpr int GATE_PAIRS = 14;         // test the last two levels' rows only with >= this many items left

constexpr int TWO_LISTS_FROM = 16;     // from this many items on, keep two running top lists (shorter dependency chains)

// Everything one search shares across its workers: the moments in search order,
// the bound tables and the threshold shared by threads
struct GramSearch {
  int p, k, K; double ssy, T0, Q0;
  std::vector<int> perm, pinv;         // perm[q] = caller's index of the q-th item in search order; pinv its inverse
  std::vector<double> tc, dc;          // t and C(j, j) in the caller's order (for re-scoring)
  std::vector<double> C, u0, t;        // in search order; u0[j] = C(j, j) (+ fixed items' cross-products)
  std::vector<double> LG, HG, LR;      // [r * p + j]: sum of the r smallest / largest C(j, l), l != j; of the r smallest C(j, l), l > j
  std::vector<double> TMIN, TMAX;      // [m * (p + 1) + s]: sum of the m smallest / largest t over [s, p) (+-inf if fewer)
  std::vector<double> binom;           // [n * (k + 1) + m] = C(n, m)
  // Split walk (plain searches): the last hY items form block Y, split into its low hl (<= 10) and high hh
  // items. A subset of Y is (high part h, low part l); for each size b its subsets are stored grouped by h,
  // each group running over every l of the size it needs, in a fixed order (Lall: low parts by size); groups
  // are ordered by that low size, so groups sharing their l run side by side
  int hY = 0, hl = 0, hh = 0;
  std::vector<uint32_t> Lall; int Loff[18] = {0};        // low parts with 0, 1, ... bits; Loff[c] = start of size c
  struct YGroup { uint32_t h; int c; uint32_t off; };
  struct YRun { std::vector<YGroup> g; std::vector<double> QB; std::vector<float> QBf; uint32_t n = 0; int gc[18] = {0}; };   // gc[c]..gc[c + 1]: groups of low size c
  std::vector<YRun> yrun;
  std::vector<double> TBh, TBl;                          // T of each high part; of each low part, in Lall order
  std::vector<float> TBlf, TL2f;                         // TBl and TBl^2 in single precision (the screen)
  // per item x of block X: twice its cross-products with each low part (Lall order, single) and each high part
  std::vector<float> CLf; std::vector<double> CHd, cabs;  // cabs[x]: twice the sum of |C(x, y)| over Y
  double mTBh = 0, mTl = 0, mQB = 0;                     // their largest magnitudes (and QB's), for rounding allowances
  std::vector<float> Cf, tf;           // C and t in single precision (the plain walk's screen)
  double mt = 0, mC = 0;               // largest |t_j|, |C(i, j)| (i != j): rounding allowances
  int sgn;                             // +1: every combination's T >= 0; -1: every T <= 0; 0: either
  bool screen_on;
  // The threshold shared by threads, on a cache line of its own (128 bytes covers every current CPU):
  // otherwise each update would evict the fields above from every other thread's cache
  alignas(128) std::atomic<double> shared_thr;
  char pad_[128 - sizeof(std::atomic<double>)];

  GramSearch(const std::vector<double>& Cin, const std::vector<double>& diag, const std::vector<double>& tin,
             double ssy, int p, int k, int K, double T0 = 0, double Q0 = 0, double screen_ratio = SCREEN_RATIO_EXHAUSTIVE)
    : p(p), k(k), K(K), ssy(ssy), T0(T0), Q0(Q0), perm(p), pinv(p), tc(tin.begin(), tin.begin() + p),
      dc(diag.begin(), diag.begin() + p), C((size_t)p * p), u0(p), t(p), LG((size_t)(k + 1) * p, 0.0),
      HG((size_t)(k + 1) * p, 0.0), LR((size_t)(k + 1) * p, 0.0), shared_thr(-2.0) {
    // Strongest single items first: each item's own key with the fixed items
    std::vector<double> s(p);
    for (int j = 0; j < p; ++j) { const double q = Q0 + diag[j], tt = T0 + tin[j]; s[j] = q > 0 ? tt * tt / q : -1.0; }
    for (int j = 0; j < p; ++j) perm[j] = j;
    std::stable_sort(perm.begin(), perm.end(), [&](int a, int b) { return s[a] > s[b]; });
    for (int q = 0; q < p; ++q) pinv[perm[q]] = q;
    for (int b = 0; b < p; ++b) {
      const int ob = perm[b]; u0[b] = diag[ob]; t[b] = tin[ob];
      for (int a = 0; a < p; ++a) C[(size_t)b * p + a] = Cin[(size_t)ob * p + perm[a]];
    }
    std::vector<double> col(p);
    for (int j = 0; j < p; ++j) {
      int n = 0; for (int i = 0; i < p; ++i) if (i != j) col[n++] = C[(size_t)j * p + i];
      std::sort(col.begin(), col.begin() + n);
      for (int r = 1; r <= k; ++r) {
        LG[(size_t)r * p + j] = LG[(size_t)(r - 1) * p + j] + (r - 1 < n ? col[r - 1] : 0.0);
        HG[(size_t)r * p + j] = HG[(size_t)(r - 1) * p + j] + (r - 1 < n ? col[n - r] : 0.0);
      }
      n = 0; for (int i = j + 1; i < p; ++i) col[n++] = C[(size_t)j * p + i];
      std::sort(col.begin(), col.begin() + n);
      for (int r = 1; r <= k; ++r) LR[(size_t)r * p + j] = LR[(size_t)(r - 1) * p + j] + (r - 1 < n ? col[r - 1] : 0.0);
    }
    Cf.assign(C.begin(), C.end()); tf.assign(t.begin(), t.end());
    for (int j = 0; j < p; ++j) { mt = std::max(mt, std::fabs(t[j])); for (int i = 0; i < p; ++i) if (i != j) mC = std::max(mC, std::fabs(C[(size_t)j * p + i])); }
    TMIN.assign((size_t)(k + 1) * (p + 1), INFINITY); TMAX.assign((size_t)(k + 1) * (p + 1), -INFINITY);
    std::vector<double> srt;                 // t over [s, p), ascending
    for (int s0 = p; s0 >= 0; --s0) {
      if (s0 < p) srt.insert(std::upper_bound(srt.begin(), srt.end(), t[s0]), t[s0]);
      const int n = (int)srt.size(); double lo = 0, hi = 0; TMIN[s0] = 0; TMAX[s0] = 0;
      for (int m = 1; m <= k && m <= n; ++m) { lo += srt[m - 1]; hi += srt[n - m]; TMIN[(size_t)m * (p + 1) + s0] = lo; TMAX[(size_t)m * (p + 1) + s0] = hi; }
    }
    sgn = T0 + TMIN[(size_t)k * (p + 1)] >= 0 ? 1 : (T0 + TMAX[(size_t)k * (p + 1)] <= 0 ? -1 : 0);
    binom.assign((size_t)(p + 1) * (k + 1), 0.0);
    for (int n = 0; n <= p; ++n) {
      binom[(size_t)n * (k + 1)] = 1;
      for (int m = 1; m <= k && m <= n; ++m) binom[(size_t)n * (k + 1) + m] = binom[(size_t)(n - 1) * (k + 1) + m - 1] + (m <= n - 1 ? binom[(size_t)(n - 1) * (k + 1) + m] : 0.0);
    }
    screen_on = k >= 2 && (double)(p - k + 2) * (p - k + 1) >= screen_ratio * (double)k * (k - 1);
    if (!screen_on && k >= 3 && p >= SPLIT_MIN_P && calculate_combinations_double(p, k) >= SPLIT_MIN_COMBOS)
      build_split(std::min(SPLIT_HY, p - 2));
  }
  void build_split(int h) {
    hY = h; hl = std::min(SPLIT_HL, h); hh = h - hl; const int Y0 = p - h;
    for (int c = 0, at = 0; c <= hl; ++c) { Loff[c] = at; for (uint32_t m = 0; m < (1u << hl); ++m) if (rt_popcount(m) == c) { Lall.push_back(m); ++at; } }
    Loff[hl + 1] = (int)Lall.size();
    auto Tof = [&](uint32_t m, int base) { double s = 0; for (int y = 0; m; ++y, m >>= 1) if (m & 1u) s += t[Y0 + base + y]; return s; };
    auto Qof = [&](uint32_t m, int base) { double s = 0; for (int x = 0; x < 16; ++x) if (m >> x & 1u) { const int gx = Y0 + base + x; s += u0[gx];
        for (int z = x + 1; z < 16; ++z) if (m >> z & 1u) s += 2.0 * C[(size_t)gx * p + Y0 + base + z]; } return s; };
    TBh.resize(1u << hh); std::vector<double> QBh(1u << hh), QBl(1u << hl);
    for (uint32_t m = 0; m < (1u << hh); ++m) { TBh[m] = Tof(m, hl); QBh[m] = Qof(m, hl); }
    for (uint32_t m = 0; m < (1u << hl); ++m) QBl[m] = Qof(m, 0);
    TBl.resize(Lall.size()); for (size_t i = 0; i < Lall.size(); ++i) TBl[i] = Tof(Lall[i], 0);
    yrun.assign(std::min(k, h) + 1, YRun());
    for (int bsz = 0; bsz <= std::min(k, h); ++bsz) {
      YRun& R = yrun[bsz];
      for (int c = 0; c <= hl; ++c) { R.gc[c] = (int)R.g.size();
      for (uint32_t hm = 0; hm < (1u << hh); ++hm) {
        if (bsz - rt_popcount(hm) != c) continue;
        R.g.push_back({hm, c, R.n});
        double xh[16] = {0};                                  // C between the high part and each low item
        for (int z = 0; z < hl; ++z) for (int x = 0; x < hh; ++x) if (hm >> x & 1u) xh[z] += C[(size_t)(Y0 + hl + x) * p + Y0 + z];
        for (int i = Loff[c]; i < Loff[c + 1]; ++i) {
          const uint32_t l = Lall[i]; double X = 0; for (int z = 0; z < hl; ++z) if (l >> z & 1u) X += xh[z];
          R.QB.push_back(QBh[hm] + QBl[l] + 2.0 * X); ++R.n;
        }
      } }
      R.gc[hl + 1] = (int)R.g.size();
      for (double v : R.QB) mQB = std::max(mQB, std::fabs(v));
      R.QBf.assign(R.QB.begin(), R.QB.end());
    }
    TBlf.assign(TBl.begin(), TBl.end()); TL2f.resize(TBl.size()); for (size_t i = 0; i < TBl.size(); ++i) TL2f[i] = (float)(TBl[i] * TBl[i]);
    const int Xn = p - h, nL = (int)Lall.size(), nH = 1 << hh;
    CLf.resize((size_t)Xn * nL); CHd.resize((size_t)Xn * nH); cabs.assign(Xn, 0.0);
    std::vector<double> cl(1u << hl);
    for (int x = 0; x < Xn; ++x) {
      const double* Cx = &C[(size_t)x * p + Y0];
      cl[0] = 0; for (int z = 0; z < hl; ++z) { const uint32_t n = 1u << z; for (uint32_t m = 0; m < n; ++m) cl[n + m] = cl[m] + 2.0 * Cx[z]; }
      for (int i = 0; i < nL; ++i) CLf[(size_t)x * nL + i] = (float)cl[Lall[i]];
      double* ch = &CHd[(size_t)x * nH]; ch[0] = 0;
      for (int z = 0; z < hh; ++z) { const uint32_t n = 1u << z; for (uint32_t m = 0; m < n; ++m) ch[n + m] = ch[m] + 2.0 * Cx[hl + z]; }
      for (int y = 0; y < h; ++y) cabs[x] += 2.0 * std::fabs(Cx[y]);
    }
    for (double v : TBh) mTBh = std::max(mTBh, std::fabs(v));
    for (double v : TBl) mTl = std::max(mTl, std::fabs(v));
  }
  inline double shared() const { return shared_thr.load(std::memory_order_relaxed); }
  inline void publish(double v) {
    double cur = shared_thr.load(std::memory_order_relaxed);
    while (v > cur && !shared_thr.compare_exchange_weak(cur, v, std::memory_order_relaxed)) {}
  }
};

// The split walk's screen for G groups at once (sharing the loads of f and t): hit[g] = whether any
// f[i] + c[g] t[i] - th q[g][i] reaches lim[g]
template <int G>
RT_INLINE void split_hit_fG(const float* __restrict f, const float* __restrict t, const float* const* q, int len, const float* c, float th,
                            const float* lim, bool* hit) {
#ifdef RT_NEON
  if (len >= 4) {
    const float32x4_t vt = vdupq_n_f32(th);
    float32x4_t vc[G], m[G];
    {                                               // the last block (overlapping the one before when len % 4 != 0)
      const float32x4_t fv = vld1q_f32(f + len - 4), tv = vld1q_f32(t + len - 4);
      for (int g = 0; g < G; ++g) { vc[g] = vdupq_n_f32(c[g]); m[g] = vfmsq_f32(vfmaq_f32(fv, vc[g], tv), vt, vld1q_f32(q[g] + len - 4)); }
    }
    for (int i = 0; i + 4 < len; i += 4) {
      const float32x4_t fv = vld1q_f32(f + i), tv = vld1q_f32(t + i);
      for (int g = 0; g < G; ++g) m[g] = vmaxq_f32(m[g], vfmsq_f32(vfmaq_f32(fv, vc[g], tv), vt, vld1q_f32(q[g] + i)));
    }
    for (int g = 0; g < G; ++g) hit[g] = vmaxvq_f32(m[g]) >= lim[g];
    return;
  }
#endif
  // (written as a flag OR, which compilers vectorise; a running maximum they won't)
  for (int g = 0; g < G; ++g) { int h = 0; for (int i = 0; i < len; ++i) h |= f[i] + c[g] * t[i] - th * q[g][i] >= lim[g]; hit[g] = h != 0; }
}

// Sets a[0..N) to -inf one element at a time: a loop would become a library call (memset_pattern16) that
// also keeps small arrays out of registers
template <std::size_t... I> RT_INLINE void neg_fill_(double* a, std::index_sequence<I...>) { ((a[I] = -INFINITY), ...); }
template <int N> RT_INLINE void neg_fill(double* a) { neg_fill_(a, std::make_index_sequence<N>{}); }

// x[0..r) holds the r largest values seen, descending (branch-free insertion)
inline void top_insert(double* x, int r, double v) {
  for (int q = r - 1; q > 0; --q) x[q] = std::max(x[q], std::min(x[q - 1], v));
  x[0] = std::max(x[0], v);
}
#ifdef RT_NEON
// The same with two lanes: each lane keeps its own list
template <int N> RT_INLINE void vtop_insert(float64x2_t* x, float64x2_t v) {
  for (int q = N - 1; q > 0; --q) x[q] = vmaxq_f64(x[q], vminq_f64(x[q - 1], v));
  x[0] = vmaxq_f64(x[0], v);
}
template <std::size_t... I> RT_INLINE void vneg_fill_(float64x2_t* x, std::index_sequence<I...>) { ((x[I] = vdupq_n_f64(-INFINITY)), ...); }
template <int N> RT_INLINE void vneg_fill(float64x2_t* x) { vneg_fill_(x, std::make_index_sequence<N>{}); }
// The sum of the N largest of everything in lists a and b (2 lanes each): with b merged into a lane by lane,
// each lane is a sorted list, so the N largest take the first i of lane 0 and the first N - i of lane 1, for
// the best i -- read off the two lanes' running sums
template <int N> RT_INLINE double vtop_sum(float64x2_t* a, const float64x2_t* b) {
  for (int q = 0; q < N; ++q) vtop_insert<N>(a, b[q]);
  double p0[N + 1], p1[N + 1]; p0[0] = p1[0] = 0;
  float64x2_t run = a[0]; p0[1] = vgetq_lane_f64(run, 0); p1[1] = vgetq_lane_f64(run, 1);
  for (int q = 1; q < N; ++q) { run = vaddq_f64(run, a[q]); p0[q + 1] = vgetq_lane_f64(run, 0); p1[q + 1] = vgetq_lane_f64(run, 1); }
  double best = -INFINITY;
  for (int i = 0; i <= N; ++i) best = std::max(best, p0[i] + p1[N - i]);
  return best;
}
// The same for a single list
template <int N> RT_INLINE double vtop_sum1(const float64x2_t* a) {
  double p0[N + 1], p1[N + 1]; p0[0] = p1[0] = 0;
  float64x2_t run = a[0]; p0[1] = vgetq_lane_f64(run, 0); p1[1] = vgetq_lane_f64(run, 1);
  for (int q = 1; q < N; ++q) { run = vaddq_f64(run, a[q]); p0[q + 1] = vgetq_lane_f64(run, 0); p1[q + 1] = vgetq_lane_f64(run, 1); }
  double best = -INFINITY;
  for (int i = 0; i <= N; ++i) best = std::max(best, p0[i] + p1[N - i]);
  return best;
}
// One or two more values into a lane list (the missing one as -inf)
RT_INLINE float64x2_t vpair(const double* x, int n) { return n >= 2 ? vld1q_f64(x) : vsetq_lane_f64(x[0], vdupq_n_f64(-INFINITY), 0); }
#endif

struct GramSearchWorker : public RcppParallel::Worker {
  GramSearch& S;
  const int p, k, K;
  const std::vector<int>& task_items; // branch prefixes, flattened (variable length)
  const std::vector<int>& task_off;   // prefix of task i is task_items[task_off[i] .. task_off[i + 1])

  struct HeapEntry { double key; double r; int slot; };
  std::vector<HeapEntry> heap;        // min-heap: front() is the worst kept entry
  std::vector<int> slots;             // K x k, each kept combination in the caller's indices, ascending

  // per-worker scratch, reused across calls
  std::vector<double> U, T, Q, SP, SN; // U[d * p + j]: u_j for the branch of length d; SP/SN: the last two levels' suffix maxima
  std::vector<int> idx, cand, kids;     // kids[d * p ..]: the children of the branch of length d that pass the cheap test
  std::vector<unsigned char> live;
  int win_t[MAX_BOUND_ITEMS + 1] = {0}, win_k[MAX_BOUND_ITEMS + 1] = {0}, probe[MAX_BOUND_ITEMS + 1] = {0};
  bool off[MAX_BOUND_ITEMS + 1] = {false};   // per items-still-to-add: tests stopped paying (kill rate < 5%), only probe

  GramSearchWorker(GramSearch& S, const std::vector<int>& task_items, const std::vector<int>& task_off)
    : S(S), p(S.p), k(S.k), K(S.K), task_items(task_items), task_off(task_off), slots((size_t)S.K * S.k) { heap.reserve(K); }
  GramSearchWorker(const GramSearchWorker& o, RcppParallel::Split)
    : S(o.S), p(o.p), k(o.k), K(o.K), task_items(o.task_items), task_off(o.task_off), slots((size_t)o.K * o.k) { heap.reserve(K); }

  // true if (key_a, combo_a) should rank above (key_b, combo_b)
  inline bool better(double ka, const int* a, double kb, const int* b) const {
    if (ka != kb) return ka > kb;
    for (int d = 0; d < k; ++d) if (a[d] != b[d]) return a[d] < b[d];
    return false;
  }
  inline const int* combo_of(const HeapEntry& e) const { return &slots[(size_t)e.slot * k]; }
  struct HeapCmp {
    const GramSearchWorker* w;
    bool operator()(const HeapEntry& a, const HeapEntry& b) const { return w->better(a.key, w->combo_of(a), b.key, w->combo_of(b)); }
  };
  // Screening threshold: the K-th best key known to any worker, lowered by a relative 1e-12 (see above).
  // The shared value is re-read only now and then: other threads update it, and reading it on every
  // branch would keep missing the cache (a stale value is merely lower, so it only prunes less)
  double sh_seen = -2.0; unsigned sh_reads = 0;
  inline double threshold() {
    if ((++sh_reads & 1023u) == 0) sh_seen = S.shared();
    const double own = ((int)heap.size() < K) ? -2.0 : heap.front().key, h = own > sh_seen ? own : sh_seen;
    return h > 0 ? h * (1.0 - 1e-12) : h;
  }
  inline void publish() { const double v = heap.front().key; if (v > sh_seen) { S.publish(v); sh_seen = std::max(sh_seen, S.shared()); } }

  // Offer a combination already in the caller's indices, ascending
  inline void offer_sorted(double key, double r, const int* c) {
    HeapCmp cmp{this};
    if ((int)heap.size() < K) {
      const int slot = (int)heap.size();
      std::copy(c, c + k, slots.begin() + (size_t)slot * k);
      heap.push_back({key, r, slot}); std::push_heap(heap.begin(), heap.end(), cmp);
      if ((int)heap.size() == K) publish();
    } else if (better(key, c, heap.front().key, combo_of(heap.front()))) {
      std::pop_heap(heap.begin(), heap.end(), cmp);
      HeapEntry& e = heap.back();
      std::copy(c, c + k, slots.begin() + (size_t)e.slot * k); e.key = key; e.r = r;
      std::push_heap(heap.begin(), heap.end(), cmp);
      publish();
    }
  }
  // Offer the combination idx[0..k) (search order): in the caller's indices, sorted, re-scored in that order
  inline void offer() {
    for (int d = 0; d < k; ++d) {
      const int v = S.perm[idx[d]]; int q = d;
      while (q > 0 && cand[q - 1] > v) { cand[q] = cand[q - 1]; --q; }
      cand[q] = v;
    }
    const int* c = cand.data();
    auto Cc = [&](int a, int b) { return S.C[(size_t)S.pinv[a] * p + S.pinv[b]]; };   // the caller's C[a * p + b]
    double Tj, Qj;
    if (k == 1) {
      Tj = S.T0 + S.tc[c[0]]; Qj = S.Q0 + S.dc[c[0]];
    } else {
      double Tq = S.T0, Qq = S.Q0;
      for (int d = 0; d < k - 2; ++d) {
        const int i = c[d]; double Ri = 0.0;
        for (int e = 0; e < d; ++e) Ri = Ri + Cc(c[e], i);
        Tq = Tq + S.tc[i]; Qq = Qq + S.dc[i] + 2.0 * Ri;
      }
      const int j1 = c[k - 2], j2 = c[k - 1];
      double R1 = 0.0, R2 = 0.0;
      for (int e = 0; e < k - 2; ++e) { R1 = R1 + Cc(c[e], j1); R2 = R2 + Cc(c[e], j2); }
      const double u1 = S.dc[j1] + 2.0 * R1, u2 = S.dc[j2] + 2.0 * R2;
      const double Ta = Tq + S.tc[j1], Qa = Qq + u1;
      Tj = Ta + S.tc[j2]; Qj = Qa + u2 + 2.0 * Cc(j1, j2);
    }
    if (Qj > 0) offer_sorted(Tj * Tj / Qj, Tj / std::sqrt(Qj * S.ssy), c);
    else offer_sorted(-1.0, NA_REAL, c);    // invalid (zero-variance) combination: only kept while the heap isn't full
  }

  // Sign mode of the branch of length d with m items still to add from [start, p): +1 if every
  // completion has T >= 0, -1 if every one has T <= 0, else 0 (both sides need testing)
  inline int node_sign(int d, int m, int start) const {
    if (S.sgn) return S.sgn;
    const size_t o = (size_t)m * (p + 1) + start;
    return T[d] + S.TMIN[o] >= 0 ? 1 : (T[d] + S.TMAX[o] <= 0 ? -1 : 0);
  }

  // Score one combination of the last level
  inline void leaf(int j2, double Ta, double Qa, double u2, double c2, double& thr) {
    const double Tj = Ta + S.t[j2];
    const double Qj = Qa + u2 + 2.0 * c2;
    if (Qj > 0) {
      if (Tj * Tj >= thr * Qj) { idx[k - 1] = j2; offer(); thr = threshold(); }
    } else if (thr <= -1.0) { idx[k - 1] = j2; offer(); thr = threshold(); }
  }

  // One row of the last two levels: {branch, j1, j2} for every j2 > j1, screened a block at a time
  // (most blocks hold nothing above the threshold); `stop` ends the row early once the bound shows
  // no later j2 can reach it
  template <class Stop>
  inline void row(int j1, double Ta, double Qa, const double* __restrict u, double& thr, Stop stop) {
    const double* __restrict tt = S.t.data();
    const double* __restrict Cj = &S.C[(size_t)j1 * p];
    idx[k - 2] = j1;
    int j2 = j1 + 1;
    for (; j2 + LEAF_BLOCK <= p; j2 += LEAF_BLOCK) {
      if (stop(j2)) return;
      int hit = 0;
      for (int q = 0; q < LEAF_BLOCK; ++q) {
        const double Tj = Ta + tt[j2 + q];
        const double Qj = Qa + u[j2 + q] + 2.0 * Cj[j2 + q];
        hit |= (Tj * Tj >= thr * Qj) | (Qj <= 0);
      }
      if (hit) for (int q = 0; q < LEAF_BLOCK; ++q) leaf(j2 + q, Ta, Qa, u[j2 + q], Cj[j2 + q], thr);
    }
    for (; j2 < p; ++j2) {
      const double Tj = Ta + tt[j2];
      const double Qj = Qa + u[j2] + 2.0 * Cj[j2];
      if (Qj > 0) {
        if (Tj * Tj >= thr * Qj) { idx[k - 1] = j2; offer(); thr = threshold(); }
      } else if (thr <= -1.0) { idx[k - 1] = j2; offer(); thr = threshold(); }
    }
  }

  // Branch of length k - 1 (k == 1, or a task prefix that long): finish it with every remaining item
  void finish_single(int d, int start) {
    double thr = threshold();
    const double* u = &U[(size_t)d * p];
    for (int j = start; j < p; ++j) leaf(j, T[d], Q[d], u[j], 0.0, thr);
  }

  // ---- Plain walk (no tests): searches that don't screen, and anything under a branch too small to ----
  // In bushy trees (e.g. 14 of 32) a branch of the last two levels holds only a few combinations, so the
  // walk is a single loop over the branch stack (no recursion, no call per branch), with the last two
  // levels finished inline and the threshold re-read only after an offer or every 1024 branches

  // The last two levels of the branch of length k - 2 (its u in U[k - 2]), items from `start` on
  std::vector<float> EF;                    // per-branch e_j = t_j^2 - th u_j (single), for rows_plain's screen
  RT_INLINE void rows_plain(int start, double& thr) {
    const int d = k - 2;
    const double Tp = T[d], Qp = Q[d];
    const double* __restrict u = &U[(size_t)d * p];
    const double* __restrict tt = S.t.data();
#ifdef RT_NEON
    // Screen the whole branch first, in single precision: (Ta + t2)^2 - th (Qa + u2 + 2 C12) =
    //   G(j1) + 2 Ta t2 + e(j2) - 2 th C12,  G = Ta^2 - th Qa,  e = t^2 - th u
    // (with th = the current threshold); if nothing reaches -allowance, the branch holds nothing
    if (thr > 0 && p - start >= 6) {
      const double th = thr;
      float* __restrict ef = EF.data(); const float* __restrict tf = S.tf.data();
      double umax = 0;
      for (int j = start; j < p; ++j) { ef[j] = (float)(tt[j] * tt[j] - th * u[j]); umax = std::max(umax, std::fabs(u[j])); }
      const double Tabs = std::fabs(Tp) + 2.0 * S.mt, Qabs = std::fabs(Qp) + 2.0 * umax + 2.0 * S.mC;
      const double allow = 2e-6 * (Tabs * Tabs + th * Qabs);
      const float32x4_t v2t = vdupq_n_f32((float)(2.0 * th));
      float32x4_t m0 = vdupq_n_f32(-INFINITY), m1 = m0; float mx = -INFINITY;
      for (int j1 = start; j1 <= p - 2; ++j1) {
        const double Ta = Tp + tt[j1], Qa = Qp + u[j1];
        const float g = (float)(Ta * Ta - th * Qa + allow), c1 = (float)(2.0 * Ta);
        const float* __restrict cf = &S.Cf[(size_t)j1 * p];
        if (p - 1 - j1 >= 4) {
          const float32x4_t vg = vdupq_n_f32(g), vc = vdupq_n_f32(c1);
          auto v4 = [&](int i) { return vaddq_f32(vfmsq_f32(vfmaq_f32(vld1q_f32(ef + i), vc, vld1q_f32(tf + i)), v2t, vld1q_f32(cf + i)), vg); };
          int i = j1 + 1;
          for (; i + 8 <= p; i += 8) { m0 = vmaxq_f32(m0, v4(i)); m1 = vmaxq_f32(m1, v4(i + 4)); }
          if (i + 4 <= p) { m0 = vmaxq_f32(m0, v4(i)); i += 4; }
          if (i < p) m1 = vmaxq_f32(m1, v4(p - 4));           // the last few, overlapping (a maximum doesn't mind)
        } else {
          for (int i = j1 + 1; i < p; ++i) mx = std::max(mx, g + ef[i] + c1 * tf[i] - (float)(2.0 * th) * cf[i]);
        }
      }
      if (!(std::max(vmaxvq_f32(vmaxq_f32(m0, m1)), mx) >= 0.0f)) return;
    }
#endif
    for (int j1 = start; j1 <= p - 2; ++j1) {
      const double Ta = Tp + tt[j1], Qa = Qp + u[j1];
      const double* __restrict Cj = &S.C[(size_t)j1 * p];
      int j2 = j1 + 1;
      for (; j2 + LEAF_BLOCK <= p; j2 += LEAF_BLOCK) {
        int hit = 0;
        for (int q = 0; q < LEAF_BLOCK; ++q) {
          const double Tj = Ta + tt[j2 + q];
          const double Qj = Qa + u[j2 + q] + 2.0 * Cj[j2 + q];
          hit |= (Tj * Tj >= thr * Qj) | (Qj <= 0);
        }
        if (hit) { idx[d] = j1; for (int q = 0; q < LEAF_BLOCK; ++q) leaf(j2 + q, Ta, Qa, u[j2 + q], Cj[j2 + q], thr); }
      }
      for (; j2 < p; ++j2) {
        const double Tj = Ta + tt[j2];
        const double Qj = Qa + u[j2] + 2.0 * Cj[j2];
        if ((Tj * Tj >= thr * Qj) | (Qj <= 0)) { idx[d] = j1; leaf(j2, Ta, Qa, u[j2], Cj[j2], thr); }
      }
    }
  }
  void pairs_plain(int start) { double thr = threshold(); rows_plain(start, thr); }
  // ---- Split walk: subsets A of block X (items [start, p - hY)) by the usual walk, and for each, every
  // B of block Y from the precomputed list: Q = Q(prefix + A) + QB(B) + cross, cross = sum over B of
  // r_y = U[y] - C(y, y) (twice y's cross-products with prefix + A): SL[l] + SH[h] over B's low and high
  // parts. The running sums drift in the last bits, so the double check there leaves a relative 1e-9;
  // every combination offered is re-scored exactly
  std::vector<double> rY, SL; std::vector<float> Ff, HGL, HGC;  // r over block Y; SL in mask order (scratch); E(l); per high part: limit, 2 Th
  std::vector<float> SLf; std::vector<double> SHd, rtot; std::vector<int> nstep;  // per depth: SL (Lall order, single), SH, and their error bounds
  void split_B(int d, int b, double& thr) {
    const GramSearch::YRun& R = S.yrun[b];
    const int Y0 = p - S.hY, hl = S.hl, hh = S.hh;
    const double* __restrict u = &U[(size_t)d * p + Y0];
    const double* __restrict u0 = &S.u0[Y0];
    double* __restrict r = rY.data();
    for (int y = 0; y < S.hY; ++y) r[y] = u[y] - u0[y];
    const double TA = T[d], QA = Q[d];
    double ths = thr > 0 ? thr * (1.0 - 1e-9) : thr;
    const double* __restrict QB = R.QB.data(); const double* __restrict TBl = S.TBl.data();
    const uint32_t* __restrict La = S.Lall.data();
    if (R.n < SPLIT_TABLE_MIN) {
      // short list: each B's cross term summed directly (fewer additions than building the tables)
      for (const GramSearch::YGroup& G : R.g) {
        double ch = 0; for (int x = 0; x < hh; ++x) if (G.h >> x & 1u) ch += r[hl + x];
        const double Th = TA + S.TBh[G.h], Qh = QA + ch;
        const int lo = S.Loff[G.c], len = S.Loff[G.c + 1] - lo;
        const double* __restrict qb = QB + G.off; const double* __restrict tl = TBl + lo;
        for (int i = 0; i < len; ++i) {
          double cl = 0; for (uint32_t l = La[lo + i]; l; l &= l - 1) cl += r[rt_ctz(l)];
          const double Tj = Th + tl[i], Qj = Qh + qb[i] + cl;
          if (!((Tj * Tj >= ths * Qj) | (Qj <= 0))) continue;
          if (!(Qj > 0 ? Tj * Tj >= ths * Qj : thr <= -1.0)) continue;
          const uint32_t mask = La[lo + i] | (G.h << hl);
          int c = d; for (int y = 0; y < S.hY; ++y) if (mask >> y & 1u) idx[c++] = Y0 + y;
          offer(); thr = threshold(); ths = thr > 0 ? thr * (1.0 - 1e-9) : thr;
        }
      }
      return;
    }
    const int nL = (int)S.Lall.size();
    const float* __restrict slf = &SLf[(size_t)d * nL]; const double* __restrict sh = &SHd[(size_t)d << hh];
    if (!(ths > 0)) {
      // heap not full yet: every combination is offered (the general check below)
      for (const GramSearch::YGroup& G : R.g) {
        const double Th = TA + S.TBh[G.h], Qh = QA + sh[G.h];
        const int lo = S.Loff[G.c], len = S.Loff[G.c + 1] - lo;
        const double* __restrict qb = QB + G.off; const double* __restrict tl = TBl + lo;
        for (int i = 0; i < len; ++i) {
          double cl = 0; for (uint32_t l = La[lo + i]; l; l &= l - 1) cl += r[rt_ctz(l)];
          const double Tj = Th + tl[i], Qj = Qh + qb[i] + cl;
          if (!(Qj > 0 ? Tj * Tj >= ths * Qj : thr <= -1.0)) continue;
          const uint32_t mask = La[lo + i] | (G.h << hl);
          int c = d; for (int y = 0; y < S.hY; ++y) if (mask >> y & 1u) idx[c++] = Y0 + y;
          offer(); thr = threshold(); ths = thr > 0 ? thr * (1.0 - 1e-9) : thr;
        }
      }
      return;
    }
    // T^2 - th Q = G(h) + 2 Th t(l) + E(l) - th QB(h, l), with Th = TA + TB(h), th fixed for this A (below the
    // threshold, so passing more, never fewer) and
    //   G(h) = Th^2 - th (QA + SH[h]),   E(l) = t(l)^2 - th SL[l]
    // in single precision: a combination costs two multiply-adds and a max. Groups whose largest value is
    // below -allowance hold nothing; the others are checked entry by entry in double
    const double th = ths;
    double rabs = 0; for (int y = 0; y < S.hY; ++y) rabs += std::fabs(r[y]);
    const double Tabs = std::fabs(TA) + S.mTBh + S.mTl, Qabs = std::fabs(QA) + rtot[d] + rabs + S.mQB;
    // allowance: the ~10 roundings of the screen, each within 2^-24 of a term no larger than Tabs^2 + th Qabs,
    // and SL's running single-precision sums (nstep[d] additions, each within 2^-24 of rtot[d])
    const double allow = 2e-6 * (Tabs * Tabs + th * Qabs) + 2.4e-7 * (nstep[d] + 2) * th * rtot[d];
    const int cmin = std::max(0, b - hh), cmax = std::min(b, hl);
    float* __restrict Ev = Ff.data(); const float* __restrict tlf = S.TBlf.data(); const float* __restrict tl2 = S.TL2f.data();
    const float thf = (float)th;
    for (int i = S.Loff[cmin]; i < S.Loff[cmax + 1]; ++i) Ev[i] = tl2[i] - thf * slf[i];
    const float* __restrict QBf = R.QBf.data();
    // per high part h: the group's limit -(G(h)) - allowance and 2 Th, for every h at once
    float* __restrict GL = HGL.data(); float* __restrict GC = HGC.data();
    { const double* __restrict tbh = S.TBh.data(); const int nH = 1 << hh;
      for (int h = 0; h < nH; ++h) { const double Th = TA + tbh[h]; GL[h] = (float)(th * (QA + sh[h]) - Th * Th - allow); GC[h] = (float)(2.0 * Th); } }
    // groups of the same low size share their f and t: screened four (then two, one) at a time
    auto check = [&](const GramSearch::YGroup& G, double Th, double Qh) {
      const int lo = S.Loff[G.c], len = S.Loff[G.c + 1] - lo;
      const double* __restrict qb = QB + G.off; const double* __restrict tl = TBl + lo;
      for (int i = 0; i < len; ++i) {
        double cl = 0; for (uint32_t l = La[lo + i]; l; l &= l - 1) cl += r[rt_ctz(l)];
        const double Tj = Th + tl[i], Qj = Qh + qb[i] + cl;
        if (!(Qj > 0 ? Tj * Tj >= ths * Qj : thr <= -1.0)) continue;
        const uint32_t mask = La[lo + i] | (G.h << hl);
        int c = d; for (int y = 0; y < S.hY; ++y) if (mask >> y & 1u) idx[c++] = Y0 + y;
        offer(); thr = threshold(); ths = thr > 0 ? thr * (1.0 - 1e-9) : thr;
      }
    };
    for (int c = cmin; c <= cmax; ++c) {
      const int g0 = R.gc[c], g1 = R.gc[c + 1], lo = S.Loff[c], len = S.Loff[c + 1] - lo;
      const float* __restrict ev = Ev + lo; const float* __restrict tv = tlf + lo;
      int g = g0;
      float cf[4], lim[4]; bool hit[4]; const float* qp[4];
      auto prep = [&](int q, const GramSearch::YGroup& G) { lim[q] = GL[G.h]; cf[q] = GC[G.h]; qp[q] = QBf + G.off; };
      auto chk = [&](int q, const GramSearch::YGroup& G) { if (hit[q]) check(G, TA + S.TBh[G.h], QA + sh[G.h]); };
      for (; g + 4 <= g1; g += 4) {
        for (int q = 0; q < 4; ++q) prep(q, R.g[g + q]);
        split_hit_fG<4>(ev, tv, qp, len, cf, thf, lim, hit);
        for (int q = 0; q < 4; ++q) chk(q, R.g[g + q]);
      }
      switch (g1 - g) {                             // the rest in one pass
        case 3: for (int q = 0; q < 3; ++q) prep(q, R.g[g + q]); split_hit_fG<3>(ev, tv, qp, len, cf, thf, lim, hit); for (int q = 0; q < 3; ++q) chk(q, R.g[g + q]); break;
        case 2: for (int q = 0; q < 2; ++q) prep(q, R.g[g + q]); split_hit_fG<2>(ev, tv, qp, len, cf, thf, lim, hit); for (int q = 0; q < 2; ++q) chk(q, R.g[g + q]); break;
        case 1: prep(0, R.g[g]); split_hit_fG<1>(ev, tv, qp, len, cf, thf, lim, hit); chk(0, R.g[g]); break;
        default: break;
      }
    }
  }
  // A: the branch of length d (its u in U[d]); a = its items from block X so far. SLf/SHd[d] hold the
  // branch's low / high-part cross sums; each child adds its own item's (only the sizes still needed)
  void split_A(int d, int start, int a, int m, double& thr) {
    const int b = m - a, hl = S.hl, hh = S.hh, nL = (int)S.Lall.size(), nH = 1 << hh;
    if (a == 0) {                            // top of the split: the branch's sums from its u
      const int Y0 = p - S.hY;
      double* __restrict r = rY.data(); const double* __restrict u = &U[(size_t)d * p + Y0];
      double ra = 0; for (int y = 0; y < S.hY; ++y) { r[y] = u[y] - S.u0[Y0 + y]; ra += std::fabs(r[y]); }
      double* __restrict sl = SL.data();
      sl[0] = 0; for (int z = 0; z < hl; ++z) { const double rz = r[z]; const uint32_t n = 1u << z; for (uint32_t q = 0; q < n; ++q) sl[n + q] = sl[q] + rz; }
      float* __restrict slf = &SLf[(size_t)d * nL]; for (int i = 0; i < nL; ++i) slf[i] = (float)sl[S.Lall[i]];
      double* __restrict sh = &SHd[(size_t)d * nH];
      sh[0] = 0; for (int z = 0; z < hh; ++z) { const double rz = r[hl + z]; const uint32_t n = 1u << z; for (uint32_t q = 0; q < n; ++q) sh[n + q] = sh[q] + rz; }
      rtot[d] = ra; nstep[d] = 0;
    }
    if (b <= S.hY) split_B(d, b, thr);
    if (a == m) return;
    const int Xe = p - S.hY;
    const double* __restrict u = &U[(size_t)d * p];
    double* __restrict un = &U[(size_t)(d + 1) * p];
    const int nl = S.Loff[std::min(hl, b - 1) + 1];      // the children's lists use low parts of at most b - 1 items
    const float* __restrict slf = &SLf[(size_t)d * nL]; float* __restrict slc = &SLf[(size_t)(d + 1) * nL];
    const double* __restrict sh = &SHd[(size_t)d * nH]; double* __restrict shc = &SHd[(size_t)(d + 1) * nH];
    for (int i = start; i < Xe; ++i) {
      if (m - (a + 1) > S.hY + (Xe - i - 1)) continue;     // not enough items left to finish
      const double* __restrict Ci = &S.C[(size_t)i * p];
      for (int j = i + 1; j < p; ++j) un[j] = u[j] + 2.0 * Ci[j];
      const float* __restrict cl = &S.CLf[(size_t)i * nL]; for (int q = 0; q < nl; ++q) slc[q] = slf[q] + cl[q];
      const double* __restrict ch = &S.CHd[(size_t)i * nH]; for (int q = 0; q < nH; ++q) shc[q] = sh[q] + ch[q];
      rtot[d + 1] = rtot[d] + S.cabs[i]; nstep[d + 1] = nstep[d] + 1;
      T[d + 1] = T[d] + S.t[i]; Q[d + 1] = Q[d] + u[i]; idx[d] = i;
      split_A(d + 1, i + 1, a + 1, m, thr);
    }
  }

  void walk_plain(int d0, int start0) {
    if (d0 == k - 1) { finish_single(d0, start0); return; }
    if (S.hY > 0 && start0 <= p - S.hY && k - d0 >= 3) {
      if (rY.empty()) { rY.assign(S.hY, 0.0); SL.assign(1u << S.hl, 0.0); Ff.assign(S.Lall.size(), 0.0f); HGL.assign(1u << S.hh, 0.0f); HGC.assign(1u << S.hh, 0.0f);
        SLf.assign((size_t)(k + 1) * S.Lall.size(), 0.0f); SHd.assign((size_t)(k + 1) << S.hh, 0.0); rtot.assign(k + 1, 0.0); nstep.assign(k + 1, 0); }
      double thr = threshold(); split_A(d0, start0, 0, k - d0, thr); return;
    }
    double thr = threshold();
    if (d0 == k - 2) { rows_plain(start0, thr); return; }
    const double* __restrict tt = S.t.data();
    const double* __restrict Cb = S.C.data();
    int* __restrict pos = idx.data();        // pos[d]: the item tried at depth d
    unsigned beat = 0;
    int d = d0; pos[d] = start0;
    for (;;) {
      const int i = pos[d];
      if (i > p - (k - d)) {                 // this depth is exhausted: back up
        if (d == d0) break;
        --d; ++pos[d]; continue;
      }
      const double* __restrict u = &U[(size_t)d * p];
      double* __restrict un = &U[(size_t)(d + 1) * p];
      const double* __restrict Ci = &Cb[(size_t)i * p];
      for (int j = i + 1; j < p; ++j) un[j] = u[j] + 2.0 * Ci[j];
      T[d + 1] = T[d] + tt[i]; Q[d + 1] = Q[d] + u[i];
      if (d + 1 == k - 2) {
        if ((++beat & 1023u) == 0) thr = threshold();
        rows_plain(i + 1, thr);
        ++pos[d];
      } else {
        ++d; pos[d] = i + 1;
      }
    }
  }

  // ---- Screened walk ----
  void walk(int d, int start) {           // branch of length d is set
    if (!S.screen_on) { walk_plain(d, start); return; }
    if (d == k - 2) {
      if (p - start < GATE_PAIRS) { pairs_plain(start); return; }
      const int sg = node_sign(d, 2, start);
      if (sg > 0) pairs<1>(start); else if (sg < 0) pairs<-1>(start); else pairs<0>(start);
      return;
    }
    if (d == k - 1) { finish_single(d, start); return; }
    if (S.binom[(size_t)(p - start) * (k + 1) + (k - d)] < GATE_NODE) { walk_plain(d, start); return; }
    const int r = k - d - 1, sg = node_sign(d, r + 1, start);
    if (sg > 0) { switch (r) { case 2: node<2, 1>(d, start); return; case 3: node<3, 1>(d, start); return; case 4: node<4, 1>(d, start); return;
                               case 5: node<5, 1>(d, start); return; case 6: node<6, 1>(d, start); return; case 7: node<7, 1>(d, start); return; default: node<0, 1>(d, start); return; } }
    if (sg < 0) { switch (r) { case 2: node<2, -1>(d, start); return; case 3: node<3, -1>(d, start); return; case 4: node<4, -1>(d, start); return; default: node<0, -1>(d, start); return; } }
    switch (r) { case 2: node<2, 0>(d, start); return; case 3: node<3, 0>(d, start); return; default: node<0, 0>(d, start); return; }
  }

  // The last two levels. Every pair's V = u1 + u2 + 2 C(j1, j2) lies in [Vlo, Vhi]; row j1 is dead if
  // |Tp + t1 + t2| < a + b (u1 + u2 + 2 LR1[j1]) for every later j2, i.e. if
  // Tp + t1 - a - b (u1 + 2 LR1[j1]) + max_{j2 > j1} (t2 - b u2) < 0 (and the same with -t). SG: sign mode
  template <int SG>
  void pairs(int start) {
    const int d = k - 2;
    const double Tp = T[d], Qp = Q[d];
    const double* __restrict u = &U[(size_t)d * p];
    const double* __restrict tt = S.t.data();
    double* __restrict sp = &SP[(size_t)d * p]; double* __restrict sn = &SN[(size_t)d * p];
    double thr = threshold();
    if (!(thr > 0)) { pairs_plain(start); return; }
    // the two smallest / largest per-item bounds on V (two running lists on long rows)
    const double* __restrict lg1 = &S.LG[p]; const double* __restrict hg1 = &S.HG[p];
    double mn0 = INFINITY, mn1 = INFINITY, mx0 = -INFINITY, mx1 = -INFINITY;
    { int j = start;
#ifdef RT_NEON
      if (p - start >= 8) {
        float64x2_t vn0 = vdupq_n_f64(INFINITY), vn1 = vn0, vx0 = vdupq_n_f64(-INFINITY), vx1 = vx0;
        for (; j + 4 <= p; j += 4) {
          const float64x2_t u0 = vld1q_f64(u + j), u1 = vld1q_f64(u + j + 2);
          vn0 = vminq_f64(vn0, vaddq_f64(u0, vld1q_f64(lg1 + j))); vn1 = vminq_f64(vn1, vaddq_f64(u1, vld1q_f64(lg1 + j + 2)));
          vx0 = vmaxq_f64(vx0, vaddq_f64(u0, vld1q_f64(hg1 + j))); vx1 = vmaxq_f64(vx1, vaddq_f64(u1, vld1q_f64(hg1 + j + 2)));
        }
        mn0 = vminvq_f64(vminq_f64(vn0, vn1)); mx0 = vmaxvq_f64(vmaxq_f64(vx0, vx1));
      }
#endif
      for (; j + 1 < p; j += 2) { mn0 = std::min(mn0, u[j] + lg1[j]); mx0 = std::max(mx0, u[j] + hg1[j]); mn1 = std::min(mn1, u[j + 1] + lg1[j + 1]); mx1 = std::max(mx1, u[j + 1] + hg1[j + 1]); }
      for (; j < p; ++j) { mn0 = std::min(mn0, u[j] + lg1[j]); mx0 = std::max(mx0, u[j] + hg1[j]); } }
    const double Vlo = 2.0 * std::min(mn0, mn1), Vhi = 2.0 * std::max(mx0, mx1);
    if (!(Qp + Vlo > 0)) { pairs_plain(start); return; }
    const double fl = std::sqrt(thr * (Qp + Vlo)), fh = std::sqrt(thr * (Qp + Vhi));
    const double bb = Vhi > Vlo ? (fh - fl) / (Vhi - Vlo) : 0.0, at = fl - bb * Vlo - 1e-9 * (fh + std::fabs(Tp));
    const double* __restrict LR1 = &S.LR[p];
#ifdef RT_NEON
    // Whole branch first: row j1 is live only if (t1 - b u1 - 2 b LR1) + max over later j2 of (t2 - b u2) reaches
    // at - Tp; bounding both terms by their maxima over every item gives one check for all rows, without the
    // serial pass below (most branches have no live row)
    if (p - start >= 8) {
      const float64x2_t vb = vdupq_n_f64(bb), v2b = vdupq_n_f64(2.0 * bb);
      float64x2_t wp = vdupq_n_f64(-INFINITY), xp = wp, wn = wp, xn = wp;
      int j = start;
      for (; j + 2 <= p; j += 2) {
        const float64x2_t uv = vld1q_f64(u + j), tv = vld1q_f64(tt + j), lr = vld1q_f64(LR1 + j);
        if (SG >= 0) { const float64x2_t x = vfmsq_f64(tv, vb, uv); xp = vmaxq_f64(xp, x); wp = vmaxq_f64(wp, vfmsq_f64(x, v2b, lr)); }
        if (SG <= 0) { const float64x2_t x = vfmsq_f64(vnegq_f64(tv), vb, uv); xn = vmaxq_f64(xn, x); wn = vmaxq_f64(wn, vfmsq_f64(x, v2b, lr)); }
      }
      double mwp = -INFINITY, mxp = -INFINITY, mwn = -INFINITY, mxn = -INFINITY;
      if (SG >= 0) { mwp = vmaxvq_f64(wp); mxp = vmaxvq_f64(xp); }
      if (SG <= 0) { mwn = vmaxvq_f64(wn); mxn = vmaxvq_f64(xn); }
      for (; j < p; ++j) {
        if (SG >= 0) { const double x = tt[j] - bb * u[j]; mxp = std::max(mxp, x); mwp = std::max(mwp, x - 2.0 * bb * LR1[j]); }
        if (SG <= 0) { const double x = -tt[j] - bb * u[j]; mxn = std::max(mxn, x); mwn = std::max(mwn, x - 2.0 * bb * LR1[j]); }
      }
      const double slack = 1e-12 * (std::fabs(at) + std::fabs(Tp));
      if ((SG < 0 || mwp + mxp < at - Tp - slack) && (SG > 0 || mwn + mxn < at + Tp - slack)) return;
    }
#endif
    // one backward pass: suffix maxima of t2 - b u2 (and -t2 - b u2) over later j2, and each row's flag
    unsigned char* __restrict lv = live.data();
    int any = 0;
    double mp = -INFINITY, mn = -INFINITY;
    for (int jj = p - 1; jj >= start; --jj) {
      const double h = at + bb * (u[jj] + 2.0 * LR1[jj]), Ta = Tp + tt[jj];
      int l = 0;
      if (SG >= 0) { sp[jj] = mp; l |= Ta + mp >= h; mp = std::max(mp, tt[jj] - bb * u[jj]); }
      if (SG <= 0) { sn[jj] = mn; l |= -Ta + mn >= h; mn = std::max(mn, -tt[jj] - bb * u[jj]); }
      lv[jj] = (unsigned char)l; any |= l;
    }
    lv[p - 1] = 0;                              // (no row: no later item)
    if (!any) return;
    for (int j1 = start; j1 <= p - 2; ++j1) {
      if (!lv[j1]) continue;
      const double Ta = Tp + tt[j1], h = at + bb * (u[j1] + 2.0 * LR1[j1]);
      row(j1, Ta, Qp + u[j1], u, thr, [&](int j2) { return (SG < 0 || Ta + sp[j2 - 1] < h) && (SG > 0 || -Ta + sn[j2 - 1] < h); });
    }
  }

  // A branch of length d <= k - 3 (m = k - d >= 3 items to add from [start, p)). Child i, then the
  // m - 1 items A' from (i, p):
  //  cheap test:  V >= u_i + 2 LR[m-1][i] + sum_A' (u_j + LG[m-2][j])           (O(1) per child)
  //  fused test:  V >= u_i + sum_A' (u_j + 2 C(i, j) + LG[m-2][j]), building the child's u on the way
  // and the child is dead if T + t_i - a - b (...) + the m - 1 largest (t_j - b (...)) < 0 (and with -t).
  // R: m - 1 when specialised (0 = any); SG: sign mode
  template <int R, int SG>
  void node(int d, int start) {
    const int m = k - d, r = R ? R : m - 1;
    constexpr int NA = R ? R + 1 : MAX_BOUND_ITEMS;   // array sizes: exact when specialised (kept in registers)
    const double* __restrict u = &U[(size_t)d * p];
    double* __restrict un = &U[(size_t)(d + 1) * p];
    const double* __restrict tt = S.t.data();
    const double* __restrict lgx = &S.LG[(size_t)(m - 2) * p];
    const double* __restrict lrm = &S.LR[(size_t)(m - 1) * p];
    const double thr = threshold();
    bool screen = false; double a = 0, bb = 0, tol = 0;
    int* __restrict kl = &kids[(size_t)d * p]; int nkid = 0;
    bool try_screen = thr > 0 && m <= MAX_BOUND_ITEMS && p - start > m;
    if (try_screen && off[m] && (++probe[m] & 63) != 0) try_screen = false;
    if (try_screen) {
      // interval of V: the m smallest / largest per-item bounds
      const double* __restrict lgm = &S.LG[(size_t)(m - 1) * p]; const double* __restrict hgm = &S.HG[(size_t)(m - 1) * p];
      const int mc = R ? R + 1 : m;                 // compile-time when specialised
      double lo[NA], hi[NA], lo2[NA], hi2[NA];       // (the scalar lists, when the vector ones aren't used)
      double Vlo_v = 0, Vhi_v = 0; bool have_v = false;
      if constexpr (R > 0) { neg_fill<NA>(lo); neg_fill<NA>(hi); neg_fill<NA>(lo2); neg_fill<NA>(hi2); }
      else for (int q = 0; q < mc; ++q) lo[q] = hi[q] = lo2[q] = hi2[q] = -INFINITY;
      int j = start;
#ifdef RT_NEON
      if constexpr (R > 0) if (p - start >= 8) {
        // two lanes x two lists per bound: four items a step
        float64x2_t L1[NA], L2[NA], H1[NA], H2[NA];
        vneg_fill<NA>(L1); vneg_fill<NA>(L2); vneg_fill<NA>(H1); vneg_fill<NA>(H2);
        for (; j + 4 <= p; j += 4) {
          const float64x2_t u0 = vld1q_f64(u + j), u1 = vld1q_f64(u + j + 2);
          vtop_insert<NA>(L1, vnegq_f64(vaddq_f64(u0, vld1q_f64(lgm + j)))); vtop_insert<NA>(L2, vnegq_f64(vaddq_f64(u1, vld1q_f64(lgm + j + 2))));
          vtop_insert<NA>(H1, vaddq_f64(u0, vld1q_f64(hgm + j))); vtop_insert<NA>(H2, vaddq_f64(u1, vld1q_f64(hgm + j + 2)));
        }
        for (; j < p; j += 2) {                    // the last one or two
          const int n2 = std::min(2, p - j); double xl[2], xh[2];
          for (int q = 0; q < n2; ++q) { xl[q] = -(u[j + q] + lgm[j + q]); xh[q] = u[j + q] + hgm[j + q]; }
          vtop_insert<NA>(L1, vpair(xl, n2)); vtop_insert<NA>(H1, vpair(xh, n2));
        }
        Vlo_v = -vtop_sum<NA>(L1, L2); Vhi_v = vtop_sum<NA>(H1, H2); have_v = true;
      }
#endif
      if (j == start) {
      if (p - start >= TWO_LISTS_FROM) for (; j + 1 < p; j += 2) {
        top_insert(lo, mc, -(u[j] + lgm[j])); top_insert(hi, mc, u[j] + hgm[j]);
        top_insert(lo2, mc, -(u[j + 1] + lgm[j + 1])); top_insert(hi2, mc, u[j + 1] + hgm[j + 1]);
      }
      for (; j < p; ++j) { top_insert(lo, mc, -(u[j] + lgm[j])); top_insert(hi, mc, u[j] + hgm[j]); }
      for (int q = 0; q < mc; ++q) { top_insert(lo, mc, lo2[q]); top_insert(hi, mc, hi2[q]); }
      }
      double Vlo = 0, Vhi = 0;
      if (have_v) { Vlo = Vlo_v; Vhi = Vhi_v; } else for (int q = 0; q < mc; ++q) { Vlo -= lo[q]; Vhi += hi[q]; }
      if (Q[d] + Vlo > 0) {
        const double fl = std::sqrt(thr * (Q[d] + Vlo)), fh = std::sqrt(thr * (Q[d] + Vhi));
        bb = Vhi > Vlo ? (fh - fl) / (Vhi - Vlo) : 0.0; a = fl - bb * Vlo;
        tol = 1e-9 * (fh + std::fabs(T[d]));
        // one backward pass: the r largest x_j = t_j - b (u_j + LG[m-2][j]) (and y_j with -t) over j > i, and
        // with them the cheap test of child i; the children that pass are listed (descending)
        double bx[NA], by[NA];
        if constexpr (R > 0) { neg_fill<R>(bx); neg_fill<R>(by); }
        else for (int q = 0; q < r; ++q) bx[q] = by[q] = -INFINITY;
#ifdef RT_NEON
        if constexpr (R > 0 && SG > 0) if (p - start >= 8) {
          // the values inserted (x_i) and each child's limit (base_i - T - t_i) first, for all i at once; the
          // serial pass then only keeps the running top-r and compares
          double* __restrict xv = &SP[(size_t)d * p]; double* __restrict cv = &SN[(size_t)d * p];
          const float64x2_t vb = vdupq_n_f64(bb), vlim = vdupq_n_f64(a - tol - T[d]), two = vdupq_n_f64(2.0);
          int i = start;
          for (; i + 2 <= p; i += 2) {
            const float64x2_t uv = vld1q_f64(u + i), tv = vld1q_f64(tt + i);
            vst1q_f64(xv + i, vfmsq_f64(tv, vb, vaddq_f64(uv, vld1q_f64(lgx + i))));
            vst1q_f64(cv + i, vsubq_f64(vfmaq_f64(vlim, vb, vfmaq_f64(uv, two, vld1q_f64(lrm + i))), tv));
          }
          for (; i < p; ++i) { xv[i] = tt[i] - bb * (u[i] + lgx[i]); cv[i] = (a - tol - T[d]) + bb * (u[i] + 2.0 * lrm[i]) - tt[i]; }
          for (i = p - 1; i > p - m; --i) top_insert(bx, r, xv[i]);
          for (; i >= start; --i) {
            double sx = 0; for (int q = 0; q < r; ++q) sx += bx[q];
            kl[nkid] = i; nkid += !(sx < cv[i]);
            top_insert(bx, r, xv[i]);
          }
          goto listed;
        }
#endif
        for (int i = p - 1; i >= start; --i) {
          if (i <= p - m) {                       // a child: at least r items follow it
            double sx = 0, sy = 0;
            for (int q = 0; q < r; ++q) { if (SG >= 0) sx += bx[q]; if (SG <= 0) sy += by[q]; }
            const double Ti = T[d] + tt[i], base = a + bb * (u[i] + 2.0 * lrm[i]) - tol;
            const bool kill = (SG < 0 || Ti + sx < base) && (SG > 0 || -Ti + sy < base);
            kl[nkid] = i; nkid += !kill;
          }
          const double ul = u[i] + lgx[i];
          if (SG >= 0) top_insert(bx, r, tt[i] - bb * ul);
          if (SG <= 0) top_insert(by, r, -tt[i] - bb * ul);
        }
#ifdef RT_NEON
        listed:
#endif
        screen = true;
        win_t[m] += p - m - start + 1; win_k[m] += p - m - start + 1 - nkid;
      }
    }
    const int nchild = p - m - start + 1;
    for (int kq = (screen ? nkid : nchild) - 1; kq >= 0; --kq) {
      const int i = screen ? kl[kq] : start + (nchild - 1 - kq);    // ascending either way
      const double Ti = T[d] + tt[i];
      const double* __restrict Ci = &S.C[(size_t)i * p];
      if (screen) {
        // fused: build the child's u and test it exactly up to the pairs inside A', with two running
        // top-r lists (even / odd j; with vector instructions, four) to keep the dependency chains short
        double ax[NA], ay[NA], cx[NA], cy[NA];
        if constexpr (R > 0) { neg_fill<R>(ax); neg_fill<R>(ay); neg_fill<R>(cx); neg_fill<R>(cy); }
        else for (int q = 0; q < r; ++q) ax[q] = ay[q] = cx[q] = cy[q] = -INFINITY;
        int j = i + 1;
        double sx = 0, sy = 0;
#ifdef RT_NEON
        if constexpr (R > 0) { if (p - i > 8) {
          // two lanes x two lists: four items a step
          float64x2_t A1[R], A2[R], B1[R], B2[R];
          if (SG >= 0) { vneg_fill<R>(A1); vneg_fill<R>(A2); }
          if (SG <= 0) { vneg_fill<R>(B1); vneg_fill<R>(B2); }
          const float64x2_t vb = vdupq_n_f64(bb), two = vdupq_n_f64(2.0);
          for (; j + 4 <= p; j += 4) {
            const float64x2_t x0 = vfmaq_f64(vld1q_f64(u + j), two, vld1q_f64(Ci + j)), x1 = vfmaq_f64(vld1q_f64(u + j + 2), two, vld1q_f64(Ci + j + 2));
            vst1q_f64(un + j, x0); vst1q_f64(un + j + 2, x1);
            const float64x2_t l0 = vaddq_f64(x0, vld1q_f64(lgx + j)), l1 = vaddq_f64(x1, vld1q_f64(lgx + j + 2));
            const float64x2_t t0 = vld1q_f64(tt + j), t1 = vld1q_f64(tt + j + 2);
            if (SG >= 0) { vtop_insert<R>(A1, vfmsq_f64(t0, vb, l0)); vtop_insert<R>(A2, vfmsq_f64(t1, vb, l1)); }
            if (SG <= 0) { vtop_insert<R>(B1, vfmsq_f64(vnegq_f64(t0), vb, l0)); vtop_insert<R>(B2, vfmsq_f64(vnegq_f64(t1), vb, l1)); }
          }
          for (; j < p; j += 2) {                  // the last one or two
            const int n2 = std::min(2, p - j); double xa[2], ya[2];
            for (int q = 0; q < n2; ++q) { const double x0 = u[j + q] + 2.0 * Ci[j + q]; un[j + q] = x0; const double l0 = x0 + lgx[j + q];
              xa[q] = tt[j + q] - bb * l0; ya[q] = -tt[j + q] - bb * l0; }
            if (SG >= 0) vtop_insert<R>(A1, vpair(xa, n2));
            if (SG <= 0) vtop_insert<R>(B1, vpair(ya, n2));
          }
          if (SG >= 0) sx = vtop_sum<R>(A1, A2);
          if (SG <= 0) sy = vtop_sum<R>(B1, B2);
        } else if (p - i > 2) {
          // short rows: one list of two lanes, two items a step
          float64x2_t A1[R], B1[R];
          if (SG >= 0) vneg_fill<R>(A1);
          if (SG <= 0) vneg_fill<R>(B1);
          const float64x2_t vb = vdupq_n_f64(bb), two = vdupq_n_f64(2.0);
          for (; j + 2 <= p; j += 2) {
            const float64x2_t x0 = vfmaq_f64(vld1q_f64(u + j), two, vld1q_f64(Ci + j));
            vst1q_f64(un + j, x0);
            const float64x2_t l0 = vaddq_f64(x0, vld1q_f64(lgx + j)), t0 = vld1q_f64(tt + j);
            if (SG >= 0) vtop_insert<R>(A1, vfmsq_f64(t0, vb, l0));
            if (SG <= 0) vtop_insert<R>(B1, vfmsq_f64(vnegq_f64(t0), vb, l0));
          }
          if (j < p) {
            const double x0 = u[j] + 2.0 * Ci[j]; un[j] = x0; const double l0 = x0 + lgx[j];
            const double xa = tt[j] - bb * l0, ya = -tt[j] - bb * l0;
            if (SG >= 0) vtop_insert<R>(A1, vpair(&xa, 1));
            if (SG <= 0) vtop_insert<R>(B1, vpair(&ya, 1));
            ++j;
          }
          if (SG >= 0) sx = vtop_sum1<R>(A1);
          if (SG <= 0) sy = vtop_sum1<R>(B1);
        } }
#endif
        if (j == i + 1) {
        if (p - i >= TWO_LISTS_FROM) for (; j + 1 < p; j += 2) {
          const double x0 = u[j] + 2.0 * Ci[j], x1 = u[j + 1] + 2.0 * Ci[j + 1];
          un[j] = x0; un[j + 1] = x1;
          const double l0 = x0 + lgx[j], l1 = x1 + lgx[j + 1];
          if (SG >= 0) { top_insert(ax, r, tt[j] - bb * l0); top_insert(cx, r, tt[j + 1] - bb * l1); }
          if (SG <= 0) { top_insert(ay, r, -tt[j] - bb * l0); top_insert(cy, r, -tt[j + 1] - bb * l1); }
        }
        for (; j < p; ++j) {
          const double x0 = u[j] + 2.0 * Ci[j]; un[j] = x0; const double l0 = x0 + lgx[j];
          if (SG >= 0) top_insert(ax, r, tt[j] - bb * l0);
          if (SG <= 0) top_insert(ay, r, -tt[j] - bb * l0);
        }
        if (SG >= 0) { for (int q = 0; q < r; ++q) top_insert(ax, r, cx[q]); for (int q = 0; q < r; ++q) sx += ax[q]; }
        if (SG <= 0) { for (int q = 0; q < r; ++q) top_insert(ay, r, cy[q]); for (int q = 0; q < r; ++q) sy += ay[q]; }
        }
        const double base2 = a + bb * u[i] - tol;
        if ((SG < 0 || Ti + sx < base2) && (SG > 0 || -Ti + sy < base2)) { ++win_k[m]; continue; }
      } else {
        for (int j = i + 1; j < p; ++j) un[j] = u[j] + 2.0 * Ci[j];
      }
      T[d + 1] = Ti; Q[d + 1] = Q[d] + u[i]; idx[d] = i;
      walk(d + 1, i + 1);
    }
    if (screen && win_t[m] >= 512) { off[m] = win_k[m] < 0.05 * win_t[m]; win_t[m] = win_k[m] = 0; }
  }

  void operator()(std::size_t begin, std::size_t end) {
    if (U.empty()) {
      U.assign((size_t)(k + 1) * p, 0.0); SP.assign((size_t)(k + 1) * p, 0.0); SN.assign((size_t)(k + 1) * p, 0.0);
      T.assign(k + 1, 0.0); Q.assign(k + 1, 0.0); idx.assign(k, 0); cand.assign(k, 0); live.assign(p, 0); kids.assign((size_t)(k + 1) * p, 0); EF.assign(p, 0.0f);
      std::copy(S.u0.begin(), S.u0.end(), U.begin());
      T[0] = S.T0; Q[0] = S.Q0;
    }
    std::vector<int> cur(k, -1);
    int cur_len = 0;
    for (std::size_t task = begin; task < end; ++task) {
      sh_seen = std::max(sh_seen, S.shared());
      const int* pre = task_items.data() + task_off[task];
      int len = task_off[task + 1] - task_off[task], start;
      if (len > 0 && pre[len - 1] < 0) { start = -pre[len - 1] - 1; --len; }   // a piece with its own start (see BranchGenerator)
      else start = len > 0 ? pre[len - 1] + 1 : 0;
      int same = 0;                          // consecutive tasks share leading items -- reuse them
      while (same < len && same < cur_len && cur[same] == pre[same]) ++same;
      for (int d = same; d < len; ++d) {
        const int i = pre[d]; idx[d] = i; cur[d] = i;
        const double* Ci = &S.C[(size_t)i * p]; const double* u = &U[(size_t)d * p]; double* un = &U[(size_t)(d + 1) * p];
        for (int j = i + 1; j < p; ++j) un[j] = u[j] + 2.0 * Ci[j];
        T[d + 1] = T[d] + S.t[i]; Q[d + 1] = Q[d] + u[i];
      }
      cur_len = len;
      walk(len, start);
    }
  }

  void join(const GramSearchWorker& o) {
    for (const HeapEntry& e : o.heap) offer_sorted(e.key, e.r, o.combo_of(e));
  }
};

// Splits the combination tree into branches (a fixed prefix of leading
// items plus everything below it) of at most `target` combinations, in
// lexicographic order. Only branches still above target are split deeper, so
// every shape -- e.g. 16 of 40, whose fixed-depth branches held up to 130M
// combinations and left most threads idle -- gets evenly small pieces.
// Resumable, so each batch generates only its own branches
struct BranchGenerator {
  const int p, k; const double target;
  std::vector<int> cur, nxt;
  int depth; bool done, root_pending;
  int y0;                                // the split walk's block Y starts here (p: none)
  BranchGenerator(int p, int k, double target)
    : p(p), k(k), target(target), cur(k, 0), nxt(k + 1, 0), depth(0), done(false),
      root_pending(k == 1 || calculate_combinations_double(p, k) <= target), y0(p) {}
  // With the split walk on, the choices from block Y onwards at a level make one piece: the prefix so far with
  // all its completions from [s, p), s >= y0 (pieces whose prefix ended inside Y would miss the split walk).
  // Such a piece ends with -(s + 1)
  void set_block(int y) { y0 = y; }
  // Appends the next branch prefix to `items` and returns its size (0 = none left)
  double next(std::vector<int>& items) {
    if (done) return 0;
    if (root_pending) { done = true; return calculate_combinations_double(p, k); }
    while (depth >= 0) {
      const int i = nxt[depth];
      if (i > p - (k - depth)) { --depth; continue; }     // level exhausted: backtrack
      if (i >= y0 && depth > 0) {
        nxt[depth] = p;                                   // the rest of this level: one piece
        items.insert(items.end(), cur.begin(), cur.begin() + depth); items.push_back(-(i + 1));
        return calculate_combinations_double(p - i, k - depth);
      }
      cur[depth] = i; nxt[depth] = i + 1;
      const double size = calculate_combinations_double(p - 1 - i, k - depth - 1);
      if (size <= target || depth + 1 == k - 1) {
        items.insert(items.end(), cur.begin(), cur.begin() + depth + 1);
        return size;
      }
      ++depth; nxt[depth] = i + 1;                         // still too big: split deeper
    }
    done = true;
    return 0;
  }
};

#endif
