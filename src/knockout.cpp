// [[Rcpp::depends(RcppParallel)]]

// ============================================================================
//  KNOCKOUT SEARCH
// ============================================================================
// For searches too large to run exhaustively: like knocking walls out of an
// old building to find the ones holding it up. Starting from the full scale,
// the item whose loss moves the centroid least is knocked out until n.items
// remain. Each item is then judged by how close a call it is: the r reachable
// by reversing its status (in -> out, or out -> in) with one swap, or with two
// swaps at once, which catches items whose value depends on balancing another.
// Every combination of as many closest calls as the budget allows is tested,
// the other items held fixed, and the search repeats around any better form.
// Once it settles, each candidate of the best form is knocked out in turn and
// the search runs again without it (Leave One Candidate Out, LOCO) -- or, for
// a long form, each left-out item is put back in turn. Everything works from centred moments: C (p x p item
// cross-products), t (item-target cross-products) and ssy.
//
// Larger budgets take bigger steps, and where several near-equal best forms lie far apart, searches at
// different budgets can settle on different ones -- so a larger budget could return a worse form. The
// sweepdown rules that out: the whole search runs again at a tenth of the budget, a hundredth, ... down to 1, and
// the best form of any of them is kept, so ten times the budget never returns a worse form. The smaller
// searches cost little: a smaller budget's pool around a form the larger searches already met is part of
// theirs, so its best form can usually be read off their lists instead of searched for (see SearchCache).

#include <Rcpp.h>
#include <RcppParallel.h>
#include <algorithm>
#include <vector>
#include <cmath>
#include <chrono>
#include <thread>
#include <set>
#include <numeric>
#include <string>
#include <sstream>
#include <iomanip>
#include <atomic>
#include <memory>
#include <mutex>
#include <condition_variable>
#include <map>
#include <tbb/parallel_for.h>
#include <tbb/parallel_reduce.h>
#include <tbb/blocked_range.h>

#include "gram_engine.h"
#include "parallel_guard.h"   // parallel loops that fall back to one thread in a fork()'d child

// The Rcpp and RcppParallel names this file uses unqualified; everything else is written in full (Rcpp::stop, ...)
using Rcpp::NumericVector;
using Rcpp::NumericMatrix;
using Rcpp::IntegerVector;
using Rcpp::IntegerMatrix;
using Rcpp::CharacterVector;
using Rcpp::List;
using Rcpp::DataFrame;
using Rcpp::Named;
using RcppParallel::Worker;
using RcppParallel::Split;
using RcppParallel::RVector;
using RcppParallel::RMatrix;


namespace {

const double NEG = -1.0;                       // key of an invalid (zero-variance) set; valid keys are >= 0
const double TOL2 = (1 + 1e-12) * (1 + 1e-12); // "strictly better", on the squared scale of key()
const int SWEEPDOWN_KEEP = 4;                  // forms kept by each search of the sweepdown (see knockout_cpp)

struct Mom {
  int p; std::vector<double> C, dC, t; double ssy;
  inline double c(int i, int j) const { return C[(size_t)j * p + i]; }
  inline const double* col(int j) const { return &C[(size_t)j * p]; }
};

// key = T^2 / Q: proportional to r^2, so it ranks sets by |r|
inline double key(double T, double Q) { return Q > 0 ? T * T / Q : NEG; }

// Running totals of a set S: a[j] = sum of C(j, s) over s in S, T and Q
struct State { std::vector<double> a; double T = 0, Q = 0; };
State state_of(const Mom& M, const std::vector<int>& S) {
  State s; s.a.assign(M.p, 0.0);
  for (int x : S) { const double* cx = M.col(x); for (int j = 0; j < M.p; ++j) s.a[j] += cx[j]; s.T += M.t[x]; }
  for (int x : S) s.Q += s.a[x];
  return s;
}
double set_key(const Mom& M, const std::vector<int>& S) { State s = state_of(M, S); return key(s.T, s.Q); }
std::vector<int> sorted(std::vector<int> S) { std::sort(S.begin(), S.end()); return S; }

// ---- Trim from the full scale: drop the item whose loss hurts r least ----
std::vector<int> trim(const Mom& M, int k) {
  const int p = M.p;
  std::vector<int> S(p); std::iota(S.begin(), S.end(), 0);
  State s = state_of(M, S);
  while ((int)S.size() > k) {
    int bq = 0; double bs = -INFINITY;
    for (int q = 0; q < (int)S.size(); ++q) {
      const int i = S[q]; const double Qn = s.Q - 2 * s.a[i] + M.dC[i];
      if (!(Qn > 0)) continue;
      const double v = (s.T - M.t[i]) / std::sqrt(Qn);
      if (v > bs) { bs = v; bq = q; }
    }
    const int i = S[bq];
    s.T -= M.t[i]; s.Q = s.Q - 2 * s.a[i] + M.dC[i];
    const double* ci = M.col(i); for (int j = 0; j < p; ++j) s.a[j] -= ci[j];
    S.erase(S.begin() + bq);
  }
  return S;
}

// ---- How close a call is each item? Best key reachable when it changes side ----
// 2-swaps (remove i1, i2; add j1, j2) in parallel over the pairs of members,
// each worker keeping only per-item maxima -- the k^2 (p - k)^2 / 4 table of
// 2-swap scores is never stored
struct PairSwapWorker : public Worker {
  const Mom& M; const std::vector<int>& i1; const std::vector<int>& i2;
  const std::vector<int>& j1; const std::vector<int>& j2;
  const std::vector<double>& addT; const std::vector<double>& addQ;
  const std::vector<double>& a; const double T, Q;
  std::vector<double> best;
  PairSwapWorker(const Mom& M, const std::vector<int>& i1, const std::vector<int>& i2, const std::vector<int>& j1, const std::vector<int>& j2,
                 const std::vector<double>& addT, const std::vector<double>& addQ, const std::vector<double>& a, double T, double Q)
    : M(M), i1(i1), i2(i2), j1(j1), j2(j2), addT(addT), addQ(addQ), a(a), T(T), Q(Q), best(M.p, NEG) {}
  PairSwapWorker(const PairSwapWorker& o, Split)
    : M(o.M), i1(o.i1), i2(o.i2), j1(o.j1), j2(o.j2), addT(o.addT), addQ(o.addQ), a(o.a), T(o.T), Q(o.Q), best(o.M.p, NEG) {}
  void operator()(std::size_t b, std::size_t e) {
    const size_t no = j1.size();
    const int* J1 = j1.data(); const int* J2 = j2.data(); const double* AT = addT.data(); const double* AQ = addQ.data();
    double* B = best.data();
    for (size_t q = b; q < e; ++q) {
      const int x1 = i1[q], x2 = i2[q];
      const double Tr = T - M.t[x1] - M.t[x2];
      const double Qr = Q - 2 * a[x1] - 2 * a[x2] + M.dC[x1] + M.dC[x2] + 2 * M.c(x1, x2);
      const double* C1 = M.col(x1); const double* C2 = M.col(x2);
      double rmax = NEG;
      for (size_t u = 0; u < no; ++u) {
        const int y1 = J1[u], y2 = J2[u];
        const double Tn = Tr + AT[u];
        const double Qn = Qr + AQ[u] - 2.0 * (C1[y1] + C2[y1] + C1[y2] + C2[y2]);
        const double kk = Qn > 0 ? Tn * Tn / Qn : NEG;
        if (kk > rmax) rmax = kk;
        if (kk > B[y1]) B[y1] = kk;
        if (kk > B[y2]) B[y2] = kk;
      }
      if (rmax > B[x1]) B[x1] = rmax;
      if (rmax > B[x2]) B[x2] = rmax;
    }
  }
  void join(const PairSwapWorker& o) { for (int j = 0; j < M.p; ++j) if (o.best[j] > best[j]) best[j] = o.best[j]; }
};

// cost[i] = |r|(S) - best |r| reachable when item i changes side (both up to sqrt(ssy)); +Inf if no valid move
std::vector<double> flip_costs(const Mom& M, const std::vector<int>& S) {
  const int p = M.p;
  std::vector<char> in(p, 0); for (int x : S) in[x] = 1;
  std::vector<int> O; for (int j = 0; j < p; ++j) if (!in[j]) O.push_back(j);
  State s = state_of(M, S);
  std::vector<double> best(p, NEG);
  for (int i : S) for (int j : O) {              // 1-swaps
    const double kk = key(s.T - M.t[i] + M.t[j], s.Q - 2 * s.a[i] + M.dC[i] + 2 * s.a[j] + M.dC[j] - 2 * M.c(i, j));
    if (kk > best[i]) best[i] = kk;
    if (kk > best[j]) best[j] = kk;
  }
  if (S.size() >= 2 && O.size() >= 2) {          // 2-swaps
    std::vector<int> i1, i2, j1, j2; std::vector<double> addT, addQ;
    for (size_t u = 0; u < S.size(); ++u) for (size_t v = u + 1; v < S.size(); ++v) { i1.push_back(S[u]); i2.push_back(S[v]); }
    for (size_t u = 0; u < O.size(); ++u) for (size_t v = u + 1; v < O.size(); ++v) {
      const int y1 = O[u], y2 = O[v]; j1.push_back(y1); j2.push_back(y2);
      addT.push_back(M.t[y1] + M.t[y2]);
      addQ.push_back(2 * s.a[y1] + M.dC[y1] + 2 * s.a[y2] + M.dC[y2] + 2 * M.c(y1, y2));
    }
    PairSwapWorker w(M, i1, i2, j1, j2, addT, addQ, s.a, s.T, s.Q);
    par_reduce(0, i1.size(), w, std::max<size_t>(1, 200000 / std::max<size_t>(1, j1.size())));
    for (int j = 0; j < p; ++j) if (w.best[j] > best[j]) best[j] = w.best[j];
  }
  const double s0 = std::sqrt(std::max(key(s.T, s.Q), 0.0));
  std::vector<double> cost(p);
  for (int j = 0; j < p; ++j) cost[j] = best[j] >= 0 ? s0 - std::sqrt(best[j]) : INFINITY;
  return cost;
}

// ---- Open the closest calls, in or out, until the next would exceed the budget ----
// The search then refills the open slots in every possible way: choose(|U|, j)
// combinations, j = the open items currently in the form
struct Pool { std::vector<int> F, U; int j = 0; };
Pool closest_pool(const std::vector<int>& S, const std::vector<double>& cost, double budget, int p) {
  std::vector<int> ord(p); std::iota(ord.begin(), ord.end(), 0);
  std::stable_sort(ord.begin(), ord.end(), [&](int a, int b) { return cost[a] < cost[b]; });
  std::vector<char> in(p, 0), open(p, 0); for (int x : S) in[x] = 1;
  Pool P;
  for (int i : ord) {
    const int nin = P.j + in[i];
    if (calculate_combinations_double((int)P.U.size() + 1, nin) > budget) break;
    P.U.push_back(i); P.j = nin; open[i] = 1;
  }
  for (int x : S) if (!open[x]) P.F.push_back(x);
  return P;
}

// ---- Progress display (main thread only) ----
// Compact rates ("1.7B") so a search's line fits an 80-column console
std::string compact_num(double x) {
  std::ostringstream o; o << std::fixed << std::setprecision(x >= 1e10 || (x < 1e9 && x >= 1e7) || (x < 1e6 && x >= 1e4) ? 0 : 1);
  if (x >= 1e9) o << x / 1e9 << "B"; else if (x >= 1e6) o << x / 1e6 << "M"; else if (x >= 1e3) o << x / 1e3 << "K"; else { o.str(""); o << (long long)x; }
  return o.str();
}
// The live progress line. It is redrawn only when its text changes (at most every MIN_UPDATE_INTERVAL_MS),
// not over and over unchanged; once it has stood still for 0.5 s, a '~' flashes (1 s on, 1 s off) at
// position `beat` -- the bar's first empty cell, -1 for none -- to show the search is still running
struct Progress {
  bool on; int width; double last_ms, changed_ms; std::string shown; bool beat_on;
  std::chrono::steady_clock::time_point t0;
  Progress(bool on, int width) : on(on), width(std::max(width, 20)), last_ms(-1e9), changed_ms(0), beat_on(false),
                                 t0(std::chrono::steady_clock::now()) {}
  void line(const std::string& s, bool force = false, int beat = -1) {
    if (!on) return;
    const double ms = std::chrono::duration<double, std::milli>(std::chrono::steady_clock::now() - t0).count();
    bool b = false;
    if (s == shown && !force) {
      const double quiet = ms - changed_ms;
      b = beat >= 0 && beat < (int)s.size() && quiet >= 500 && std::fmod(quiet - 500, 2000) < 1000;
      if (b == beat_on) return;                 // nothing new to show
    } else {
      if (!force && ms - last_ms < MIN_UPDATE_INTERVAL_MS) return;
      if (s != shown) changed_ms = ms;
      shown = s;
    }
    last_ms = ms; beat_on = b;
    std::string out = s; if (b) out[beat] = '~';
    if ((int)out.size() > width) out.resize(width);
    out.resize(width, ' ');
    Rcpp::Rcout << "\r" << out << std::flush;
  }
};

// ---- Exhaustive top-K over the pool: F always in, choose j of U ----
// Enumerates the smaller side (the j to keep, or the |U| - j to drop), as the
// exhaustive search does; the fixed part enters as starting totals
struct SearchOut { std::vector<std::vector<int>> sets; double combos = 0, secs = 0; };
SearchOut search_pool(const Mom& M, const Pool& P, int K, Progress& prog, const std::string& head) {
  SearchOut out; const int nU = P.U.size(), j = P.j;
  if (j == 0 || j == nU) {
    std::vector<int> S = P.F; if (j == nU) S.insert(S.end(), P.U.begin(), P.U.end());
    out.sets.push_back(sorted(S)); out.combos = 1; return out;
  }
  const int n_drop = nU - j; const bool drop = n_drop < j;
  const int kk = drop ? n_drop : j;
  std::vector<int> base = P.F; if (drop) base.insert(base.end(), P.U.begin(), P.U.end());
  State b = state_of(M, base);
  const double sign = drop ? -1.0 : 1.0;      // drop side: score (base minus the chosen items)
  std::vector<double> C((size_t)nU * nU), dC(nU), t(nU);
  for (int v = 0; v < nU; ++v) {
    const double* cv = M.col(P.U[v]);
    for (int u = 0; u < nU; ++u) C[(size_t)v * nU + u] = cv[P.U[u]];
    dC[v] = M.dC[P.U[v]] + 2 * sign * b.a[P.U[v]];
    t[v] = sign * M.t[P.U[v]];
  }

  const double n_combos = calculate_combinations_double(nU, kk); out.combos = n_combos;
  const int n_threads = std::max(1u, std::thread::hardware_concurrency());
  BranchGenerator gen(nU, kk, std::min(n_combos / (8.0 * n_threads), 5e6));
  const int MIN_BRANCHES_PER_BATCH = 4 * n_threads;
  std::vector<int> task_items, task_off;
  const int n_top = (int)std::min((double)K, n_combos);
  GramSearch search(C, dC, t, M.ssy, nU, kk, n_top, b.T, b.Q, SCREEN_RATIO_POOLS);
  if (search.hY > 0) gen.set_block(nU - search.hY);
  GramSearchWorker master(search, task_items, task_off);
  auto start = std::chrono::steady_clock::now();
  double batch_target = 2e7, done = 0; bool finished = false;
  while (!finished) {
    task_items.clear(); task_off.assign(1, 0); double batch = 0;
    while (batch < batch_target || (int)task_off.size() - 1 < MIN_BRANCHES_PER_BATCH) {
      double size = gen.next(task_items);
      if (size == 0) { finished = true; break; }
      task_off.push_back((int)task_items.size()); batch += size;
    }
    if (task_off.size() == 1) break;
    GramSearchWorker w(search, task_items, task_off);
    par_reduce(0, task_off.size() - 1, w, 1);
    master.join(w); done += batch;
    Rcpp::checkUserInterrupt();
    const double ms = std::max(std::chrono::duration<double, std::milli>(std::chrono::steady_clock::now() - start).count(), 1.0);
    batch_target = std::max(done / ms * TARGET_BATCH_MS, 1e5);
    if (prog.on) {
      const double frac = done / n_combos, speed = done / (ms / 1000.0);
      const int W = 15, filled = (int)(W * std::min(frac, 1.0));
      std::string bar = BAR_START; for (int i = 0; i < W; ++i) bar += (i < filled ? BAR_FILL : BAR_EMPTY); bar += BAR_END;
      std::ostringstream o; o << head << " ";
      const int beat = filled < W ? (int)o.tellp() + (int)std::string(BAR_START).size() + filled : -1;
      o << bar << " " << (int)(100 * std::min(frac, 1.0)) << "% | " << compact_num(speed)
        << "/s | ETA: " << format_time((n_combos - done) / speed);
      prog.line(o.str(), finished, beat);
    }
  }
  out.secs = std::chrono::duration<double>(std::chrono::steady_clock::now() - start).count();
  GramSearchWorker::HeapCmp cmp{&master};
  std::vector<GramSearchWorker::HeapEntry> kept = master.heap; std::sort(kept.begin(), kept.end(), cmp);
  for (auto& e : kept) {
    const int* c = master.combo_of(e); std::vector<int> S = P.F;
    if (drop) { std::vector<char> gone(nU, 0); for (int d = 0; d < kk; ++d) gone[c[d]] = 1; for (int u = 0; u < nU; ++u) if (!gone[u]) S.push_back(P.U[u]); }
    else for (int d = 0; d < kk; ++d) S.push_back(P.U[c[d]]);
    out.sets.push_back(sorted(S));
  }
  return out;
}

// Every form found, best first (stable: equal keys keep first-found order)
struct Found {
  std::vector<std::vector<int>> sets; std::vector<double> keys; std::set<std::vector<int>> seen;
  void add(const Mom& M, const std::vector<std::vector<int>>& L) {
    for (auto& S : L) if (seen.insert(S).second) { sets.push_back(S); keys.push_back(set_key(M, S)); }
    std::vector<int> o(sets.size()); std::iota(o.begin(), o.end(), 0);
    std::stable_sort(o.begin(), o.end(), [&](int a, int b) { return keys[a] > keys[b]; });
    std::vector<std::vector<int>> s2; std::vector<double> k2; s2.reserve(o.size()); k2.reserve(o.size());
    for (int i : o) { s2.push_back(std::move(sets[i])); k2.push_back(keys[i]); }
    sets.swap(s2); keys.swap(k2);
  }
};

// ---- Gram moments from the data, in parallel ----
// Missing values take their column's mean (search only; reported statistics
// are recomputed from the true data), and rows with a missing target are
// skipped. Rows are processed in blocks copied to row-major order, each worker
// accumulating its own p x p upper triangle
struct GramWorker : public Worker {
  const RMatrix<double> X; const RVector<double> y;
  const std::vector<double>& fill; const std::vector<double>& mu; const double ybar;
  const std::vector<int>& rows; const int p;
  std::vector<double> acc, tacc; double sacc;
  GramWorker(const NumericMatrix& X, const NumericVector& y, const std::vector<double>& fill, const std::vector<double>& mu,
             double ybar, const std::vector<int>& rows)
    : X(X), y(y), fill(fill), mu(mu), ybar(ybar), rows(rows), p(X.ncol()), acc((size_t)p * p, 0.0), tacc(p, 0.0), sacc(0) {}
  GramWorker(const GramWorker& o, Split)
    : X(o.X), y(o.y), fill(o.fill), mu(o.mu), ybar(o.ybar), rows(o.rows), p(o.p), acc((size_t)o.p * o.p, 0.0), tacc(o.p, 0.0), sacc(0) {}
  void operator()(std::size_t begin, std::size_t end) {
    const int BLOCK = 64;
    std::vector<double> B((size_t)BLOCK * p), yv(BLOCK);
    for (std::size_t r0 = begin; r0 < end; r0 += BLOCK) {
      const int nb = (int)std::min<std::size_t>(BLOCK, end - r0);
      for (int j = 0; j < p; ++j) for (int b = 0; b < nb; ++b) {
        const double v = X(rows[r0 + b], j);
        B[(size_t)b * p + j] = (std::isnan(v) ? fill[j] : v) - mu[j];
      }
      for (int b = 0; b < nb; ++b) yv[b] = y[rows[r0 + b]] - ybar;
      for (int b = 0; b < nb; ++b) {
        const double* v = &B[(size_t)b * p]; const double yb = yv[b];
        for (int i = 0; i < p; ++i) {
          const double vi = v[i]; double* a = &acc[(size_t)i * p];
          for (int j = i; j < p; ++j) a[j] += vi * v[j];
          tacc[i] += vi * yb;
        }
        sacc += yb * yb;
      }
    }
  }
  void join(const GramWorker& o) {
    for (size_t q = 0; q < acc.size(); ++q) acc[q] += o.acc[q];
    for (int j = 0; j < p; ++j) tacc[j] += o.tacc[j];
    sacc += o.sacc;
  }
};

Mom make_mom(const NumericMatrix& C, const NumericVector& tt, double ssy) {
  Mom M; M.p = C.nrow(); M.C.assign(C.begin(), C.end()); M.t.assign(tt.begin(), tt.end()); M.ssy = ssy;
  M.dC.resize(M.p); for (int j = 0; j < M.p; ++j) M.dC[j] = M.c(j, j);
  return M;
}

// Runs an RcppParallel-style worker (split constructor, operator()(begin, end), join) with TBB directly.
// RcppParallel's own wrappers create and destroy a global TBB setting on every call, which aborts when
// calls are nested inside other threads' parallel loops -- the plain TBB algorithms are made for nesting
template <class W> struct TbbBody {
  std::unique_ptr<W> own; W* w;
  explicit TbbBody(W& root) : w(&root) {}
  TbbBody(TbbBody& o, tbb::split) : own(new W(*o.w, RcppParallel::Split())), w(nullptr) { w = own.get(); }
  void operator()(const tbb::blocked_range<std::size_t>& r) { (*w)(r.begin(), r.end()); }
  void join(TbbBody& o) { w->join(*o.w); }
};
template <class W> void tbb_reduce(W& worker, std::size_t n, std::size_t grain) {
  if (forked_child()) { if (n > 0) worker(0, n); return; }   // see parallel_guard.h
  within_thread_limit([&] { TbbBody<W> body(worker); tbb::parallel_reduce(tbb::blocked_range<std::size_t>(0, n, std::max<std::size_t>(1, grain)), body); });
}

// ============================================================================
//  LOCO: THE SEARCHES
// ============================================================================
// Converge on the best form (closest calls, repeating from any better form),
// then run the same search again with each candidate of the best form left out
// in turn. Those searches are independent, so they run in parallel, each also
// splitting across threads when it is large.

// Closest-call costs with banned items (never swapped in), optionally scoring
// 2-swaps only among the `cand` items on each side whose best 1-swap comes
// closest (0 = all pairs, which grows as k^2 (p - k)^2)
std::vector<double> flip_costs_ex(const Mom& M, const std::vector<int>& S, const std::vector<char>& banned, const std::vector<char>& locked, int cand, bool parallel) {
  const int p = M.p;
  std::vector<char> in(p, 0); for (int x : S) in[x] = 1;
  std::vector<int> O; for (int j = 0; j < p; ++j) if (!in[j] && !(banned.size() && banned[j])) O.push_back(j);
  State s = state_of(M, S);
  std::vector<double> best(p, NEG);
  std::vector<int> Sf; for (int x : S) if (!(locked.size() && locked[x])) Sf.push_back(x);   // locked items never leave
  for (int i : Sf) for (int j : O) {
    const double kk = key(s.T - M.t[i] + M.t[j], s.Q - 2 * s.a[i] + M.dC[i] + 2 * s.a[j] + M.dC[j] - 2 * M.c(i, j));
    if (kk > best[i]) best[i] = kk;
    if (kk > best[j]) best[j] = kk;
  }
  if (Sf.size() >= 2 && O.size() >= 2) {
    std::vector<int> Sc = Sf, Oc = O;
    auto shortlist = [&](std::vector<int>& v) {
      if (cand <= 0 || (int)v.size() <= cand) return;
      std::stable_sort(v.begin(), v.end(), [&](int a, int b) { return best[a] > best[b]; }); v.resize(cand); };
    shortlist(Sc); shortlist(Oc);
    std::vector<int> i1, i2, j1, j2; std::vector<double> addT, addQ;
    for (size_t u = 0; u < Sc.size(); ++u) for (size_t v = u + 1; v < Sc.size(); ++v) { i1.push_back(Sc[u]); i2.push_back(Sc[v]); }
    for (size_t u = 0; u < Oc.size(); ++u) for (size_t v = u + 1; v < Oc.size(); ++v) {
      const int y1 = Oc[u], y2 = Oc[v]; j1.push_back(y1); j2.push_back(y2);
      addT.push_back(M.t[y1] + M.t[y2]);
      addQ.push_back(2 * s.a[y1] + M.dC[y1] + 2 * s.a[y2] + M.dC[y2] + 2 * M.c(y1, y2));
    }
    PairSwapWorker w(M, i1, i2, j1, j2, addT, addQ, s.a, s.T, s.Q);
    if (parallel) tbb_reduce(w, i1.size(), 200000 / std::max<size_t>(1, j1.size()));
    else w(0, i1.size());
    for (int j = 0; j < p; ++j) if (w.best[j] > best[j]) best[j] = w.best[j];
  }
  const double s0 = std::sqrt(std::max(key(s.T, s.Q), 0.0));
  std::vector<double> cost(p);
  for (int j = 0; j < p; ++j) cost[j] = (best[j] >= 0 && !(banned.size() && banned[j]) && !(locked.size() && locked[j])) ? s0 - std::sqrt(best[j]) : INFINITY;
  return cost;
}

Pool closest_pool_ex(const std::vector<int>& S, const std::vector<double>& cost, double budget, int p, const std::vector<char>& banned, const std::vector<char>& locked) {
  std::vector<int> ord(p); std::iota(ord.begin(), ord.end(), 0);
  std::stable_sort(ord.begin(), ord.end(), [&](int a, int b) { return cost[a] < cost[b]; });
  std::vector<char> in(p, 0), open(p, 0); for (int x : S) in[x] = 1;
  Pool P;
  for (int i : ord) {
    if ((banned.size() && banned[i]) || (locked.size() && locked[i])) continue;
    const int nin = P.j + in[i];
    if (calculate_combinations_double((int)P.U.size() + 1, nin) > budget) break;
    P.U.push_back(i); P.j = nin; open[i] = 1;
  }
  for (int x : S) if (!open[x]) P.F.push_back(x);
  return P;
}

// The sweepdown meets the same forms again and again, at every budget. Both caches are for one call and
// keyed by the form (sorted) and the item left out or put back
//
// Closest-call costs depend only on the form and that item, not on the budget
struct FlipCache {
  std::mutex mu; std::map<std::vector<int>, std::vector<double>> m;
  std::vector<double> get(const Mom& M, const std::vector<int>& key, const std::vector<int>& S, const std::vector<char>& banned,
                          const std::vector<char>& locked, int cand, bool parallel) {
    { std::lock_guard<std::mutex> g(mu); auto it = m.find(key); if (it != m.end()) return it->second; }
    std::vector<double> c = flip_costs_ex(M, S, banned, locked, cand, parallel);
    std::lock_guard<std::mutex> g(mu); m.emplace(key, c); return c;
  }
};
// Searches already run, each over every way of filling a pool's open slots around a fixed part. A smaller
// budget's search often covers only forms some larger search already covered -- its fixed part contains the larger
// one's, and every other item it may use is open there too (always so around the same form: the same closest
// calls, fewer of them; often around a nearby one). The best form it would find is then the best form of the
// larger search's list that it covers -- provided one is there, clearly ahead (by a relative 1e-9, far above
// rounding) of the list's cut-off and of every other form there that it covers: searching could only return that
// same form. Its next best forms there come too (up to K, those clearly above the cut-off). Otherwise the search runs.
// Only searches of larger budgets are read: they all finished before this budget's began, and are put in a fixed
// order first (order()), so what is read never depends on how the threads happened to run
struct SearchCache {
  struct Rec { double budget; std::vector<int> F, FU; std::vector<std::vector<int>> sets; std::vector<double> keys; double cut; };
  std::mutex mu; std::vector<Rec> recs;
  void store(const Mom& M, const Pool& P, const std::vector<std::vector<int>>& sets, int K, double budget) {
    Rec r; r.budget = budget; r.F = sorted(P.F); r.FU = r.F; r.FU.insert(r.FU.end(), P.U.begin(), P.U.end()); std::sort(r.FU.begin(), r.FU.end());
    std::vector<double> ks(sets.size()); for (size_t i = 0; i < sets.size(); ++i) ks[i] = set_key(M, sets[i]);
    std::vector<int> o(sets.size()); std::iota(o.begin(), o.end(), 0);
    std::stable_sort(o.begin(), o.end(), [&](int a, int b) { return ks[a] > ks[b]; });
    for (int i : o) { r.sets.push_back(sets[i]); r.keys.push_back(ks[i]); }
    r.cut = (int)r.sets.size() >= K ? r.keys.back() : -INFINITY;   // forms not kept score at most this (none missing below K)
    std::lock_guard<std::mutex> g(mu); recs.push_back(std::move(r));
  }
  void order() {
    std::sort(recs.begin(), recs.end(), [](const Rec& a, const Rec& b) {
      if (a.budget != b.budget) return a.budget > b.budget;
      if (a.F != b.F) return a.F < b.F;
      if (a.FU != b.FU) return a.FU < b.FU;
      return a.sets < b.sets; });
  }
  bool read_off(const Mom& M, const Pool& P, int K, double budget, std::vector<std::vector<int>>& got) {
    const std::vector<int> F = sorted(P.F); std::vector<int> FU = F; FU.insert(FU.end(), P.U.begin(), P.U.end()); std::sort(FU.begin(), FU.end());
    const int nF = P.F.size();
    std::vector<char> mark(M.p, 0); for (int x : P.F) mark[x] = 1; for (int x : P.U) mark[x] = 2;
    auto covered = [&](const std::vector<int>& x) { int f = 0; for (int v : x) { if (!mark[v]) return false; f += mark[v] == 1; } return f == nF; };
    std::lock_guard<std::mutex> g(mu);
    for (const Rec& r : recs) {
      if (!(r.budget > budget)) break;   // in order(): the larger budgets come first
      if (!std::includes(F.begin(), F.end(), r.F.begin(), r.F.end()) || !std::includes(r.FU.begin(), r.FU.end(), FU.begin(), FU.end())) continue;
      size_t i = 0; while (i < r.sets.size() && !covered(r.sets[i])) ++i;
      if (i == r.sets.size()) continue;
      const double lo = r.keys[i] * (1 - 1e-9);
      if (!(r.keys[i] > 0) || !(r.cut < lo)) continue;
      bool clear = true;
      for (size_t q = i + 1; q < r.sets.size() && r.keys[q] >= lo; ++q) if (covered(r.sets[q])) { clear = false; break; }
      if (!clear) continue;
      const double top_cut = r.cut * (1 + 1e-9);
      for (size_t q = i; q < r.sets.size() && (int)got.size() < K && r.keys[q] > top_cut; ++q) if (covered(r.sets[q])) got.push_back(r.sets[q]);
      return true;
    }
    return false;
  }
};

// Exhaustive top-K over the pool on the calling thread only (no R API, so safe inside a worker thread);
// stops early if `cancel` is set
SearchOut search_pool_serial(const Mom& M, const Pool& P, int K, const std::atomic<bool>* cancel) {
  SearchOut out; const int nU = P.U.size(), j = P.j;
  if (j == 0 || j == nU) {
    std::vector<int> S = P.F; if (j == nU) S.insert(S.end(), P.U.begin(), P.U.end());
    out.sets.push_back(sorted(S)); out.combos = 1; return out;
  }
  const int n_drop = nU - j; const bool drop = n_drop < j; const int kk = drop ? n_drop : j;
  std::vector<int> base = P.F; if (drop) base.insert(base.end(), P.U.begin(), P.U.end());
  State b = state_of(M, base); const double sign = drop ? -1.0 : 1.0;
  std::vector<double> C((size_t)nU * nU), dC(nU), t(nU);
  for (int v = 0; v < nU; ++v) {
    const double* cv = M.col(P.U[v]);
    for (int u = 0; u < nU; ++u) C[(size_t)v * nU + u] = cv[P.U[u]];
    dC[v] = M.dC[P.U[v]] + 2 * sign * b.a[P.U[v]];
    t[v] = sign * M.t[P.U[v]];
  }
  const double n_combos = calculate_combinations_double(nU, kk); out.combos = n_combos;
  const int n_top = (int)std::min((double)K, n_combos);
  std::vector<int> task_items, task_off;
  GramSearch search(C, dC, t, M.ssy, nU, kk, n_top, b.T, b.Q, SCREEN_RATIO_POOLS);
  GramSearchWorker w(search, task_items, task_off);
  BranchGenerator gen(nU, kk, 1e6);              // pieces of at most ~1M combinations, checking for cancellation between them
  if (search.hY > 0) gen.set_block(nU - search.hY);
  for (;;) {
    if (cancel && cancel->load(std::memory_order_relaxed)) break;
    task_items.clear(); task_off.assign(1, 0);
    if (gen.next(task_items) == 0) break;
    task_off.push_back((int)task_items.size());
    w(0, 1);
  }
  GramSearchWorker::HeapCmp cmp{&w};
  std::vector<GramSearchWorker::HeapEntry> kept = w.heap; std::sort(kept.begin(), kept.end(), cmp);
  for (auto& e : kept) {
    const int* c = w.combo_of(e); std::vector<int> S = P.F;
    if (drop) { std::vector<char> gone(nU, 0); for (int d = 0; d < kk; ++d) gone[c[d]] = 1; for (int u = 0; u < nU; ++u) if (!gone[u]) S.push_back(P.U[u]); }
    else for (int d = 0; d < kk; ++d) S.push_back(P.U[c[d]]);
    out.sets.push_back(sorted(S));
  }
  return out;
}

// Exhaustive top-K over the pool on the parallel engine without any R API calls (safe off the main thread,
// and nestable inside a parallel loop); small pools run on the calling thread
SearchOut search_pool_quiet(const Mom& M, const Pool& P, int K, const std::atomic<bool>* cancel) {
  const int nU = P.U.size(), j = P.j;
  if (j == 0 || j == nU) return search_pool_serial(M, P, K, cancel);
  const int n_drop = nU - j; const bool drop = n_drop < j; const int kk = drop ? n_drop : j;
  const double n_combos = calculate_combinations_double(nU, kk);
  if (n_combos < 2e5) return search_pool_serial(M, P, K, cancel);
  SearchOut out; out.combos = n_combos;
  std::vector<int> base = P.F; if (drop) base.insert(base.end(), P.U.begin(), P.U.end());
  State b = state_of(M, base); const double sign = drop ? -1.0 : 1.0;
  std::vector<double> C((size_t)nU * nU), dC(nU), t(nU);
  for (int v = 0; v < nU; ++v) {
    const double* cv = M.col(P.U[v]);
    for (int u = 0; u < nU; ++u) C[(size_t)v * nU + u] = cv[P.U[u]];
    dC[v] = M.dC[P.U[v]] + 2 * sign * b.a[P.U[v]];
    t[v] = sign * M.t[P.U[v]];
  }
  const int n_threads = std::max(1u, std::thread::hardware_concurrency());
  BranchGenerator gen(nU, kk, std::min(n_combos / (8.0 * n_threads), 2e6));
  const int n_top = (int)std::min((double)K, n_combos);
  std::vector<int> task_items, task_off;
  GramSearch search(C, dC, t, M.ssy, nU, kk, n_top, b.T, b.Q, SCREEN_RATIO_POOLS);
  if (search.hY > 0) gen.set_block(nU - search.hY);
  GramSearchWorker master(search, task_items, task_off);
  bool finished = false;
  while (!finished) {
    if (cancel && cancel->load(std::memory_order_relaxed)) break;
    task_items.clear(); task_off.assign(1, 0); double batch = 0;
    while (batch < 5e7 || (int)task_off.size() - 1 < 4 * n_threads) {
      double size = gen.next(task_items);
      if (size == 0) { finished = true; break; }
      task_off.push_back((int)task_items.size()); batch += size;
    }
    if (task_off.size() == 1) break;
    GramSearchWorker w(search, task_items, task_off);
    tbb_reduce(w, task_off.size() - 1, 1);
    master.join(w);
  }
  GramSearchWorker::HeapCmp cmp{&master};
  std::vector<GramSearchWorker::HeapEntry> kept = master.heap; std::sort(kept.begin(), kept.end(), cmp);
  for (auto& e : kept) {
    const int* c = master.combo_of(e); std::vector<int> S = P.F;
    if (drop) { std::vector<char> gone(nU, 0); for (int d = 0; d < kk; ++d) gone[c[d]] = 1; for (int u = 0; u < nU; ++u) if (!gone[u]) S.push_back(P.U[u]); }
    else for (int d = 0; d < kk; ++d) S.push_back(P.U[c[d]]);
    out.sets.push_back(sorted(S));
  }
  return out;
}

// The best form without `out`: drop it and add back the single best item that isn't banned
std::vector<int> refill_without(const Mom& M, const std::vector<int>& best, int out, const std::vector<char>& banned) {
  std::vector<int> S; for (int x : best) if (x != out) S.push_back(x);
  State s = state_of(M, S); std::vector<char> in(M.p, 0); for (int x : S) in[x] = 1;
  double bk = -INFINITY; int bj = -1;
  for (int j = 0; j < M.p; ++j) if (!in[j] && !banned[j]) { const double kk = key(s.T + M.t[j], s.Q + 2 * s.a[j] + M.dC[j]); if (kk > bk) { bk = kk; bj = j; } }
  if (bj >= 0) S.push_back(bj);
  return sorted(S);
}
// The best form with `in` forced in: add it and drop the single least useful item other than it
std::vector<int> retrim_with(const Mom& M, const std::vector<int>& best, int in) {
  std::vector<int> S = best; S.push_back(in);
  State s = state_of(M, S); double bk = -INFINITY; int bq = -1;
  for (int q = 0; q < (int)S.size(); ++q) { const int x = S[q]; if (x == in) continue;
    const double kk = key(s.T - M.t[x], s.Q - 2 * s.a[x] + M.dC[x]); if (kk > bk) { bk = kk; bq = q; } }
  if (bq >= 0) S.erase(S.begin() + bq);
  return sorted(S);
}

struct SearchLog { int changed, open, open_in, kept; double combos, r_centre, r_best, secs; };
struct JobOut { std::vector<std::vector<int>> sets; std::vector<double> keys; std::vector<SearchLog> log; bool failed = false; };

// One search: closest calls around the start, repeating from any better form. `out` is banned from every form
// (LOCO), `in` is forced into every form (LOCO on a long form: put back), both -1 for the main search.
// Thread-safe: no R API unless inner == 2 (the main thread's parallel engine with progress)
// With the caches (fc, sc), a search of a smaller budget in the sweepdown (read_off) first tries reading its best form
// off a larger search's list
JobOut loco_job(const Mom& M, const std::vector<int>& best, int out, int in, double budget, int K, int cand, int inner,
               const std::atomic<bool>* cancel, Progress* prog = nullptr, const std::string& head = "",
               FlipCache* fc = nullptr, SearchCache* sc = nullptr, bool read_off = false) {
  const bool parallel = inner > 0;
  JobOut res; std::vector<char> ban(M.p, 0), lock(M.p, 0); if (out >= 0) ban[out] = 1; if (in >= 0) lock[in] = 1;
  std::vector<int> S = out >= 0 ? refill_without(M, best, out, ban) : in >= 0 ? retrim_with(M, best, in) : best;
  Found f; f.add(M, {S}); const double rs = std::sqrt(M.ssy);
  for (int rd = 0; rd < 50; ++rd) {
    if (cancel && cancel->load(std::memory_order_relaxed)) break;
    auto t1 = std::chrono::steady_clock::now();
    std::vector<int> ck = sorted(S); ck.push_back(-1); ck.push_back(out); ck.push_back(in);
    const Pool P = closest_pool_ex(S, fc ? fc->get(M, ck, S, ban, lock, cand, parallel) : flip_costs_ex(M, S, ban, lock, cand, parallel), budget, M.p, ban, lock);
    const double kS = set_key(M, S);
    std::ostringstream h; if (prog) h << head << " search " << rd + 1;
    SearchOut so;
    if (!(sc && read_off && sc->read_off(M, P, K, budget, so.sets))) {
      so = inner == 2 ? search_pool(M, P, K, *prog, h.str()) : inner == 1 ? search_pool_quiet(M, P, K, cancel) : search_pool_serial(M, P, K, cancel);
      if (sc) sc->store(M, P, so.sets, K, budget);
    }
    f.add(M, so.sets);
    res.log.push_back({out >= 0 ? out : in, (int)P.U.size(), P.j, (int)P.F.size(), so.combos, std::sqrt(std::max(kS, 0.0)) / rs,
                       std::sqrt(std::max(f.keys[0], 0.0)) / rs, std::chrono::duration<double>(std::chrono::steady_clock::now() - t1).count()});
    if (f.sets[0] == S || !(f.keys[0] > kS * TOL2)) break;   // nothing better: settled
    S = f.sets[0];
  }
  res.sets = f.sets; res.keys = f.keys;
  return res;
}

} // namespace

// Centred moments of the (mean-imputed) data and target over rows with a
// non-missing target: C = item cross-products, tt = item-target
// cross-products, ssy = target sum of squares
// [[Rcpp::export]]
List gram_moments_cpp(NumericMatrix data, NumericVector target) {
  const int n = data.nrow(), p = data.ncol();
  std::vector<int> rows; for (int i = 0; i < n; ++i) if (!std::isnan(target[i])) rows.push_back(i);
  const int nv = rows.size();
  std::vector<double> fill(p, 0.0), mu(p, 0.0);
  for (int j = 0; j < p; ++j) {                // imputation means over all rows, then centring means over valid rows
    long double s = 0; int c = 0;
    for (int i = 0; i < n; ++i) { const double v = data(i, j); if (!std::isnan(v)) { s += v; ++c; } }
    fill[j] = c > 0 ? (double)(s / c) : 0.0;
    long double m = 0; for (int r : rows) { const double v = data(r, j); m += std::isnan(v) ? fill[j] : v; }
    mu[j] = nv > 0 ? (double)(m / nv) : 0.0;
  }
  long double ys = 0; for (int r : rows) ys += target[r];
  const double ybar = nv > 0 ? (double)(ys / nv) : 0.0;
  GramWorker w(data, target, fill, mu, ybar, rows);
  const int n_threads = std::max(1u, std::thread::hardware_concurrency());
  par_reduce(0, rows.size(), w, std::max<std::size_t>(256, rows.size() / (4 * n_threads) + 1));
  NumericMatrix C(p, p);
  for (int i = 0; i < p; ++i) for (int j = i; j < p; ++j) { C(i, j) = w.acc[(size_t)i * p + j]; C(j, i) = C(i, j); }
  return List::create(Named("C") = C, Named("tt") = NumericVector(w.tacc.begin(), w.tacc.end()),
                      Named("ssy") = w.sacc, Named("n_valid") = nv);
}

namespace {

// One knockout search at one budget: trim, converge on the best form, then LOCO
struct KnockoutRun { JobOut main; std::vector<JobOut> res; std::vector<int> items; bool add = false; double secs_main = 0, secs_loco = 0; };
KnockoutRun run_knockout(const Mom& M, int k, double budget, int keep_top, int cand, int mode, int mirror_min, Progress& prog,
                         FlipCache* fc, SearchCache* sc, bool read_off) {
  const int p = M.p;
  auto t0 = std::chrono::steady_clock::now();
  auto secs_since = [](std::chrono::steady_clock::time_point a) { return std::chrono::duration<double>(std::chrono::steady_clock::now() - a).count(); };

  // 1. Trim, then converge: closest calls around the best form, repeating from any better form
  JobOut main = loco_job(M, trim(M, k), -1, -1, budget, keep_top, cand, 2, nullptr, &prog, "~{ knockout }~", fc, sc, read_off);
  const std::vector<int> best = main.sets[0];
  const double secs_main = secs_since(t0);

  // 2. LOCO: the same search with each item of the best form left out (or, for long forms, each left-out item put back)
  const bool add = k > p / 2.0 && p - k >= mirror_min;
  std::vector<int> items;
  if (add) { std::vector<char> in(p, 0); for (int x : best) in[x] = 1; for (int j = 0; j < p; ++j) if (!in[j]) items.push_back(j); }
  else items = best;
  const int jobs = items.size();
  std::vector<JobOut> res(jobs);
  std::unique_ptr<std::atomic<int>[]> state(new std::atomic<int>[jobs]); for (int q = 0; q < jobs; ++q) state[q].store(0);
  auto t1 = std::chrono::steady_clock::now();
  auto run_job = [&](int q, int inner, const std::atomic<bool>* cancel, Progress* pr, const std::string& head) {
    state[q].store(1);
    res[q] = add ? loco_job(M, best, -1, items[q], budget, keep_top, cand, inner, cancel, pr, head, fc, sc, read_off)
                 : loco_job(M, best, items[q], -1, budget, keep_top, cand, inner, cancel, pr, head, fc, sc, read_off);
    state[q].store(2); };
  if (mode != 2 && jobs > 1 && !forked_child()) {   // (in a fork()'d child: one after another, see parallel_guard.h)
    // In parallel: TBB spreads the searches over the threads, each also splitting across threads when large;
    // the main thread shows how many are done and takes interrupts
    std::atomic<int> done(0); std::atomic<bool> cancel(false), finished(false);
    std::mutex wake_mu; std::condition_variable wake;
    std::thread driver([&]() {
      try { within_thread_limit([&] { tbb::parallel_for(tbb::blocked_range<std::size_t>(0, jobs, 1), [&](const tbb::blocked_range<std::size_t>& r) {
              for (std::size_t q = r.begin(); q < r.end(); ++q) { if (cancel.load()) break;
                try { run_job(q, mode == 0 ? 0 : 1, &cancel, nullptr, ""); } catch (...) { res[q].failed = true; }
                done.fetch_add(1); } }); }); }
      catch (...) { cancel.store(true); }
      { std::lock_guard<std::mutex> g(wake_mu); finished.store(true); }
      wake.notify_all(); });
    try {
      for (;;) {
        // every 5 ms, or as soon as the searches end
        { std::unique_lock<std::mutex> lk(wake_mu); wake.wait_for(lk, std::chrono::milliseconds(5), [&] { return finished.load(); }); }
        if (finished.load()) break;
        Rcpp::checkUserInterrupt();
        if (prog.on) {
          // Candidates done, and the time left at the rate so far (searches still running count as half done)
          const int d = done.load(); int running = 0; for (int q = 0; q < jobs; ++q) if (state[q].load() == 1) ++running;
          const double frac = (double)d / jobs, part = d + 0.5 * running;
          const int W = 15, filled = (int)(W * std::min(frac, 1.0));
          std::string bar = BAR_START; for (int i = 0; i < W; ++i) bar += (i < filled ? BAR_FILL : BAR_EMPTY); bar += BAR_END;
          std::ostringstream o; o << "~{ LOCO }~ " << d << "/" << jobs << " ";
          const int beat = filled < W ? (int)o.tellp() + (int)std::string(BAR_START).size() + filled : -1;
          o << bar << " " << (int)(100 * std::min(frac, 1.0)) << "% | ETA: "
            << (d > 0 ? format_time(secs_since(t1) / part * (jobs - part)) : std::string("-"));
          prog.line(o.str(), false, beat);
        }
      }
    } catch (...) { cancel.store(true); driver.join(); throw; }
    driver.join();
    for (int q = 0; q < jobs; ++q) if (state[q].load() != 2) res[q].failed = true;
  } else {
    // Few searches (short forms): one after another, each on every thread, with the search's own progress bar
    for (int q = 0; q < jobs; ++q) {
      std::ostringstream h; h << "~{ LOCO }~ " << q + 1 << "/" << jobs << ",";
      run_job(q, 2, nullptr, &prog, h.str());
    }
  }
  for (const JobOut& r : res) if (r.failed) Rcpp::stop("A LOCO search failed.");
  const double secs_loco = secs_since(t1);

  KnockoutRun out; out.main = std::move(main); out.res = std::move(res); out.items = items; out.add = add;
  out.secs_main = secs_main; out.secs_loco = secs_loco;
  return out;
}

} // namespace

// Knockout search. Converges on the best form, then searches again with each of its items knocked out in turn
// (LOCO) -- or, for a long form (more than half the pool kept, and at least mirror_min items
// left out), with each left-out item put back in turn: a long form is really a choice of what to leave out, and
// that list is shorter. Then (sweepdown) the same at a tenth of the budget, a hundredth, ... down to 1, keeping the
// best few forms of each search. Returns the best keep_top forms found (1-based column indices, best first, with
// their |r| from the moments), one row per search, one row per candidate (the best r found with it left out or put
// back, at the full budget), the pool around the best form (for the conservative mode's exact pass), and the
// seconds per phase
// [[Rcpp::export]]
List knockout_cpp(NumericMatrix C, NumericVector tt, double ssy, int n_items, double budget,
                  int keep_top = 100, bool show_progress = true, int width = 79, int cand = 40, int mode = 1,
                  int mirror_min = 20, bool sweepdown = true) {
  Mom M = make_mom(C, tt, ssy);
  const int k = n_items;
  if (k < 1 || k >= M.p) Rcpp::stop("n_items must be between 1 and the number of items - 1.");
  if (!(ssy > 0)) Rcpp::stop("The target has no variance.");
  Progress prog(show_progress, width);
  auto secs_since = [](std::chrono::steady_clock::time_point a) { return std::chrono::duration<double>(std::chrono::steady_clock::now() - a).count(); };
  FlipCache fc; SearchCache sc;

  // 1-2. The search at the budget
  KnockoutRun top = run_knockout(M, k, budget, keep_top, cand, mode, mirror_min, prog, &fc, &sc, false);

  // 3. The sweepdown, largest budget first, so each reads what it can off the searches before it. Only each
  // search's best form steers it (and only the overall best matters here), so these keep just a few
  auto t2 = std::chrono::steady_clock::now();
  std::vector<double> smaller;
  if (sweepdown) for (int j = 1; budget / std::pow(10.0, j) >= 1 - 1e-9; ++j) smaller.push_back(budget / std::pow(10.0, j));
  std::vector<KnockoutRun> runs;
  for (size_t q = 0; q < smaller.size(); ++q) {
    if (prog.on) { std::ostringstream o; o << "~{ sweepdown }~ " << q + 1 << "/" << smaller.size() << ": " << compact_num(smaller[q]); prog.line(o.str(), true); }
    Progress quiet(false, width);
    sc.order();
    runs.push_back(run_knockout(M, k, smaller[q], SWEEPDOWN_KEEP, cand, mode, mirror_min, quiet, &fc, &sc, true));
    Rcpp::checkUserInterrupt();
  }
  const double secs_sweepdown = secs_since(t2);

  // 4. Every form found, best first: one sort over all of them, in the order found (ties keep that order)
  std::vector<std::vector<int>> every(top.main.sets);
  auto take = [&](const KnockoutRun& R) { if (&R != &top) every.insert(every.end(), R.main.sets.begin(), R.main.sets.end());
                                          for (const JobOut& r : R.res) every.insert(every.end(), r.sets.begin(), r.sets.end()); };
  take(top); for (const KnockoutRun& R : runs) take(R);
  Found all; all.add(M, every);
  const Pool P = closest_pool(all.sets[0], flip_costs(M, all.sets[0]), budget, M.p);
  if (show_progress) Rcpp::Rcout << "\r" << std::string(std::max(width, 20), ' ') << "\r" << std::flush;

  const int n_out = std::min((int)all.sets.size(), keep_top);
  IntegerMatrix sets(n_out, k); NumericVector r(n_out);
  for (int i = 0; i < n_out; ++i) { for (int d = 0; d < k; ++d) sets(i, d) = all.sets[i][d] + 1; r[i] = std::sqrt(std::max(all.keys[i], 0.0) / M.ssy); }
  std::vector<int> ph, ch, op, oi, kp; std::vector<double> cb, rc, rb, sc_, bg; double searched = 0;
  auto put = [&](const std::vector<SearchLog>& L, int phase, double b) { for (const SearchLog& e : L) { ph.push_back(phase); ch.push_back(e.changed >= 0 ? e.changed + 1 : NA_INTEGER);
      op.push_back(e.open); oi.push_back(e.open_in); kp.push_back(e.kept); cb.push_back(e.combos); rc.push_back(e.r_centre); rb.push_back(e.r_best); sc_.push_back(e.secs); bg.push_back(b); searched += e.combos; } };
  put(top.main.log, 1, budget); for (const JobOut& x : top.res) put(x.log, 2, budget);
  for (size_t q = 0; q < runs.size(); ++q) { put(runs[q].main.log, 1, smaller[q]); for (const JobOut& x : runs[q].res) put(x.log, 2, smaller[q]); }
  std::vector<int> round(ph.size()); std::iota(round.begin(), round.end(), 1);
  CharacterVector phase(ph.size()); for (size_t i = 0; i < ph.size(); ++i) phase[i] = ph[i] == 1 ? "converge" : "LOCO";
  const int jobs = top.items.size();
  IntegerVector li(jobs), ls(jobs); NumericVector lr(jobs);
  for (int q = 0; q < jobs; ++q) { li[q] = top.items[q] + 1; ls[q] = top.res[q].log.size(); lr[q] = top.res[q].keys.empty() ? NA_REAL : std::sqrt(std::max(top.res[q].keys[0], 0.0) / M.ssy); }
  auto plus1 = [](std::vector<int> v) { std::sort(v.begin(), v.end()); for (int& x : v) ++x; return IntegerVector(v.begin(), v.end()); };
  DataFrame rounds = DataFrame::create(Named("search") = round, Named("phase") = phase, Named("budget") = bg, Named("changed") = ch, Named("open") = op, Named("open_in") = oi,
                                       Named("kept") = kp, Named("combinations") = cb, Named("r_centre") = rc, Named("r_best") = rb, Named("seconds") = sc_);
  DataFrame items_df = DataFrame::create(Named("item") = li, Named("change") = top.add ? "put back" : "left out", Named("r_best") = lr, Named("searches") = ls);
  return List::create(Named("sets") = sets, Named("r") = r, Named("rounds") = rounds, Named("items") = items_df, Named("shaken") = false, Named("searched") = searched,
                      Named("pool_kept") = plus1(P.F), Named("pool_open") = plus1(P.U), Named("pool_choose") = P.j,
                      Named("secs") = NumericVector::create(Named("converge") = top.secs_main, Named("loco") = top.secs_loco, Named("sweepdown") = secs_sweepdown));
}
