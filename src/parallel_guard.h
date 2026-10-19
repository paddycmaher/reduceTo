// How the parallel loops run: on TBB directly, safely in a fork()'d child, and within the user's thread limit.
//
// The loops take RcppParallel-style workers (a split constructor, operator()(begin, end) and, to reduce, join)
// and run them with TBB's own algorithms, compiled here, rather than through RcppParallel::parallelFor() and
// parallelReduce(). From RcppParallel 6.0 those hand the work to code compiled into RcppParallel itself,
// parallelReduce() through std::function -- which crashes wherever RcppParallel and this package were built
// against different C++ standard libraries (libstdc++ and libc++: R-hub's clang-asan, for one). TBB's
// algorithms also nest, as the knockout's loops do.
//
// TBB can't be used after fork(): once the parent has run a search, the first parallel loop in a child of
// parallel::mclapply() (or anything else that forks) crashes it. In a child, the loops run on the calling
// thread instead -- the fork has already shared the cores out among the children. (RcppParallel >= 6.1.0
// offers isProcessForkedChild() for this; the same test is kept here so older versions work too.)
//
// RcppParallel::setThreadOptions(numThreads = n) limits the loops to n threads: they run in an arena of n
// threads, and loops nested inside run in that arena too. Unlimited by default
#ifndef REDUCETO_PARALLEL_GUARD_H
#define REDUCETO_PARALLEL_GUARD_H

#include <RcppParallel.h>
#include <tbb/blocked_range.h>
#include <tbb/parallel_for.h>
#include <tbb/parallel_reduce.h>
#include <tbb/task_arena.h>
#include <algorithm>
#include <cstddef>
#include <memory>
#ifndef _WIN32
#include <unistd.h>
#endif

namespace {

#ifdef _WIN32
inline bool forked_child() { return false; }        // no fork() on Windows
#else
const pid_t rt_load_pid = getpid();                 // the process this library was loaded into
inline bool forked_child() { return getpid() != rt_load_pid; }
#endif

// RcppParallel::setThreadOptions()'s limit (-1: none)
inline int thread_limit() { return RcppParallel::resolveValue("RCPP_PARALLEL_NUM_THREADS", -1, -1); }
template <class F> inline void within_thread_limit(F&& f) {
  const int n = thread_limit();
  if (n > 0 && tbb::this_task_arena::max_concurrency() > n) { tbb::task_arena arena(n); arena.execute(f); }
  else f();
}

// An RcppParallel-style reducing worker as a TBB body
template <class W> struct TbbBody {
  std::unique_ptr<W> own; W* w;
  explicit TbbBody(W& root) : w(&root) {}
  TbbBody(TbbBody& o, tbb::split) : own(new W(*o.w, RcppParallel::Split())), w(nullptr) { w = own.get(); }
  void operator()(const tbb::blocked_range<std::size_t>& r) { (*w)(r.begin(), r.end()); }
  void join(TbbBody& o) { w->join(*o.w); }
};

template <class W> inline void par_for(std::size_t begin, std::size_t end, W& worker, std::size_t grain = 1) {
  if (begin >= end) return;
  if (forked_child()) { worker(begin, end); return; }
  within_thread_limit([&] {
    tbb::parallel_for(tbb::blocked_range<std::size_t>(begin, end, std::max<std::size_t>(1, grain)),
                      [&](const tbb::blocked_range<std::size_t>& r) { worker(r.begin(), r.end()); }); });
}
template <class W> inline void par_reduce(std::size_t begin, std::size_t end, W& worker, std::size_t grain = 1) {
  if (begin >= end) return;
  if (forked_child()) { worker(begin, end); return; }
  within_thread_limit([&] {
    TbbBody<W> body(worker);
    tbb::parallel_reduce(tbb::blocked_range<std::size_t>(begin, end, std::max<std::size_t>(1, grain)), body); });
}

} // namespace

#endif
