// How the parallel loops run: safely in a fork()'d child, and within the user's thread limit.
//
// TBB, which runs RcppParallel's loops (and the knockout's own), can't be used after fork(): once the
// parent has run a search, the first parallel loop in a child of parallel::mclapply() (or anything else
// that forks) crashes it. In a child, these run on the calling thread instead -- the fork has already
// shared the cores out among the children. (RcppParallel >= 6.1.0 offers isProcessForkedChild() for
// this; the same test is kept here so older versions work too.)
//
// RcppParallel::setThreadOptions(numThreads = n) limits RcppParallel's loops to n threads; the knockout's
// own TBB loops (used directly because they nest) keep to the same limit by running in an arena of n
// threads -- loops nested inside run in that arena too. Unlimited by default
#ifndef REDUCETO_PARALLEL_GUARD_H
#define REDUCETO_PARALLEL_GUARD_H

#include <RcppParallel.h>
#include <tbb/task_arena.h>
#include <cstddef>
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

template <class W> inline void par_for(std::size_t begin, std::size_t end, W& worker, std::size_t grain = 1) {
  if (forked_child()) { if (begin < end) worker(begin, end); }
  else RcppParallel::parallelFor(begin, end, worker, grain);
}
template <class W> inline void par_reduce(std::size_t begin, std::size_t end, W& worker, std::size_t grain = 1) {
  if (forked_child()) { if (begin < end) worker(begin, end); }
  else RcppParallel::parallelReduce(begin, end, worker, grain);
}

// RcppParallel::setThreadOptions()'s limit (-1: none)
inline int thread_limit() { return RcppParallel::resolveValue("RCPP_PARALLEL_NUM_THREADS", -1, -1); }
template <class F> inline void within_thread_limit(F&& f) {
  const int n = thread_limit();
  if (n > 0 && tbb::this_task_arena::max_concurrency() > n) { tbb::task_arena arena(n); arena.execute(f); }
  else f();
}

} // namespace

#endif
