# reduceTo 1.7.3

* Items far from zero (e.g. values in the millions) no longer lose accuracy in the search: the
  cross-products are now centred first. Around 10^8 they used to give negative variances and crash R.
* Works inside forked processes (`parallel::mclapply()` and the like) after the session has already run a
  search; those children used to crash. In a forked child, searches run on one thread.
* `RcppParallel::setThreadOptions(numThreads = n)` now limits the knockout search too (it used every core).
* Fixed two crashes seen only on some Linux builds (R-hub's gcc15 and clang-asan): the parallel loops now
  run on TBB directly rather than through RcppParallel's compiled wrappers, and the knockout's
  leave-one-out searches no longer start a thread of their own. Same results, same speed.
* Searches with `speed = "conservative"` and `show.progress = FALSE` can be interrupted.
* The binary cutoff search tries only the cutoffs where the result can change: the same answers, but fast
  and light for items on any scale (items in the millions used to need gigabytes of memory).
* `target` can be a quoted column name (`target = "crit"`), or a variable holding one.
* With `scale.vars = TRUE`, a binary target stays binary (it used to be treated as continuous): the
  cutoff can then be any value of the standardised sum score, the best of every threshold.
* New argument `seed` (default 1, as before) for the cross-validation split; `seed = NULL` uses the
  session's random state.
* Clear errors for infinite target values, splits leaving fewer than 3 training or holdout rows, data that
  is not a matrix or data frame, and invalid `ceiling` or `cross.validate` values. A single column works.
* No longer needs BLAS or a Fortran runtime: the cross-products are computed by the package's own parallel
  C++ (about as fast as Apple's Accelerate, and faster than R's reference BLAS), so building from source
  needs only a C++ compiler.
* A clear error when no rows have a usable target (e.g. `na.rm = FALSE` with missing values throughout).
* `print()` returns its argument invisibly; binary targets with `r.sq = TRUE` and cross-validation also get
  holdout R-squared; item statistics for long item sets are much faster.

# reduceTo 1.7.2

* The knockout search's "sweepdown": the search is repeated at a tenth of `knockout.budget`, a hundredth,
  and so on, keeping the best set from any of them, so ten times the budget never returns a worse set.
  Most of those smaller searches are read off the larger ones' results, adding up to about 15% to the time.
* Faster exhaustive searches of 500,000 to 20 million combinations.

# reduceTo 1.7.1

* Faster Gram-matrix engine (2-5 times on long forms and half-length forms).
* Default `ceiling` raised to 10^12; each knockout search scores up to `knockout.budget` combinations
  (default `ceiling / 1000`).
* `optimise = TRUE` runs a knockout search on any problem; `NULL` (the default) only above `ceiling`.
