## Submission

This is a new submission.

## Test environments

* local macOS 26 (aarch64), R 4.5.2
* R-hub (GitHub Actions), R-devel (2026-10-18): Ubuntu Linux, Windows, macOS (x86_64 and arm64),
  m1-san (macOS arm64, ASan/UBSan), gcc-asan, clang-asan, clang-ubsan, valgrind, gcc15 (Fedora), donttest
* win-builder, R-devel (2026-10-18 r90707 ucrt), Windows Server 2022 x64

## R CMD check results

0 errors | 0 warnings | 1 note

* New submission.
* Possibly misspelled words in DESCRIPTION: "Youden's" (Youden's J statistic) and "binarised" (British
  spelling) are spelled correctly.

## Notes for the reviewers

* The package's compiled code needs only a C++17 compiler (no BLAS, LAPACK or Fortran). GNU make is used
  for the `RcppParallel::LdFlags()` call in `src/Makevars`, as RcppParallel recommends.
* The package uses multiple threads through RcppParallel. Examples and tests limit themselves to two
  threads (`RcppParallel::setThreadOptions(numThreads = 2)`), as the CRAN policy asks.
* `reduceTo()` sets a seed (argument `seed`, default 1) for its cross-validation split, so that results can
  be repeated; the caller's random state is restored afterwards, and `seed = NULL` uses it instead.
