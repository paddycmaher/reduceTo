# CRAN's check machines allow a package at most two threads at a time
if (!identical(Sys.getenv("NOT_CRAN"), "true")) RcppParallel::setThreadOptions(numThreads = 2)
