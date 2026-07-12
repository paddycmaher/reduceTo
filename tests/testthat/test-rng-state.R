test_that("reduceTo() does not leak its internal set.seed(1) calls into the caller's RNG stream", {
  set.seed(99)
  d <- data.frame(matrix(rnorm(1000), 100))

  invisible(reduceTo(d, 3, show.progress = FALSE))
  x1 <- rnorm(5)

  invisible(reduceTo(d, 3, show.progress = FALSE))
  x2 <- rnorm(5)

  # Before the fix, reduceTo()'s internal set.seed(1) calls left the global
  # RNG in the identical post-set.seed(1) state after every call, so any
  # caller code drawing random numbers after reduceTo() would silently
  # restart from the same point every time.
  expect_false(identical(x1, x2))
})

test_that("reduceTo() restores the caller's RNG state even when a target is supplied and cross-validated", {
  set.seed(42)
  d <- as.data.frame(matrix(rnorm(300 * 8), ncol = 8))
  target <- rowMeans(d[, 1:4]) + rnorm(300, 0, 0.3)

  invisible(reduceTo(d, n.items = 3, target = target, cross.validate = TRUE, show.progress = FALSE))
  x1 <- runif(5)

  invisible(reduceTo(d, n.items = 3, target = target, cross.validate = TRUE, show.progress = FALSE))
  x2 <- runif(5)

  expect_false(identical(x1, x2))
})
