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

test_that("reduceTo() leaves no RNG seed behind in a fresh session that had none", {
  d <- data.frame(matrix(rnorm(1000), 100))
  saved <- get(".Random.seed", envir = globalenv())
  on.exit(assign(".Random.seed", saved, envir = globalenv()))
  rm(".Random.seed", envir = globalenv())

  invisible(reduceTo(d, 3, show.progress = FALSE))

  # Before the fix, the seed left by reduceTo()'s internal set.seed(1) calls
  # stayed in place, so every fresh session's random draws after a call
  # were identical (the set.seed(1) stream)
  expect_false(exists(".Random.seed", envir = globalenv()))
})

test_that("seed sets the cross-validation split; seed = NULL draws from the session's random state", {
  set.seed(7); n <- 300; f <- rnorm(n)
  d <- as.data.frame(sapply(1:8, function(j) runif(1, .3, .8) * f + rnorm(n)))
  q <- function(...) reduceTo(d, 3, cross.validate = TRUE, show.progress = FALSE, verbose = FALSE, ...)
  a <- q(); b <- q(seed = 1); c2 <- q(seed = 2)
  expect_identical(a$r_holdout, b$r_holdout)                      # the default is seed = 1
  expect_false(identical(a$r_holdout, c2$r_holdout))
  expect_identical(a$settings$seed, 1)
  set.seed(11); n1 <- q(seed = NULL); x1 <- runif(1)
  set.seed(11); n2 <- q(seed = NULL); x2 <- runif(1)
  expect_identical(n1$r_holdout, n2$r_holdout)                    # the same session state, the same split
  set.seed(11); y <- runif(1); expect_false(identical(x1, y))     # and the session's state moved on
  expect_error(q(seed = "a"), "seed")
})
