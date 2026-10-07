test_that("optimise = FALSE forces exhaustive search, matching unconstrained exhaustive", {
  set.seed(1)
  data <- as.data.frame(matrix(rnorm(200 * 8), ncol = 8))
  colnames(data) <- paste0("Item_", 1:8)

  r_forced <- reduceTo(data, n.items = 3, ceiling = 5, optimise = FALSE, show.progress = FALSE)
  r_unconstrained <- reduceTo(data, n.items = 3, ceiling = 10^6, show.progress = FALSE)

  expect_equal(r_forced$r, r_unconstrained$r)
  expect_equal(r_forced$best_names, r_unconstrained$best_names)
})

test_that("optimise = TRUE (default) triggers optimisation when combinations exceed ceiling", {
  set.seed(1)
  data <- as.data.frame(matrix(rnorm(200 * 10), ncol = 10))
  colnames(data) <- paste0("Item_", 1:10)

  expect_message(
    reduceTo(data, n.items = 4, ceiling = 5, show.progress = FALSE),
    "This would generate"
  )
})

test_that("optimise = FALSE with a large problem does not trigger optimisation", {
  set.seed(1)
  data <- as.data.frame(matrix(rnorm(200 * 10), ncol = 10))
  colnames(data) <- paste0("Item_", 1:10)

  expect_message(
    reduceTo(data, n.items = 4, ceiling = 5, optimise = FALSE, show.progress = FALSE),
    "exceeds ceiling"
  )
})

test_that("searches keeping more than half the pool still score every set exactly (drop-side enumeration)", {
  set.seed(3)
  n <- 300; f <- rnorm(n)
  data <- as.data.frame(sapply(1:12, function(j) (0.2 + j / 20) * f + rnorm(n)))
  target <- f + rnorm(n)

  brute <- combn(12, 9, function(s) cor(rowSums(data[, s]), target))
  r <- reduceTo(data, n.items = 9, target = target, optimise = FALSE, show.progress = FALSE, verbose = FALSE)

  expect_equal(r$r, max(brute))
  expect_equal(sort(r$best_indices), combn(12, 9)[, which.max(brute)])
})

test_that("keeping most of a pool under optimisation still finds the exhaustive best set", {
  # narrowing used to shrink the pool straight to n.items here, leaving the
  # final set to a k = 2 ranking with no final search at all
  set.seed(4)
  n <- 1000; f <- rnorm(n)
  data <- as.data.frame(sapply(1:24, function(j) runif(1, 0.2, 0.9) * f + rnorm(n)))

  r_opt <- reduceTo(data, n.items = 20, ceiling = 1000, rfe.budget = 1000, show.progress = FALSE, verbose = FALSE)
  r_exh <- reduceTo(data, n.items = 20, optimise = FALSE, show.progress = FALSE, verbose = FALSE)

  expect_equal(r_opt$r, r_exh$r)
  expect_setequal(r_opt$best_names, r_exh$best_names)
})

test_that("narrowing never shrinks the pool below what the final search can afford", {
  set.seed(4)
  n <- 500; f <- rnorm(n)
  data <- as.data.frame(sapply(1:24, function(j) runif(1, 0.2, 0.9) * f + rnorm(n)))

  # choose(22, 20) = 231 fits ceiling = 1000, so the final search gets 22 items, not 20
  expect_output(
    reduceTo(data, n.items = 20, ceiling = 1000, rfe.budget = 1000, speed = "conservative", verbose = FALSE),
    "search space to 22 items"
  )
})
