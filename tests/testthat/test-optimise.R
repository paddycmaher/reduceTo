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

make_facet_data <- function(seed, n = 600) {
  set.seed(seed)
  g <- rnorm(n)
  fac <- sapply(1:4, function(i) rnorm(n))
  as.data.frame(sapply(1:24, function(j) runif(1, 0.3, 0.8) * g + runif(1, 0.3, 0.9) * fac[, (j - 1) %/% 6 + 1] + rnorm(n)))
}

test_that("swap polish recovers items narrowing cut (keep side) or kept outright (drop side)", {
  data <- make_facet_data(1)
  for (k in c(8, 16)) {
    r_opt <- reduceTo(data, n.items = k, ceiling = 1000, rfe.budget = 1000, show.progress = FALSE, verbose = FALSE)
    r_exh <- reduceTo(data, n.items = k, optimise = FALSE, show.progress = FALSE, verbose = FALSE)
    expect_equal(r_opt$r, r_exh$r)
    expect_setequal(r_opt$best_names, r_exh$best_names)
  }
})

test_that("items brought back by the polish work through missing data, cross-validation and binary targets", {
  data <- make_facet_data(3)
  target <- as.numeric(rowMeans(data) + rnorm(600) > 0)
  set.seed(9)
  data_na <- data
  data_na[matrix(runif(600 * 24) < 0.1, 600)] <- NA

  r_na <- reduceTo(data_na, n.items = 8, cross.validate = TRUE, ceiling = 1000, rfe.budget = 1000,
                   item.names = TRUE, show.progress = FALSE, verbose = FALSE)
  r_bin <- reduceTo(data, n.items = 8, target = target, cross.validate = TRUE, ceiling = 1000, rfe.budget = 1000,
                    item.names = TRUE, show.progress = FALSE, verbose = FALSE)

  for (r in list(r_na, r_bin)) {
    expect_length(r$best_names, 8)
    expect_true(all(r$best_names %in% names(data)))
    expect_true(all(r$best_names %in% r$final_pool_items))
    expect_false(is.na(r$output$r_holdout[1]))
  }
})

test_that("narrowing finds a hidden synergy group that local swaps cannot", {
  # 4 items that predict the target perfectly together (their noise cancels in
  # a cycle) but are individually weak, among real decoys and pure noise. The
  # swap polish can't climb towards such a group -- partial groups are worse
  # than decoys -- so finding it is narrowing's job
  set.seed(11)
  n <- 3000; target <- rnorm(n); g <- 4
  noise <- lapply(1:g, function(i) { e <- resid(lm(rnorm(n) ~ target)); 4 * e / sd(e) })
  synergy <- sapply(1:g, function(i) target + noise[[i]] - noise[[(i %% g) + 1]])
  decoys <- sapply(1:17, function(i) 0.35 * target + sqrt(1 - 0.35^2) * rnorm(n))
  data <- as.data.frame(cbind(synergy, decoys, matrix(rnorm(n * 39), n)))
  names(data) <- c(paste0("S", 1:g), paste0("D", 1:17), paste0("X", 1:39))

  r <- reduceTo(data, n.items = g, target = target, ceiling = 1e4, rfe.budget = 1e4,
                show.progress = FALSE, verbose = FALSE)
  expect_identical(r$search$type, "optimised")
  expect_setequal(r$best_names, paste0("S", 1:g))
  expect_gt(r$r, 0.999999)
})

test_that("the result reports whether the search was exhaustive, and what the polish changed", {
  data <- make_facet_data(1)
  exh <- reduceTo(data[, 1:12], n.items = 4, show.progress = FALSE, verbose = FALSE)
  opt <- reduceTo(data, n.items = 8, ceiling = 1000, rfe.budget = 1000, show.progress = FALSE, verbose = FALSE)

  expect_identical(exh$search$type, "exhaustive")
  expect_null(exh$search$polish)
  expect_identical(opt$search$type, "optimised")
  expect_equal(opt$search$polish$r_polished, opt$r)
  expect_gte(opt$search$polish$r_polished, opt$search$polish$r_search)
  expect_length(opt$search$polish$swapped_in, length(opt$search$polish$swapped_out))
})
