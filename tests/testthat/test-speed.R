test_that("speed = 'fast' and 'conservative' agree on complete data", {
  set.seed(1)
  data <- as.data.frame(matrix(rnorm(200 * 8), ncol = 8))
  colnames(data) <- paste0("Item_", 1:8)
  target <- rowMeans(data)

  r_fast <- reduceTo(data, n.items = 3, target = target, speed = "fast", show.progress = FALSE)
  r_cons <- reduceTo(data, n.items = 3, target = target, speed = "conservative", show.progress = FALSE)

  expect_equal(r_fast$r, r_cons$r)
  expect_equal(r_fast$best_names, r_cons$best_names)
})

test_that("speed = 'fast' reports the true pairwise-deletion r under missing data, not an imputed approximation", {
  set.seed(1)
  n <- 300; pool <- 10
  data <- as.data.frame(matrix(rnorm(n * pool), ncol = pool))
  colnames(data) <- paste0("Item_", 1:pool)
  target <- rowMeans(data[, 1:4]) + rnorm(n, 0, 0.3)

  set.seed(2)
  data_mat <- as.matrix(data)
  mask <- matrix(runif(n * pool) < 0.15, nrow = n)
  data_mat[mask] <- NA
  data_na <- as.data.frame(data_mat)

  r_fast <- reduceTo(data_na, n.items = 3, target = target, speed = "fast",
                     generate = TRUE, show.progress = FALSE)
  true_r <- cor(r_fast$scores[, 1], target, use = "pairwise.complete.obs")

  expect_equal(r_fast$r, true_r, tolerance = 1e-6)
})

test_that("leaderboard is ranked by the reported (exact) r, not the engine's imputed r", {
  set.seed(1)
  n <- 800; pool <- 14
  f <- rnorm(n)
  data_mat <- sapply(1:pool, function(j) (0.3 + j / 30) * f + rnorm(n))
  data_mat[matrix(runif(n * pool) < 0.25, nrow = n)] <- NA
  data <- as.data.frame(data_mat)
  target <- f + rnorm(n, 0, 0.6)

  r <- reduceTo(data, n.items = 5, target = target, speed = "fast", show.progress = FALSE)

  expect_equal(r$r, max(r$leaderboard$r))
  expect_false(is.unsorted(rev(r$leaderboard$r)))
})

test_that("speed = 'conservative' finds the true best raw-sum set on mixed-range continuous items", {
  set.seed(18)
  n <- 1500
  f <- rnorm(n)
  target <- f + rnorm(n)
  # strong-but-narrow items vs weak-but-wide items: per-column 8-bit ranges
  # used to re-weight these, so the search optimised a different composite
  data_mat <- cbind(sapply(1:5, function(j) 0.3 * (f + 0.6 * rnorm(n))),
                    sapply(1:5, function(j) 3.0 * (0.25 * f + rnorm(n))))
  colnames(data_mat) <- paste0("Item_", 1:10)

  true_best <- max(combn(10, 4, function(s) cor(rowSums(data_mat[, s]), target)))
  r <- reduceTo(as.data.frame(data_mat), n.items = 4, target = target, speed = "conservative", show.progress = FALSE)

  expect_equal(r$r, true_best, tolerance = 1e-8)
})
