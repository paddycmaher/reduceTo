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

test_that("speed = 'fast' is a no-op message-wise when data is complete", {
  set.seed(1)
  data <- as.data.frame(matrix(rnorm(200 * 8), ncol = 8))
  colnames(data) <- paste0("Item_", 1:8)

  expect_no_message(
    reduceTo(data, n.items = 3, speed = "fast", show.progress = FALSE),
    message = "Fast path"
  )
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
  expect_message(
    reduceTo(data_na, n.items = 3, target = target, speed = "fast", show.progress = FALSE),
    "Fast path"
  )
})

test_that("speed = 'conservative' never prints the fast-path message", {
  set.seed(1)
  n <- 300; pool <- 10
  data <- as.data.frame(matrix(rnorm(n * pool), ncol = pool))
  colnames(data) <- paste0("Item_", 1:pool)
  set.seed(2)
  data_mat <- as.matrix(data)
  mask <- matrix(runif(n * pool) < 0.15, nrow = n)
  data_mat[mask] <- NA
  data_na <- as.data.frame(data_mat)

  expect_no_message(
    reduceTo(data_na, n.items = 3, speed = "conservative", show.progress = FALSE),
    message = "Fast path"
  )
})
