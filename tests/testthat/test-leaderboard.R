test_that("leaderboard length is 100 for continuous targets and non-youden_j binary ranking", {
  set.seed(1)
  n <- 300; pool <- 15
  data <- as.data.frame(matrix(rnorm(n * pool), ncol = pool))
  colnames(data) <- paste0("Item_", 1:pool)

  r_cont <- reduceTo(data, n.items = 4, show.progress = FALSE)
  expect_equal(nrow(r_cont$leaderboard), 100)

  target_bin <- ifelse(rowMeans(data) > 0, 1, 0)
  r_bin_r <- reduceTo(data, n.items = 4, target = target_bin, method = "r", show.progress = FALSE)
  expect_equal(nrow(r_bin_r$leaderboard), 100)
})

test_that("leaderboard length is up to 1000 when ranking by youden_j", {
  set.seed(1)
  n <- 300; pool <- 15
  data <- as.data.frame(matrix(rnorm(n * pool), ncol = pool))
  colnames(data) <- paste0("Item_", 1:pool)
  target_bin <- ifelse(rowMeans(data) > 0, 1, 0)

  r_bin_j <- reduceTo(data, n.items = 4, target = target_bin, show.progress = FALSE)  # default is youden_j

  expect_equal(nrow(r_bin_j$leaderboard), min(1000, choose(pool, 4)))
})

test_that("leaderboard length is up to 1000 when ranking by binarised_r", {
  set.seed(1)
  n <- 300; pool <- 15
  data <- as.data.frame(matrix(rnorm(n * pool), ncol = pool))
  colnames(data) <- paste0("Item_", 1:pool)
  target_bin <- ifelse(rowMeans(data) > 0, 1, 0)

  r_bin_b <- reduceTo(data, n.items = 4, target = target_bin, method = "binarised_r", show.progress = FALSE)

  expect_equal(nrow(r_bin_b$leaderboard), min(1000, choose(pool, 4)))
})

test_that("$output has no padding NA rows when there are fewer combinations than n.sets", {
  set.seed(1)
  data <- as.data.frame(matrix(rnorm(200 * 4), ncol = 4))

  r <- reduceTo(data, n.items = 3, n.sets = 5, show.progress = FALSE)

  expect_equal(nrow(r$output), choose(4, 3))
  expect_false(anyNA(r$output$r))
})
