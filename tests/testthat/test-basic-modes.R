test_that("internal consistency mode returns valid results", {
  set.seed(1)
  data <- as.data.frame(matrix(rnorm(200 * 10), ncol = 10))
  colnames(data) <- paste0("Item_", 1:10)

  r <- reduceTo(data, n.items = 4, show.progress = FALSE)

  expect_s3_class(r, "reduced_scale")
  expect_length(r$best_names, 4)
  expect_true(r$r > 0 && r$r <= 1)
})

test_that("continuous external target mode returns valid results", {
  set.seed(1)
  data <- as.data.frame(matrix(rnorm(200 * 10), ncol = 10))
  colnames(data) <- paste0("Item_", 1:10)
  target <- rowMeans(data[, 1:5]) + rnorm(200, 0, 0.3)

  r <- reduceTo(data, n.items = 4, target = target, show.progress = FALSE)

  expect_s3_class(r, "reduced_scale")
  expect_length(r$best_names, 4)
})

test_that("binary target mode is auto-detected and returns binary_info", {
  set.seed(1)
  data <- as.data.frame(matrix(rnorm(200 * 10), ncol = 10))
  colnames(data) <- paste0("Item_", 1:10)
  target_bin <- ifelse(rowMeans(data) > 0, 1, 0)

  r <- reduceTo(data, n.items = 4, target = target_bin, show.progress = FALSE)

  expect_false(is.null(r$binary_info))
  expect_true(is.numeric(r$binary_info$cutoff))
})

test_that("binary target mode reports AUC alongside binarised_r/youden_j", {
  set.seed(1)
  data <- as.data.frame(matrix(rnorm(300 * 10), ncol = 10))
  colnames(data) <- paste0("Item_", 1:10)
  target_bin <- ifelse(rowMeans(data) > 0, 1, 0)

  r <- reduceTo(data, n.items = 4, target = target_bin, show.progress = FALSE)

  expect_true("auc" %in% colnames(r$leaderboard))
  expect_true(is.numeric(r$binary_info$results$auc))
  expect_true(r$binary_info$results$auc >= 0 && r$binary_info$results$auc <= 1)

  # Cross-check against an independent Mann-Whitney U / rank-sum computation
  # on the top-ranked combination's actual sum scores.
  manual_auc <- function(s, t) {
    n1 <- sum(t == 1); n0 <- sum(t == 0)
    ranks <- rank(s)
    (sum(ranks[t == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
  }
  expect_equal(r$binary_info$results$auc,
               manual_auc(r$scores[, "sum_score"], target_bin),
               tolerance = 1e-8)
})

test_that("cross-validation produces holdout metrics", {
  set.seed(1)
  data <- as.data.frame(matrix(rnorm(300 * 10), ncol = 10))
  colnames(data) <- paste0("Item_", 1:10)
  target <- rowMeans(data[, 1:5]) + rnorm(300, 0, 0.3)

  r <- reduceTo(data, n.items = 4, target = target, cross.validate = TRUE, show.progress = FALSE)

  expect_true("r_holdout" %in% colnames(r$leaderboard))
})

test_that("binary_info$train/$holdout are fully populated (no NAs) under cross-validation", {
  set.seed(1)
  data <- as.data.frame(matrix(rnorm(300 * 10), ncol = 10))
  colnames(data) <- paste0("Item_", 1:10)
  target_bin <- ifelse(rowMeans(data) > 0, 1, 0)

  r <- reduceTo(data, n.items = 4, target = target_bin, cross.validate = TRUE, show.progress = FALSE)

  expect_false(any(is.na(unlist(r$binary_info$train))))
  expect_false(any(is.na(unlist(r$binary_info$holdout))))
})

test_that("r.sq = TRUE does not error for binary targets under cross-validation", {
  set.seed(1)
  data <- as.data.frame(matrix(rnorm(300 * 10), ncol = 10))
  colnames(data) <- paste0("Item_", 1:10)
  target_bin <- ifelse(rowMeans(data) > 0, 1, 0)

  expect_no_error(
    reduceTo(data, n.items = 4, target = target_bin, cross.validate = TRUE, r.sq = TRUE, show.progress = FALSE)
  )
})

test_that("generate defaults to TRUE and produces $scores", {
  set.seed(1)
  data <- as.data.frame(matrix(rnorm(200 * 8), ncol = 8))
  colnames(data) <- paste0("Item_", 1:8)

  r <- reduceTo(data, n.items = 3, show.progress = FALSE)

  expect_false(is.null(r$scores))
})

test_that("generate = FALSE does not error and leaves $scores NULL", {
  set.seed(1)
  data <- as.data.frame(matrix(rnorm(200 * 8), ncol = 8))
  colnames(data) <- paste0("Item_", 1:8)

  r <- reduceTo(data, n.items = 3, generate = FALSE, show.progress = FALSE)

  expect_true(is.null(r$scores))
})

test_that("print.reduced_scale runs without error", {
  set.seed(1)
  data <- as.data.frame(matrix(rnorm(200 * 8), ncol = 8))
  colnames(data) <- paste0("Item_", 1:8)
  r <- reduceTo(data, n.items = 3, show.progress = FALSE)

  expect_output(print(r))
})
