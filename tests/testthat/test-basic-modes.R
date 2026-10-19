test_that("internal consistency mode returns valid results", {
  set.seed(1)
  data <- as.data.frame(matrix(rnorm(200 * 10), ncol = 10))
  colnames(data) <- paste0("Item_", 1:10)

  r <- reduceTo(data, n.items = 4, show.progress = FALSE)

  expect_s3_class(r, "reduced_scale")
  expect_length(r$selected$items, 4)
  expect_true(r$r > 0 && r$r <= 1)
})

test_that("continuous external target mode returns valid results", {
  set.seed(1)
  data <- as.data.frame(matrix(rnorm(200 * 10), ncol = 10))
  colnames(data) <- paste0("Item_", 1:10)
  target <- rowMeans(data[, 1:5]) + rnorm(200, 0, 0.3)

  r <- reduceTo(data, n.items = 4, target = target, show.progress = FALSE)

  expect_s3_class(r, "reduced_scale")
  expect_length(r$selected$items, 4)
})

test_that("binary target mode is auto-detected and returns binary", {
  set.seed(1)
  data <- as.data.frame(matrix(rnorm(200 * 10), ncol = 10))
  colnames(data) <- paste0("Item_", 1:10)
  target_bin <- ifelse(rowMeans(data) > 0, 1, 0)

  r <- reduceTo(data, n.items = 4, target = target_bin, show.progress = FALSE)

  expect_false(is.null(r$binary))
  expect_true(is.numeric(r$binary$cutoff))
})

test_that("binary target mode reports AUC alongside binarised_r/youden_j", {
  set.seed(1)
  data <- as.data.frame(matrix(rnorm(300 * 10), ncol = 10))
  colnames(data) <- paste0("Item_", 1:10)
  target_bin <- ifelse(rowMeans(data) > 0, 1, 0)

  r <- reduceTo(data, n.items = 4, target = target_bin, show.progress = FALSE)

  expect_true("auc" %in% colnames(r$leaderboard))
  expect_true(is.numeric(r$binary$results$auc))
  expect_true(r$binary$results$auc >= 0 && r$binary$results$auc <= 1)

  # Cross-check against an independent Mann-Whitney U / rank-sum computation
  # on the top-ranked combination's actual sum scores.
  manual_auc <- function(s, t) {
    n1 <- sum(t == 1); n0 <- sum(t == 0)
    ranks <- rank(s)
    (sum(ranks[t == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
  }
  expect_equal(r$binary$results$auc,
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

test_that("binary$train/$holdout are fully populated (no NAs) under cross-validation", {
  set.seed(1)
  data <- as.data.frame(matrix(rnorm(300 * 10), ncol = 10))
  colnames(data) <- paste0("Item_", 1:10)
  target_bin <- ifelse(rowMeans(data) > 0, 1, 0)

  r <- reduceTo(data, n.items = 4, target = target_bin, cross.validate = TRUE, show.progress = FALSE)

  expect_false(any(is.na(unlist(r$binary$train))))
  expect_false(any(is.na(unlist(r$binary$holdout))))
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

test_that("reverse-keyed items are scored conventionally ((min + max) - x), so cutoffs match hand scoring", {
  set.seed(8)
  n <- 2000
  f <- rnorm(n)
  lik <- function(z) pmin(5, pmax(1, round(3 + z)))
  data <- data.frame(p1 = lik(f + rnorm(n)), p2 = lik(f + rnorm(n)), p3 = lik(f + rnorm(n)),
                     n1 = lik(-f + rnorm(n)), n2 = lik(-f + rnorm(n)))
  target_bin <- as.numeric(f + rnorm(n, 0, 0.7) > 0.5)

  r <- reduceTo(data, n.items = 4, target = target_bin, show.progress = FALSE)

  hand <- as.matrix(data[, r$selected$items])
  reversed <- r$selected$items %in% c("n1", "n2")
  hand[, reversed] <- 6 - hand[, reversed]
  expect_equal(unname(r$scores[, "sum_score"]), unname(rowSums(hand)))
  expect_equal(unname(r$selected$keys), ifelse(reversed, -1, 1))
})

test_that("scale.vars = TRUE puts training and holdout rows on the same scale under cross-validation", {
  set.seed(2)
  data <- as.data.frame(matrix(round(runif(600 * 8, 1, 5)), ncol = 8) + rnorm(600))
  colnames(data) <- paste0("Item_", 1:8)
  target <- rowMeans(data) + rnorm(600)

  r <- reduceTo(data, n.items = 3, target = target, scale.vars = TRUE, cross.validate = TRUE, show.progress = FALSE)

  set.seed(1)
  train <- sample(1:600, 450)
  # Holdout rows used to stay on the raw scale (means ~9) next to z-scored training rows (means 0)
  expect_lt(abs(mean(r$scores[-train, 1])), 0.5)
  expect_length(r$target, 600)
})

test_that("a target given as a column name works for matrix input too", {
  set.seed(1)
  m <- matrix(rnorm(300 * 7), ncol = 7, dimnames = list(NULL, c(paste0("Item_", 1:6), "outcome")))
  m[, "outcome"] <- rowSums(m[, 1:3]) + rnorm(300)

  r <- reduceTo(m, n.items = 3, target = outcome, show.progress = FALSE)

  expect_false("outcome" %in% r$selected$items)
  expect_length(r$selected$items, 3)
})

test_that("logical (TRUE/FALSE) items are kept as 0/1 items rather than silently dropped", {
  set.seed(1)
  data <- as.data.frame(matrix(rnorm(300 * 5), ncol = 5))
  data$flag <- data$V1 + rnorm(300) > 0

  r <- reduceTo(data, n.items = 3, show.progress = FALSE)

  expect_true("flag" %in% r$pool$valid)
})

test_that("an item uncorrelated with the target is not turned into a free 'dummy' slot", {
  set.seed(3)
  n <- 3000
  f <- rnorm(n)
  target <- f + rnorm(n, 0, 0.5)
  z <- rnorm(n)
  z <- resid(lm(z ~ target))  # exactly uncorrelated with the target
  data <- data.frame(A1 = f + rnorm(n, 0, .7), A2 = f + rnorm(n, 0, .7), A3 = f + rnorm(n, 0, .7),
                     N1 = rnorm(n), N2 = rnorm(n), Z = z)

  r <- reduceTo(data, n.items = 4, target = target, show.progress = FALSE)

  # The reported r must be the r a user actually gets from the reported items
  expect_equal(r$r, cor(rowSums(data[, r$selected$items]), target), tolerance = 1e-8)
})

test_that("scale.vars = TRUE keeps a binary target binary, with a cutoff at any value of the scaled score", {
  set.seed(12); n <- 400; f <- rnorm(n)
  data <- as.data.frame(sapply(1:8, function(j) round(3 + runif(1, .4, .9) * f + rnorm(n)) * sample(c(1, 10), 1)))
  y <- as.numeric(f + rnorm(n) > 0.2)
  r <- reduceTo(data, 3, target = y, scale.vars = TRUE, show.progress = FALSE, verbose = FALSE)
  expect_false(is.null(r$binary))
  expect_identical(r$settings$ranking_metric, "youden_j")
  s <- r$scores[, "sum_score"]
  expect_true(r$cutoff %in% s)                                    # a cutoff at an observed score
  best_j <- max(sapply(sort(unique(s)), function(c) mean(s[y == 1] >= c) + mean(s[y == 0] < c) - 1))
  expect_equal(r$youden_j, best_j, tolerance = 1e-12)             # the best of every threshold
  expect_identical(unname(r$scores[, "binary_score"] == 1), unname(s >= r$cutoff))
  rc <- reduceTo(data, 3, target = y, scale.vars = TRUE, cross.validate = TRUE, show.progress = FALSE, verbose = FALSE)
  expect_false(is.na(rc$binary$holdout$youden_j))
})
