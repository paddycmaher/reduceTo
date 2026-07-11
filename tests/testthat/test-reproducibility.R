test_that("item-target correlations are reproducible across repeated calls above the 10k-row sampling threshold", {
  set.seed(1)
  n <- 10001; pool <- 8
  data <- as.data.frame(matrix(rnorm(n * pool), ncol = pool))
  colnames(data) <- paste0("Item_", 1:pool)
  target <- rowMeans(data[, 1:4]) + rnorm(n, 0, 0.3)

  r1 <- reduceTo(data, n.items = 3, target = target, show.progress = FALSE)
  r2 <- reduceTo(data, n.items = 3, target = target, show.progress = FALSE)

  expect_equal(r1$item_cors, r2$item_cors)
  expect_equal(r1$best_item_cors, r2$best_item_cors)
})
