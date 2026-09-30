test_that("n.items, n.sets and item.set must be single whole numbers >= 1", {
  set.seed(1)
  data <- as.data.frame(matrix(rnorm(200 * 6), ncol = 6))

  expect_error(reduceTo(data, n.items = 0, show.progress = FALSE), "n.items")
  expect_error(reduceTo(data, n.items = 2.5, show.progress = FALSE), "n.items")
  expect_error(reduceTo(data, n.items = c(2, 3), show.progress = FALSE), "n.items")
  expect_error(reduceTo(data, n.items = 3, n.sets = 0, show.progress = FALSE), "n.sets")
  expect_error(reduceTo(data, n.items = 3, item.set = 1.5, show.progress = FALSE), "item.set")
})

test_that("item.set beyond the ranked sets gives a clear error", {
  set.seed(1)
  data <- as.data.frame(matrix(rnorm(200 * 5), ncol = 5))

  expect_error(reduceTo(data, n.items = 4, item.set = 8, show.progress = FALSE), "item.set")
})

test_that("non-numeric and constant targets give clear errors", {
  set.seed(1)
  data <- as.data.frame(matrix(rnorm(200 * 6), ncol = 6))

  expect_error(reduceTo(data, n.items = 3, target = factor(rowMeans(data) > 0), show.progress = FALSE), "numeric")
  expect_error(reduceTo(data, n.items = 3, target = rep(1, 200), show.progress = FALSE), "variance")
})

test_that("duplicate column names are made unique rather than mis-mapped", {
  set.seed(1)
  data <- as.data.frame(matrix(rnorm(200 * 6), ncol = 6))
  colnames(data) <- c("a", "a", "b", "c", "d", "e")

  r <- reduceTo(data, n.items = 3, show.progress = FALSE)

  expect_false(anyDuplicated(r$filtered_items) > 0)
  expect_true(all(r$best_names %in% make.unique(colnames(data))))
  expect_equal(r$r, cor(r$scores[, 1], r$target), tolerance = 1e-8)
})
