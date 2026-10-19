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

  expect_false(anyDuplicated(r$pool$valid) > 0)
  expect_true(all(r$selected$items %in% make.unique(colnames(data))))
  expect_equal(r$r, cor(r$scores[, 1], r$target), tolerance = 1e-8)
})

test_that("bad data, targets, splits and ceilings give clear errors", {
  set.seed(1)
  data <- as.data.frame(matrix(rnorm(200 * 6), ncol = 6))
  targ <- rowMeans(data) + rnorm(200)

  expect_error(reduceTo(as.list(data), n.items = 3, show.progress = FALSE), "matrix or data frame")
  expect_error(reduceTo(data[[1]], n.items = 1, show.progress = FALSE), "matrix or data frame")
  expect_error(reduceTo(data, n.items = 3, target = replace(targ, 1, Inf), show.progress = FALSE), "infinite")
  expect_error(reduceTo(data, n.items = 3, cross.validate = 0.005, show.progress = FALSE), "at least 3")
  expect_error(reduceTo(data, n.items = 3, cross.validate = 0.995, show.progress = FALSE), "at least 3")
  expect_error(reduceTo(data, n.items = 3, cross.validate = "yes", show.progress = FALSE), "cross.validate")
  expect_error(reduceTo(data, n.items = 3, ceiling = NA, show.progress = FALSE), "ceiling")
  # a single column still works (one possible set)
  r1 <- reduceTo(data[, 1, drop = FALSE], n.items = 1, show.progress = FALSE, verbose = FALSE)
  expect_identical(r1$selected$items, "V1")
  # print() returns the result invisibly, as print methods do
  r <- reduceTo(data, n.items = 3, show.progress = FALSE, verbose = FALSE)
  out <- withVisible(capture.output(v <- print(r)))
  expect_identical(v, r)
})

test_that("a target missing almost everywhere gives a clear error", {
  set.seed(2); m <- matrix(round(runif(60 * 12, 1, 5)), 60, 12); m[sample(length(m), 0.4 * length(m))] <- NA
  expect_error(reduceTo(m, 3, na.rm = FALSE, show.progress = FALSE, verbose = FALSE), "na.rm = FALSE")
  expect_s3_class(reduceTo(m, 3, show.progress = FALSE, verbose = FALSE), "reduced_scale")
})

test_that("the target can be a quoted column name, or a variable holding one", {
  set.seed(4); n <- 200; f <- rnorm(n)
  data <- as.data.frame(sapply(1:7, function(j) runif(1, .3, .8) * f + rnorm(n))); data$crit <- f + rnorm(n)
  a <- reduceTo(data, 3, target = crit, show.progress = FALSE, verbose = FALSE)
  b <- reduceTo(data, 3, target = "crit", show.progress = FALSE, verbose = FALSE)
  col <- "crit"; d <- reduceTo(data, 3, target = col, show.progress = FALSE, verbose = FALSE)
  expect_identical(b$selected$items, a$selected$items); expect_equal(b$r, a$r)
  expect_identical(d$selected$items, a$selected$items)
  expect_false("crit" %in% b$pool$valid)                          # the target is not an item
  m <- as.matrix(data); e <- reduceTo(m, 3, target = "crit", show.progress = FALSE, verbose = FALSE)
  expect_identical(e$selected$items, a$selected$items)
  expect_error(reduceTo(data, 3, target = "nope", show.progress = FALSE), "not a column")
})
