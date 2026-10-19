test_that("the selected set's statistics match direct computations", {
  set.seed(5)
  n <- 400; f <- rnorm(n)
  data <- as.data.frame(sapply(1:10, function(j) runif(1, 0.3, 0.8) * f + rnorm(n)))
  data[, 3] <- -data[, 3]                          # one reverse-keyed item
  r <- reduceTo(data, n.items = 5, show.progress = FALSE, verbose = FALSE)

  X <- as.matrix(data[, r$selected$items])
  X[, r$selected$keys < 0] <- -X[, r$selected$keys < 0]
  S <- cov(X)
  expect_equal(r$alpha, 5 / 4 * (1 - sum(diag(S)) / sum(S)), tolerance = 1e-8)
  expect_equal(r$r_parent, r$r, tolerance = 1e-8)  # no target: the target is the full scale
  expect_equal(nrow(r$item_stats), 5)
  expect_equal(r$item_stats$item, r$selected$items)
  expect_equal(r$item_stats$alpha_if_deleted[1], { S1 <- S[-1, -1]; 4 / 3 * (1 - sum(diag(S1)) / sum(S1)) }, tolerance = 1e-8)
  expect_equal(r$item_stats$r_rest[2], cor(X[, 2], rowSums(X[, -2])), tolerance = 1e-8)
  expect_type(r$selected$indices, "integer")
  expect_equal(unname(r$selected$item_r), unname(r$item_r[[1]]))
  expect_equal(sign(unname(r$selected$item_r)), unname(r$selected$keys))
})

test_that("the output records its run", {
  set.seed(6)
  data <- as.data.frame(matrix(rnorm(300 * 8), ncol = 8))
  y <- rnorm(300)
  r <- reduceTo(data, n.items = 3, target = y, cross.validate = TRUE, show.progress = FALSE, verbose = FALSE)
  expect_equal(unname(r$n), c(300, 225, 75))
  expect_false(is.na(r$r_holdout))
  expect_true(is.na(r$auc) && is.na(r$cutoff))
  expect_identical(r$settings$target, "external")
  expect_type(r$settings$na.rm, "logical")
  expect_equal(r$settings$cross.validate, 0.75)
  expect_true(is.call(r$call))
  expect_type(r$version, "character")
  expect_equal(r$seconds, tail(r$timings$steps$s, 1), tolerance = 1e-3)
  expect_null(r$binary)
  expect_identical(r$pool$valid, names(data))
})

test_that("input row names are kept on $scores and $target, and don't change the result", {
  set.seed(8)
  n <- 300; f <- rnorm(n)
  data <- as.data.frame(sapply(1:9, function(j) runif(1, 0.3, 0.8) * f + rnorm(n)))
  plain <- reduceTo(data, n.items = 3, show.progress = FALSE, verbose = FALSE)
  expect_null(rownames(plain$scores)); expect_null(names(plain$target))

  ids <- paste0("id", seq_len(n)); named <- data; rownames(named) <- ids
  r <- reduceTo(named, n.items = 3, show.progress = FALSE, verbose = FALSE)
  expect_identical(rownames(r$scores), ids)
  expect_identical(names(r$target), ids)
  expect_equal(r$r, plain$r); expect_identical(r$selected$items, plain$selected$items)
  expect_equal(unname(r$scores[, 1]), unname(plain$scores[, 1]))

  scrambled <- data[sample(n), ]                  # integer row names out of order
  r2 <- reduceTo(scrambled, n.items = 3, cross.validate = TRUE, show.progress = FALSE, verbose = FALSE)
  expect_identical(rownames(r2$scores), rownames(scrambled))

  m <- as.matrix(named)                           # matrix input with row names, external target from a column
  r3 <- reduceTo(cbind(m, y = f + rnorm(n)), n.items = 3, target = y, show.progress = FALSE, verbose = FALSE)
  expect_identical(rownames(r3$scores), ids); expect_identical(names(r3$target), ids)
  r4 <- reduceTo(m, n.items = 3, target = f, show.progress = FALSE, verbose = FALSE)   # a target the user supplied: left alone
  expect_null(names(r4$target)); expect_identical(rownames(r4$scores), ids)
})
