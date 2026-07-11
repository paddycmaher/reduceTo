test_that("prefilter narrows the pool to strongly-correlated items", {
  set.seed(3)
  n <- 500
  strong <- matrix(rnorm(n * 4), ncol = 4)
  target <- rowMeans(strong) + rnorm(n, 0, 0.3)
  noise <- matrix(rnorm(n * 20), ncol = 20)
  data <- as.data.frame(cbind(strong, noise))
  colnames(data) <- c(paste0("Strong_", 1:4), paste0("Noise_", 1:20))

  r <- reduceTo(data, n.items = 3, target = target, ceiling = 50,
               prefilter.ratio = 5, show.progress = FALSE)

  expect_true(length(r$pool_names) < 24)
  expect_true(all(grepl("Strong", r$best_names)))
})

test_that("prefilter never prunes below n.items columns (safety floor)", {
  set.seed(1)
  data <- as.data.frame(matrix(rnorm(200 * 6), ncol = 6))
  colnames(data) <- paste0("Item_", 1:6)

  expect_no_error(
    reduceTo(data, n.items = 5, ceiling = 5, prefilter.ratio = 0.001, show.progress = FALSE)
  )
})

test_that("prefilter.ratio = Inf and NULL both skip the prefilter step", {
  set.seed(3)
  n <- 500
  strong <- matrix(rnorm(n * 4), ncol = 4)
  target <- rowMeans(strong) + rnorm(n, 0, 0.3)
  noise <- matrix(rnorm(n * 20), ncol = 20)
  data <- as.data.frame(cbind(strong, noise))
  colnames(data) <- c(paste0("Strong_", 1:4), paste0("Noise_", 1:20))

  expect_no_message(
    reduceTo(data, n.items = 3, target = target, ceiling = 50, prefilter.ratio = Inf, show.progress = FALSE),
    message = "Prefilter"
  )
  expect_no_message(
    reduceTo(data, n.items = 3, target = target, ceiling = 50, prefilter.ratio = NULL, show.progress = FALSE),
    message = "Prefilter"
  )
})
