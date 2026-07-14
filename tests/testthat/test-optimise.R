test_that("optimise = 'none' forces exhaustive search, matching unconstrained exhaustive", {
  set.seed(1)
  data <- as.data.frame(matrix(rnorm(200 * 8), ncol = 8))
  colnames(data) <- paste0("Item_", 1:8)

  r_forced <- reduceTo(data, n.items = 3, ceiling = 5, optimise = "none", show.progress = FALSE)
  r_unconstrained <- reduceTo(data, n.items = 3, ceiling = 10^6, show.progress = FALSE)

  expect_equal(r_forced$r, r_unconstrained$r)
  expect_equal(r_forced$best_names, r_unconstrained$best_names)
})

test_that("optimise = 'progressive' (default) triggers optimisation when combinations exceed ceiling", {
  set.seed(1)
  data <- as.data.frame(matrix(rnorm(200 * 10), ncol = 10))
  colnames(data) <- paste0("Item_", 1:10)

  expect_message(
    reduceTo(data, n.items = 4, ceiling = 5, show.progress = FALSE),
    "This task would generate"
  )
})

test_that("optimise = 'none' with a large problem does not trigger optimisation", {
  set.seed(1)
  data <- as.data.frame(matrix(rnorm(200 * 10), ncol = 10))
  colnames(data) <- paste0("Item_", 1:10)

  expect_message(
    reduceTo(data, n.items = 4, ceiling = 5, optimise = "none", show.progress = FALSE),
    "exceeds ceiling"
  )
})
