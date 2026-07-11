test_that("optimise = FALSE forces exhaustive search, matching unconstrained exhaustive", {
  set.seed(1)
  data <- as.data.frame(matrix(rnorm(200 * 8), ncol = 8))
  colnames(data) <- paste0("Item_", 1:8)

  r_forced <- reduceTo(data, n.items = 3, ceiling = 5, optimise = FALSE, show.progress = FALSE)
  r_unconstrained <- reduceTo(data, n.items = 3, ceiling = 10^6, show.progress = FALSE)

  expect_equal(r_forced$r, r_unconstrained$r)
  expect_equal(r_forced$best_names, r_unconstrained$best_names)
})

test_that("optimise = TRUE triggers beam search when combinations exceed ceiling", {
  set.seed(1)
  data <- as.data.frame(matrix(rnorm(200 * 10), ncol = 10))
  colnames(data) <- paste0("Item_", 1:10)

  expect_message(
    reduceTo(data, n.items = 4, ceiling = 5, optimise = TRUE, show.progress = FALSE),
    "Optimisation triggered"
  )
})

test_that("optimise = FALSE with a large problem does not trigger beam search", {
  set.seed(1)
  data <- as.data.frame(matrix(rnorm(200 * 10), ncol = 10))
  colnames(data) <- paste0("Item_", 1:10)

  expect_message(
    reduceTo(data, n.items = 4, ceiling = 5, optimise = FALSE, show.progress = FALSE),
    "exceeds ceiling"
  )
})
