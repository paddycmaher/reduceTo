test_that("progressive narrowing matches true exhaustive search (speed = 'fast')", {
  set.seed(1)
  n <- 300; pool <- 18
  data <- as.data.frame(matrix(rnorm(n * pool), ncol = pool))
  colnames(data) <- paste0("Item_", 1:pool)
  target <- rowMeans(data[, 1:6]) + rnorm(n, 0, 0.3)

  r_prog <- reduceTo(data, n.items = 4, target = target, ceiling = 100,
                     optimise = "progressive", speed = "fast", show.progress = FALSE)
  r_truth <- reduceTo(data, n.items = 4, target = target, optimise = "none",
                      ceiling = choose(pool, 4) + 1, show.progress = FALSE)

  expect_equal(r_prog$r, r_truth$r, tolerance = 1e-6)
  expect_equal(sort(r_prog$best_names), sort(r_truth$best_names))
})

test_that("progressive narrowing matches true exhaustive search (speed = 'conservative')", {
  set.seed(1)
  n <- 300; pool <- 18
  data <- as.data.frame(matrix(rnorm(n * pool), ncol = pool))
  colnames(data) <- paste0("Item_", 1:pool)
  target <- rowMeans(data[, 1:6]) + rnorm(n, 0, 0.3)

  r_prog <- reduceTo(data, n.items = 4, target = target, ceiling = 100,
                     optimise = "progressive", speed = "conservative", show.progress = FALSE)
  r_truth <- reduceTo(data, n.items = 4, target = target, optimise = "none",
                      ceiling = choose(pool, 4) + 1, show.progress = FALSE)

  expect_equal(r_prog$r, r_truth$r, tolerance = 1e-6)
  expect_equal(sort(r_prog$best_names), sort(r_truth$best_names))
})

test_that("progressive narrowing is the default optimise method", {
  set.seed(1)
  data <- as.data.frame(matrix(rnorm(200 * 10), ncol = 10))
  colnames(data) <- paste0("Item_", 1:10)

  expect_message(
    reduceTo(data, n.items = 4, ceiling = 5, show.progress = FALSE),
    "This task would generate"
  )
})

test_that("progressive narrowing never narrows the pool below n.items", {
  set.seed(1)
  n <- 300; pool <- 15
  data <- as.data.frame(matrix(rnorm(n * pool), ncol = pool))
  colnames(data) <- paste0("Item_", 1:pool)

  r <- reduceTo(data, n.items = 5, ceiling = 3, optimise = "progressive", show.progress = FALSE)

  expect_true(length(r$final_pool_items) >= 5)
  expect_length(r$best_names, 5)
})

test_that("progressive narrowing recovers a synergistic/suppressor item set", {
  # X1-3 and Y1-3 individually correlate weakly with the target (a shared bias
  # W dominates), but together cancel W and jointly predict it well; D items
  # are moderate, honest distractors. A method that only looks at items in
  # isolation should be tempted to pick distractors over the X/Y pair.
  set.seed(42)
  n <- 2000
  Z1 <- rnorm(n); Z2 <- rnorm(n); W <- rnorm(n) * 2.5
  pool <- 30

  data <- matrix(ncol = pool, nrow = n)
  for (i in 1:3) data[, i] <- Z1 - W + rnorm(n) * 0.4
  for (i in 4:6) data[, i] <- Z2 + W + rnorm(n) * 0.4
  for (i in 7:pool) data[, i] <- 0.4 * (Z1 + Z2) + rnorm(n) * 0.9
  colnames(data) <- c(paste0("X", 1:3), paste0("Y", 1:3), paste0("D", 1:(pool - 6)))
  data <- as.data.frame(data)

  # Shuffle columns so index order can't leak the answer
  set.seed(99)
  data <- data[, sample(ncol(data))]

  target <- Z1 + Z2

  r <- reduceTo(data, n.items = 6, target = target, ceiling = 50,
               optimise = "progressive", show.progress = FALSE)

  n_xy <- sum(grepl("^[XY]", r$best_names))
  expect_true(n_xy >= 4)
})
