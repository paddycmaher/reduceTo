test_that("item-target correlations are reproducible across repeated calls above the 10k-row sampling threshold", {
  set.seed(1)
  n <- 10001; pool <- 8
  data <- as.data.frame(matrix(rnorm(n * pool), ncol = pool))
  colnames(data) <- paste0("Item_", 1:pool)
  target <- rowMeans(data[, 1:4]) + rnorm(n, 0, 0.3)

  r1 <- reduceTo(data, n.items = 3, target = target, show.progress = FALSE)
  r2 <- reduceTo(data, n.items = 3, target = target, show.progress = FALSE)

  expect_equal(r1$item_r, r2$item_r)
  expect_equal(r1$selected$item_r, r2$selected$item_r)
})

test_that("forked children (parallel::mclapply) can search after the parent has, with the same results", {
  skip_on_os("windows")                                   # no fork() there
  skip_on_cran()
  set.seed(3); n <- 400; f <- rnorm(n)
  data <- as.data.frame(sapply(1:24, function(j) runif(1, .3, .8) * f + rnorm(n)))
  ex <- reduceTo(data, 4, show.progress = FALSE, verbose = FALSE)                       # the parent searches first
  ko <- reduceTo(data, 8, ceiling = 1e5, show.progress = FALSE, verbose = FALSE)
  kids <- parallel::mclapply(1:2, function(i) {
    e <- reduceTo(data, 4, show.progress = FALSE, verbose = FALSE); k <- reduceTo(data, 8, ceiling = 1e5, show.progress = FALSE, verbose = FALSE)
    c(e$r, k$r) }, mc.cores = 2)
  for (x in kids) expect_equal(x, c(ex$r, ko$r))          # (a crashed child returns NULL, failing this)
})
