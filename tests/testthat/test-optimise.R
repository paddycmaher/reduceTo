test_that("optimise = FALSE forces exhaustive search, matching unconstrained exhaustive", {
  set.seed(1)
  data <- as.data.frame(matrix(rnorm(200 * 8), ncol = 8))
  colnames(data) <- paste0("Item_", 1:8)

  r_forced <- reduceTo(data, n.items = 3, ceiling = 5, optimise = FALSE, show.progress = FALSE)
  r_unconstrained <- reduceTo(data, n.items = 3, ceiling = 10^6, show.progress = FALSE)

  expect_equal(r_forced$r, r_unconstrained$r)
  expect_equal(r_forced$selected$items, r_unconstrained$selected$items)
})

test_that("optimise = TRUE (default) triggers optimisation when combinations exceed ceiling", {
  set.seed(1)
  data <- as.data.frame(matrix(rnorm(200 * 10), ncol = 10))
  colnames(data) <- paste0("Item_", 1:10)

  expect_message(
    reduceTo(data, n.items = 4, ceiling = 5, show.progress = FALSE),
    "This would generate"
  )
})

test_that("optimise = FALSE with a large problem does not trigger optimisation", {
  set.seed(1)
  data <- as.data.frame(matrix(rnorm(200 * 10), ncol = 10))
  colnames(data) <- paste0("Item_", 1:10)

  expect_message(
    reduceTo(data, n.items = 4, ceiling = 5, optimise = FALSE, show.progress = FALSE),
    "exceeds ceiling"
  )
})

test_that("searches keeping more than half the pool still score every set exactly (drop-side enumeration)", {
  set.seed(3)
  n <- 300; f <- rnorm(n)
  data <- as.data.frame(sapply(1:12, function(j) (0.2 + j / 20) * f + rnorm(n)))
  target <- f + rnorm(n)

  brute <- combn(12, 9, function(s) cor(rowSums(data[, s]), target))
  r <- reduceTo(data, n.items = 9, target = target, optimise = FALSE, show.progress = FALSE, verbose = FALSE)

  expect_equal(r$r, max(brute))
  expect_equal(sort(r$selected$indices), combn(12, 9)[, which.max(brute)])
})

test_that("keeping most of a pool under optimisation still finds the exhaustive best set", {
  # optimisation used to shrink the pool straight to n.items here, leaving the
  # final set to a k = 2 ranking with no final search at all
  set.seed(4)
  n <- 1000; f <- rnorm(n)
  data <- as.data.frame(sapply(1:24, function(j) runif(1, 0.2, 0.9) * f + rnorm(n)))

  r_opt <- reduceTo(data, n.items = 20, ceiling = 10000, show.progress = FALSE, verbose = FALSE)
  r_exh <- reduceTo(data, n.items = 20, optimise = FALSE, show.progress = FALSE, verbose = FALSE)

  expect_equal(r_opt$r, r_exh$r)
  expect_setequal(r_opt$selected$items, r_exh$selected$items)
})

test_that("the knockout search reports its searches, and the conservative mode adds an exact pass", {
  set.seed(4)
  n <- 500; f <- rnorm(n)
  data <- as.data.frame(sapply(1:24, function(j) runif(1, 0.2, 0.9) * f + rnorm(n)))

  expect_output(r_cons <- reduceTo(data, n.items = 20, ceiling = 10000, speed = "conservative", verbose = FALSE),
                "done: [0-9]+ searches")
  r_fast <- reduceTo(data, n.items = 20, ceiling = 10000, show.progress = FALSE, verbose = FALSE)
  r_exh <- reduceTo(data, n.items = 20, optimise = FALSE, show.progress = FALSE, verbose = FALSE)

  expect_equal(r_cons$r, r_exh$r)
  expect_equal(r_fast$r, r_exh$r)
  expect_gt(r_cons$search$searched, r_fast$search$searched)
})

make_facet_data <- function(seed, n = 600) {
  set.seed(seed)
  g <- rnorm(n)
  fac <- sapply(1:4, function(i) rnorm(n))
  as.data.frame(sapply(1:24, function(j) runif(1, 0.3, 0.8) * g + runif(1, 0.3, 0.9) * fac[, (j - 1) %/% 6 + 1] + rnorm(n)))
}

test_that("the knockout search finds the exhaustive best set keeping fewer or more than half the pool", {
  data <- make_facet_data(1)
  for (k in c(8, 16)) {
    r_opt <- reduceTo(data, n.items = k, ceiling = 10000, show.progress = FALSE, verbose = FALSE)
    r_exh <- reduceTo(data, n.items = k, optimise = FALSE, show.progress = FALSE, verbose = FALSE)
    expect_equal(r_opt$r, r_exh$r)
    expect_setequal(r_opt$selected$items, r_exh$selected$items)
  }
})

test_that("the knockout search works through missing data, cross-validation and binary targets", {
  data <- make_facet_data(3)
  target <- as.numeric(rowMeans(data) + rnorm(600) > 0)
  set.seed(9)
  data_na <- data
  data_na[matrix(runif(600 * 24) < 0.1, 600)] <- NA

  r_na <- reduceTo(data_na, n.items = 8, cross.validate = TRUE, ceiling = 10000,
                   item.names = TRUE, show.progress = FALSE, verbose = FALSE)
  r_bin <- reduceTo(data, n.items = 8, target = target, cross.validate = TRUE, ceiling = 10000,
                    item.names = TRUE, show.progress = FALSE, verbose = FALSE)

  for (r in list(r_na, r_bin)) {
    expect_length(r$selected$items, 8)
    expect_true(all(r$selected$items %in% names(data)))
    expect_true(all(r$selected$items %in% r$pool$searched))
    expect_false(is.na(r$top$r_holdout[1]))
  }
})

test_that("the knockout search finds a hidden synergy group that local swaps cannot", {
  # 4 items that predict the target perfectly together (their noise cancels in
  # a cycle) but are individually weak, among real decoys and pure noise. Swaps
  # can't climb towards such a group -- partial groups are worse than decoys --
  # but trimming from the full scale keeps it: removing any one of its items
  # breaks the cancellation
  set.seed(11)
  n <- 3000; target <- rnorm(n); g <- 4
  noise <- lapply(1:g, function(i) { e <- resid(lm(rnorm(n) ~ target)); 4 * e / sd(e) })
  synergy <- sapply(1:g, function(i) target + noise[[i]] - noise[[(i %% g) + 1]])
  decoys <- sapply(1:17, function(i) 0.35 * target + sqrt(1 - 0.35^2) * rnorm(n))
  data <- as.data.frame(cbind(synergy, decoys, matrix(rnorm(n * 39), n)))
  names(data) <- c(paste0("S", 1:g), paste0("D", 1:17), paste0("X", 1:39))

  r <- reduceTo(data, n.items = g, target = target, ceiling = 1e5,
                show.progress = FALSE, verbose = FALSE)
  expect_identical(r$search$type, "optimised")
  expect_setequal(r$selected$items, paste0("S", 1:g))
  expect_gt(r$r, 0.999999)
})

test_that("the result reports whether the search was exhaustive, and the knockout search's searches", {
  data <- make_facet_data(1)
  exh <- reduceTo(data[, 1:12], n.items = 4, show.progress = FALSE, verbose = FALSE)
  opt <- reduceTo(data, n.items = 8, ceiling = 10000, show.progress = FALSE, verbose = FALSE)

  expect_identical(exh$search$type, "exhaustive")
  expect_null(exh$search$searches)
  expect_identical(opt$search$type, "optimised")
  expect_s3_class(opt$search$searches, "data.frame")
  expect_true(all(opt$search$searches$combinations <= 100))
  expect_equal(sum(opt$search$searches$combinations), opt$search$searched)
  expect_equal(max(opt$search$searches$r_best), opt$r, tolerance = 1e-8)
  expect_s3_class(opt$search$loco, "data.frame")
  expect_true(all(opt$search$loco$item %in% names(data)))
  expect_output(print(opt), "Search: optimised")
})

test_that("opt.n and rfe.budget are gone", {
  data <- make_facet_data(1)
  expect_error(reduceTo(data, n.items = 8, opt.n = 100, show.progress = FALSE, verbose = FALSE), "unused argument")
  expect_error(reduceTo(data, n.items = 8, rfe.budget = 10, show.progress = FALSE, verbose = FALSE), "unused argument")
})

test_that("timings cover the whole run, and the C++ sections break down the search step", {
  data <- make_facet_data(1)
  for (args in list(list(n.items = 4), list(n.items = 8, ceiling = 10000), list(n.items = 4, speed = "conservative"),
                    list(n.items = 8, ceiling = 10000, cross.validate = TRUE))) {
    t0 <- Sys.time()
    r <- do.call(reduceTo, c(list(data), args, show.progress = FALSE, verbose = FALSE))
    wall <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    tr <- r$timings$steps; tc <- r$timings$engine
    expect_false(anyDuplicated(tr$step) > 0)
    expect_true(all(tr$s >= 0))
    expect_lt(abs(tail(tr$s, 1) - sum(head(tr$s, -1))), 1e-3)   # each row is rounded to 4 decimals
    expect_lte(tail(tr$s, 1), wall + 1e-3)
    expect_true(all(c("arguments", "search", "rescoring", "output") %in% tr$step))
    expect_false(any(grepl("^time[0-9]", rownames(tc))))
    expect_lt(abs(tc["total", "s"] - sum(tc[rownames(tc) != "total", "s"])), 1e-3)
    expect_lte(tc["total", "s"], tr$s[tr$step == "search"] + 1e-3)
  }
})

test_that("the knockout search's searches never exceed ceiling, on short and long forms and external targets", {
  data <- make_facet_data(2)
  target <- rowMeans(data[, 1:6]) + rnorm(600)
  for (k in c(5, 12, 19)) {
    r <- reduceTo(data, n.items = k, target = target, ceiling = 20000, show.progress = FALSE, verbose = FALSE)
    expect_true(all(r$search$searches$combinations <= 20000))
    expect_length(r$selected$items, k)
    expect_lte(nrow(r$leaderboard), 100)
  }
})

test_that("knockout searches are capped at ceiling / 1000", {
  data <- make_facet_data(2)
  r <- reduceTo(data, n.items = 10, ceiling = 1e6, show.progress = FALSE, verbose = FALSE)
  expect_identical(r$search$type, "optimised")
  expect_true(all(r$search$searches$combinations <= 1000))
  expect_true(any(r$search$searches$combinations > 100))
})

test_that("optimise = TRUE runs a knockout search on any problem, and NULL only above ceiling", {
  data <- make_facet_data(2)
  r_auto <- reduceTo(data, n.items = 4, show.progress = FALSE, verbose = FALSE)
  r_forced <- reduceTo(data, n.items = 4, optimise = TRUE, show.progress = FALSE, verbose = FALSE)
  expect_identical(r_auto$search$type, "exhaustive")
  expect_identical(r_forced$search$type, "optimised")
  expect_equal(r_forced$r, r_auto$r)
  expect_null(r_auto$settings$optimise)
  expect_true(r_forced$settings$optimise)
  expect_message(reduceTo(data, n.items = 4, optimise = TRUE, show.progress = FALSE), "With optimise = TRUE, a knockout search")
  # small pools and one-item forms still work; a single possible combination is just scored
  for (k in c(1, 2, 23)) {
    r <- reduceTo(data[, 1:if (k == 2) 3 else 24], n.items = k, optimise = TRUE, show.progress = FALSE, verbose = FALSE)
    expect_identical(r$search$type, "optimised")
    expect_length(r$selected$items, k)
  }
  r_all <- reduceTo(data[, 1:6], n.items = 6, optimise = TRUE, show.progress = FALSE, verbose = FALSE)
  expect_identical(r_all$search$type, "exhaustive")
})

test_that("knockout.budget overrides ceiling / 1000", {
  data <- make_facet_data(2)
  r <- reduceTo(data, n.items = 10, optimise = TRUE, knockout.budget = 500, show.progress = FALSE, verbose = FALSE)
  expect_true(all(r$search$searches$combinations <= 500))
  expect_true(any(r$search$searches$combinations > 50))
  expect_equal(r$settings$knockout.budget, 500)
  expect_equal(reduceTo(data, n.items = 4, show.progress = FALSE, verbose = FALSE)$settings$knockout.budget, 1e9)
  expect_error(reduceTo(data, n.items = 4, knockout.budget = 0, show.progress = FALSE), "knockout.budget")
  expect_error(reduceTo(data, n.items = 4, optimise = "yes", show.progress = FALSE), "optimise")
})

test_that("the sweepdown: ten times the budget never returns a worse set", {
  # many near-equal items: several near-equal best sets far apart. On their own, the searches at budgets 10 and
  # 100 settle on different ones, and the larger budget's is worse
  set.seed(264)
  n <- 300; g <- rnorm(n); fac <- sapply(1:4, function(i) rnorm(n))
  X <- sapply(1:24, function(j) runif(1, 0.4, 0.6) * g + runif(1, 0.2, 0.5) * fac[, (j - 1) %% 4 + 1] + rnorm(n))
  mom <- reduceTo:::gram_moments_cpp(X, rowMeans(X))
  ko <- function(b, sweepdown) reduceTo:::knockout_cpp(mom$C, mom$tt, mom$ssy, 9L, b, show_progress = FALSE, sweepdown = sweepdown)
  single <- sapply(10^(0:4), function(b) ko(b, FALSE)$r[1])
  sw <- lapply(10^(0:4), function(b) ko(b, TRUE))
  r_sw <- sapply(sw, function(x) x$r[1])
  expect_true(all(diff(r_sw) >= -1e-14))
  expect_true(all(r_sw >= cummax(single) - 1e-14))
  # every budget's searches are listed, the sweepdown's included: 10^4, 10^3, ..., 1
  expect_setequal(unique(sw[[5]]$rounds$budget), 10^(4:0))
  expect_equal(sum(sw[[5]]$rounds$combinations), sw[[5]]$searched)
  # the same again, search for search: what the sweepdown reads off never depends on how the threads ran
  again <- ko(1e4, TRUE)
  expect_identical(again$rounds[names(again$rounds) != "seconds"], sw[[5]]$rounds[names(sw[[5]]$rounds) != "seconds"])
  expect_identical(again$sets, sw[[5]]$sets)
  # without the sweepdown, the search at the budget alone
  expect_identical(unique(ko(1e4, FALSE)$rounds$budget), 1e4)
})

test_that("the knockout search lists its searches with their budgets", {
  data <- make_facet_data(2)
  r <- reduceTo(data, n.items = 10, optimise = TRUE, knockout.budget = 1000, show.progress = FALSE, verbose = FALSE)
  expect_true(all(c(1000, 100, 10, 1) %in% r$search$searches$budget))
  expect_true(all(r$search$searches$combinations <= r$search$searches$budget))
  expect_equal(max(r$search$searches$r_best), r$r, tolerance = 1e-8)
})
