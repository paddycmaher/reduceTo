# The C++ data-preparation kernels (src/prep.cpp) against plain-R equivalents

make_prep_data <- function(n = 400, p = 12, miss = 0.08, seed = 21) {
  set.seed(seed)
  m <- matrix(rnorm(n * p, mean = 3, sd = 1.5), n, p, dimnames = list(NULL, paste0("V", seq_len(p))))
  m[sample(length(m), round(miss * length(m)))] <- NA
  m
}

test_that("column_scan_cpp gives count, min, max, sum and the infinite flag, skipping NA and NaN", {
  m <- make_prep_data()
  m[1, 2] <- NaN; m[5, 3] <- Inf; m[, 4] <- 7; m[, 5] <- NA
  s <- reduceTo:::column_scan_cpp(m)
  fin <- function(x) x[!is.na(x) & is.finite(x)]
  expect_equal(s$n, unname(apply(m, 2, function(x) length(fin(x)))))
  expect_equal(s$sum, unname(apply(m, 2, function(x) sum(fin(x)))))
  expect_equal(s$has_inf, as.integer(apply(m, 2, function(x) any(is.infinite(x)))))
  ok <- s$n > 0
  expect_equal(s$min[ok], unname(apply(m[, ok], 2, function(x) min(fin(x)))))
  expect_equal(s$max[ok], unname(apply(m[, ok], 2, function(x) max(fin(x)))))
  # "usable" (n > 0, nothing infinite, max > min) is the same as the old per-column test
  usable <- s$n > 0 & s$has_inf == 0 & s$max > s$min
  old <- apply(m, 2, function(x) !all(is.na(x)) && !any(is.infinite(x)) && isTRUE(sd(x, na.rm = TRUE) > 0))
  expect_identical(usable, unname(old))

  rows <- c(3L, 10L, 11L, 50L, 200L)               # a row subset, and a column subset
  s2 <- reduceTo:::column_scan_cpp(m, rows, c(1L, 6L, 7L))
  sub <- m[rows, c(1, 6, 7)]
  expect_equal(s2$n, unname(apply(sub, 2, function(x) sum(!is.na(x)))))
  expect_equal(s2$max, unname(apply(sub, 2, max, na.rm = TRUE)))
})

test_that("fused_gram_cpp equals the cross-products of the mean-imputed data, centred", {
  m <- make_prep_data()
  targ <- rowMeans(m, na.rm = TRUE); targ[c(4, 90)] <- NA        # rows without a target are left out
  mu <- colMeans(m, na.rm = TRUE)
  Y <- m; Y[is.na(Y)] <- rep(mu, each = nrow(m))[is.na(Y)]
  Y <- sweep(Y, 2, mu)                                            # centred on mu, the target on its own mean
  v <- !is.na(targ); Yv <- Y[v, ]; tv <- targ[v] - mean(targ[v])
  g <- reduceTo:::fused_gram_cpp(m, targ, mu)
  expect_equal(g$gram, crossprod(Yv), tolerance = 1e-12)
  expect_equal(unname(g$col_sums), unname(colSums(Yv)), tolerance = 1e-12)
  expect_equal(unname(g$col_target_dots), as.vector(crossprod(Yv, tv)), tolerance = 1e-12)
  expect_equal(g$sum_target, sum(tv)); expect_equal(g$sum_target_sq, sum(tv^2)); expect_equal(g$n_valid, sum(v))
  expect_identical(dimnames(g$gram), list(colnames(m), colnames(m)))

  rows <- sort(sample(nrow(m), 150))                              # over a row subset, with no target at all
  g2 <- reduceTo:::fused_gram_cpp(m, numeric(0), mu, rows)
  expect_equal(g2$gram, crossprod(Y[rows, ]), tolerance = 1e-12)
  expect_equal(g2$n_valid, 150)
  expect_equal(sum(g2$col_target_dots), 0)
  expect_error(reduceTo:::fused_gram_cpp(m, numeric(0), mu, c(1L, NA)), "out of range")
})

test_that("the cross-products keep their accuracy far from zero, and bad indices stop rather than crash", {
  set.seed(5); n <- 2000; f <- rnorm(n)
  m <- sapply(1:8, function(j) round(4 + runif(1, .3, .9) * f + rnorm(n))); colnames(m) <- paste0("V", 1:8)
  m[sample(length(m), 800)] <- NA
  cv_of <- function(x) { sc <- reduceTo:::column_scan_cpp(x); g <- reduceTo:::fused_gram_cpp(x, numeric(0), sc$sum / sc$n)
                         g$gram - outer(g$col_sums, g$col_sums) / g$n_valid }
  expect_equal(cv_of(m + 1e8), cv_of(m), tolerance = 1e-6)        # uncentred, these came out negative (and R crashed)
  r0 <- reduceTo(m, 3, show.progress = FALSE, verbose = FALSE); r8 <- reduceTo(m + 1e8, 3, show.progress = FALSE, verbose = FALSE)
  expect_identical(r8$selected$items, r0$selected$items); expect_equal(r8$r, r0$r, tolerance = 1e-8)
  expect_error(reduceTo:::column_scan_cpp(m, integer(0), c(1L, NA)), "out of range")
  expect_error(reduceTo:::column_scan_cpp(m, c(0L, 5L)), "out of range")
})

test_that("the correlation matrix from the fused Gram matches centring then crossprod", {
  m <- make_prep_data(n = 500, p = 15)
  rows <- sort(sample(nrow(m), 300))
  old_cor <- function(x) { xc <- sweep(x, 2, colMeans(x, na.rm = TRUE)); xc[is.na(xc)] <- 0; cv <- crossprod(xc); s <- sqrt(diag(cv)); cv / outer(s, s) }
  new_cor <- function(x, rows = integer(0)) {
    sc <- reduceTo:::column_scan_cpp(x, rows); g <- reduceTo:::fused_gram_cpp(x, numeric(0), sc$sum / sc$n, rows)
    cv <- g$gram - outer(g$col_sums, g$col_sums) / g$n_valid; s <- sqrt(diag(cv)); cv / outer(s, s)
  }
  expect_equal(unname(new_cor(m)), unname(old_cor(m)), tolerance = 1e-10)
  expect_equal(unname(new_cor(m, rows)), unname(old_cor(m[rows, ])), tolerance = 1e-10)
})

test_that("flip_columns_cpp reverses the chosen columns in a copy and never touches its input", {
  m <- make_prep_data()
  keep <- m
  out <- reduceTo:::flip_columns_cpp(m, c(2L, 5L, 9L), c(10, 20, 30))
  expect_identical(m, keep)                                       # the caller's matrix is unchanged
  expect_equal(out[, 2], 10 - m[, 2]); expect_equal(out[, 5], 20 - m[, 5]); expect_equal(out[, 9], 30 - m[, 9])
  expect_equal(out[, -c(2, 5, 9)], m[, -c(2, 5, 9)])
  expect_identical(is.na(out), is.na(m)); expect_identical(dimnames(out), dimnames(m))
  expect_error(reduceTo:::flip_columns_cpp(m, 99L, 1), "out of range")
})

test_that("reduceTo() leaves the matrix it is given untouched, whatever it reverses", {
  set.seed(30); n <- 300; f <- rnorm(n)
  m <- sapply(1:10, function(j) runif(1, .3, .8) * f * c(1, -1)[1 + (j %in% c(2, 5))] + rnorm(n))
  colnames(m) <- paste0("V", 1:10); m[sample(length(m), 80)] <- NA
  keep <- m
  for (args in list(list(n.items = 4), list(n.items = 4, target = f), list(n.items = 4, scale.vars = TRUE), list(n.items = 4, cross.validate = TRUE))) {
    r <- do.call(reduceTo, c(list(m), args, show.progress = FALSE, verbose = FALSE))
    expect_identical(m, keep)
  }
  r9 <- reduceTo(m, 9, show.progress = FALSE, verbose = FALSE)      # 9 of 10 must include a reversed item (2 or 5)
  expect_true(any(r9$selected$keys < 0))
  expect_identical(m, keep)
})

test_that("integer and mixed-type input go through the same checks", {
  set.seed(31); n <- 250; f <- rnorm(n)
  ints <- as.data.frame(sapply(1:8, function(j) pmin(5L, pmax(1L, as.integer(round(runif(1, .4, .9) * f + rnorm(n) + 3))))))
  dbl <- ints; dbl[] <- lapply(dbl, as.numeric)
  r1 <- reduceTo(ints, 3, show.progress = FALSE, verbose = FALSE); r2 <- reduceTo(dbl, 3, show.progress = FALSE, verbose = FALSE)
  expect_identical(r1$selected$items, r2$selected$items); expect_equal(r1$r, r2$r)
  mixed <- cbind(dbl, label = "x", flag = TRUE, const = 2)
  r3 <- reduceTo(mixed, 3, show.progress = FALSE, verbose = FALSE)
  expect_identical(r3$selected$items, r2$selected$items)
  expect_true(all(c("label", "const") %in% setdiff(r3$pool$original, r3$pool$valid)))
})
