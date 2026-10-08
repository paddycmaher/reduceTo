# Naive reference for the exact re-scoring of top combinations
reference_metrics <- function(X, y, set, na_rm, search_phi) {
  s <- rowMeans(X[, set, drop = FALSE], na.rm = na_rm) * length(set)
  r <- suppressWarnings(cor(s, y, use = "pairwise.complete.obs"))
  ok <- !is.na(s) & !is.na(y); s <- s[ok]; t <- y[ok]
  sp <- s[t == 1]; sn <- s[t == 0]
  auc <- mean(outer(sp, sn, ">") + 0.5 * outer(sp, sn, "=="))
  cuts <- floor(min(s)):ceiling(max(s))
  tp <- sapply(cuts, function(c) sum(sp >= c)); fp <- sapply(cuts, function(c) sum(sn >= c))
  tn <- length(sn) - fp; fn <- length(sp) - tp
  youden <- tp / length(sp) + tn / length(sn) - 1
  den <- sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  phi <- ifelse(den == 0, NA, (tp * tn - fp * fn) / den)
  crit <- if (search_phi) abs(phi) else youden
  b <- which(crit >= max(crit, na.rm = TRUE) - 1e-9)[1]
  c(r = r, cutoff = cuts[b], binarised_r = ifelse(is.na(phi[b]), 0, phi[b]), youden_j = youden[b], auc = auc)
}

test_that("exact re-scoring matches a naive R reference (r, cutoff, Youden's J, phi, AUC)", {
  for (seed in 1:4) {
    set.seed(seed)
    n <- 400; f <- rnorm(n)
    likert <- sapply(1:8, function(j) pmin(pmax(round(f + rnorm(n) + 3), 1), 5))
    cont <- sapply(1:8, function(j) f + rnorm(n))
    y <- as.numeric(f + rnorm(n) > 0.2)
    y[sample(n, 10)] <- NA
    for (X in list(likert, cont)) for (na in c(0, 0.2)) for (na_rm in c(TRUE, FALSE)) for (search_phi in c(FALSE, TRUE)) {
      Xm <- X; Xm[matrix(runif(length(X)) < na, n)] <- NA
      sets <- cbind(c(1, 2, 3), c(2, 5, 8), c(4, 6, 7))
      got <- reduceTo:::rescore_sets_cpp(Xm, y, matrix(as.integer(sets - 1), nrow = 3), na_rm, TRUE, search_phi)
      for (i in 1:3) {
        ref <- reference_metrics(Xm, y, sets[, i], na_rm, search_phi)
        expect_equal(c(r = got$r[i], cutoff = got$cutoff[i], binarised_r = got$binarised_r[i],
                       youden_j = got$youden_j[i], auc = got$auc[i]), ref, tolerance = 1e-12)
      }
    }
  }
})
