#' Find Optimal Short-Form Scales
#'
#' Systematically evaluates all possible subsets of items from a larger scale to 
#' find the combination that maximises either internal consistency or correlation 
#' with an external criterion. Runs on a parallelised C++ backend with 
#' memory-optimised data structures (8-bit compression) to evaluate hundreds of 
#' millions of combinations in seconds.
#'
#' @details
#' \strong{Key Features:}
#' \itemize{
#'   \item \strong{Combinatorial Search}: Exhaustively scores item subsets to guarantee finding the best-performing item set within the search space.
#'   \item \strong{Heuristic Optimisation}: When the number of combinations exceeds the computational ceiling, the function automatically reduces the item pool using Synergy-Ranked Recursive Feature Elimination before searching.
#'   \item \strong{Cross-Validation}: Supports a Train/Holdout split (default 75/25) to validate findings and prevent overfitting. Reports performance metrics for both the training and holdout samples side-by-side.
#'   \item \strong{Binary Classifications}: For binary targets (0/1), automatically finds the optimal integer cut-off score to maximise classification accuracy (Youden's J) or binarised correlation. AUC (threshold-independent) is also reported.
#' }
#'
#' @param data Matrix or data.frame containing item responses
#' @param n.items Desired number of items in the final short-form scale
#' @param target Optional target criterion. Can be specified as: NULL (default) for 
#'   internal consistency, an unquoted column name from data, or an external vector 
#'   (numeric criterion or binary 0/1 for classification)
#' @param n.sets Number of top-performing item sets to return (default: 5)
#' @param item.names If TRUE, output lists item names instead of column numbers (default: FALSE)
#' @param r.sq If TRUE, returns R² alongside correlation (default: FALSE)
#' @param generate If TRUE, returns computed scores for selected item set (default: TRUE)
#' @param item.set Which ranked set to generate scores for, used with generate = TRUE (default: 1)
#' @param show.progress If TRUE, displays a live progress bar during search (default: TRUE)
#' @param cross.validate Numeric input controlling a data split into Training/Holdout
#'   sets; if TRUE or 1, uses a 75%/25% split (default: FALSE)
#' @param optimise If TRUE (default), runs the Synergistic RFE optimisation algorithm
#'   when combinations exceed \code{ceiling} -- exhaustively scoring small-k combinations
#'   and progressively narrowing the item pool, keeping the items with the best achieved
#'   score at each step. When keeping more than half the pool (e.g. 50 of 60), it works
#'   from the other side: scoring the scale with small-k sets of items removed, and keeping
#'   outright the items whose removal always hurts. The best sets found are then polished
#'   by swapping 1 or 2 items with any item in the pool (recovering items that narrowing
#'   removed), and by deterministic escapes that strip opposite ends of the item residuals'
#'   main directions together and rebuild. If FALSE, forces exhaustive search regardless
#'   of \code{ceiling} (can be slow)
#' @param prefilter.ratio Drops items whose relevance (correlation with the target, or
#'   item-total centrality with no target) is more than \code{prefilter.ratio} times
#'   weaker than the strongest item, before optimisation runs. Only applies when at least
#'   twice \code{n.items} items survive, so the search still has real choices left. Set to
#'   \code{Inf} or \code{NULL} to disable (default: 5)
#' @param opt.n The maximum number of cases (rows) to subsample during heuristic
#'   optimisation (default: 20000). Raising this improves both average recovered
#'   r and run-to-run consistency at negligible time cost, since the optimisation
#'   stage's cost is typically dominated by \code{ceiling}/\code{rfe.budget}
#'   rather than row count
#' @param ceiling Combination threshold triggering optimisation (default: 100,000,000;
#'   10,000,000 with \code{speed = "conservative"}).
#'   A tighter ceiling narrows the item pool further before the final exhaustive search,
#'   which is faster but leaves less room to recover from an imperfect ranking. In
#'   testing, Synergistic RFE reliably found the true optimum except in
#'   extreme scenarios -- e.g. items whose value is invisible until combined with several
#'   (3+) specific others; a more generous ceiling protects against this rare case
#' @param rfe.budget Combination threshold per intermediate Synergistic RFE round
#'   (default: 100,000,000; 10,000,000 with \code{speed = "conservative"}), as opposed
#'   to \code{ceiling}, which bounds only the
#'   final search. Raising it lets each narrowing round exhaustively score a larger
#'   slice of the pool before dropping items, which can recover items that a
#'   tighter round would have pruned too early -- at a real, roughly linear time
#'   cost per round. Empirically, gains taper off well before \code{ceiling}-scale
#'   values; raising it far beyond the default is rarely worth the extra time
#' @param scale.vars If TRUE, mean-centers and scales all columns, using the training
#'   rows' means/SDs when cross-validating (default: FALSE)
#' @param na.rm If TRUE, handles missing values via pairwise deletion (default: TRUE)
#' @param method Metric for ranking combinations (default: NULL for auto-selection):
#'   "r" for Pearson correlation, "youden_j" for Youden's Index, "binarised_r"
#'   for correlation of binarised sum score, or "auc" for the area under the
#'   ROC curve (binary targets only; ignored otherwise)
#' @param speed \code{"fast"} (default) scores combinations using a Gram-matrix
#'   shortcut, mean-imputing missing values for search only -- reported statistics are
#'   always recomputed from the true data. \code{"conservative"} scores every
#'   combination directly with pairwise deletion: no imputation, but slower (so its
#'   \code{ceiling} and \code{rfe.budget} defaults are 10x tighter). Recommended when
#'   \code{na.rm = FALSE} and the data have substantial or patterned missingness (e.g.
#'   whole sections skipped), where each combination is scored on its own complete cases
#'   and mean-imputed search can miss the best set
#' @param verbose If TRUE, prints informational messages (default: TRUE). Progress
#'   bars and optimisation-stage updates are controlled separately by \code{show.progress}
#'
#' @return A list of class \code{reduced_scale} containing:
#' \describe{
#'   \item{output}{Data frame of the top n.sets combinations and their performance metrics}
#'   \item{leaderboard}{Extended data frame of the top combinations (100, or 1000 when ranking by Youden's J, AUC or binarised r)}
#'   \item{best_names}{Character vector of item names in the top-ranked set}
#'   \item{best_indices}{Integer vector of column indices in the top-ranked set}
#'   \item{scores}{(If generate = TRUE) A data frame containing the sum scores}
#'   \item{binary_info}{(If binary target) A list containing optimal cutoffs and classification metrics}
#'   \item{search}{List: \code{type} ("exhaustive", the guaranteed best set, or "optimised",
#'     heuristic), \code{combinations} (possible sets), \code{searched} (sets scored by the
#'     final exhaustive search) and, when optimised, \code{polish} (best r before and after
#'     polishing, and the items swapped in and out)}
#'   \item{params}{Named vector of parameters used in the function call}
#' }
#'
#' @author Paddy Maher, Max Planck Institute for Human Development, MPRG Biosocial
#' @export
#' @useDynLib reduceTo, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom stats cor median sd setNames
#' @importFrom utils flush.console head
#' @import RcppParallel
#'
#' @examples
#' \donttest{
#' # Create a simple simulated dataset
#' set.seed(123)
#' data <- as.data.frame(matrix(rnorm(200 * 10), ncol = 10))
#' colnames(data) <- paste0("Item_", 1:10)
#' 
#' # Internal Consistency Optimisation
#' results_scale <- reduceTo(data, n.items = 5)
#' print(results_scale)
#' 
#' # Criterion Validity Optimisation (Binary Target)
#' target <- ifelse(rowMeans(data) > 0, 1, 0)
#' results_binary <- reduceTo(data, n.items = 3, target = target, cross.validate = TRUE)
#' print(results_binary)
#' }

reduceTo <- function(data, n.items, target = NULL, n.sets = 5, item.names = FALSE, r.sq = FALSE,
                     generate = TRUE, item.set = 1, show.progress = TRUE, cross.validate = 0,
                     optimise = TRUE, prefilter.ratio = 5,
                     opt.n = 20000, ceiling = 1e8, rfe.budget = 1e8,
                     scale.vars = FALSE, na.rm = TRUE, method = NULL, speed = c("fast", "conservative"),
                     verbose = TRUE){

  speed <- match.arg(speed)

  # The row-scan engine re-reads every row for every combination, so it
  # keeps the previous 10x tighter budgets unless they're set explicitly
  if (speed == "conservative") {
    if (missing(ceiling)) ceiling <- 1e7
    if (missing(rfe.budget)) rfe.budget <- 1e7
  }

  # Whole-number arguments: n.items = 0 used to return an empty result
  # silently, and non-integers were quietly rounded deep inside choose()
  is_whole <- function(x) is.numeric(x) && length(x) == 1 && !is.na(x) && x >= 1 && x == round(x)
  if (!is_whole(n.items)) stop("'n.items' must be a single whole number >= 1.")
  if (!is_whole(n.sets)) stop("'n.sets' must be a single whole number >= 1.")
  if (!is_whole(item.set)) stop("'item.set' must be a single whole number >= 1.")

  # Force `data` to evaluate NOW, before the random-state snapshot below.
  # R evaluates function arguments lazily, so without this, an expression
  # like reduceTo(sample_n(pool, n), ...) wouldn't actually run sample_n()
  # until data's first real use much later in the function body -- AFTER
  # the snapshot already captured the pre-sample_n() random state. Since
  # that snapshot gets restored on exit, the caller's visible RNG stream
  # would never reflect sample_n()'s own draw, making it silently return
  # the identical sample on every call.
  force(data)

  # Preserve the caller's random state (reduceTo() seeds its own sampling internally)
  if (exists(".Random.seed", envir = globalenv())) {
    old_seed <- get(".Random.seed", envir = globalenv())
    on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
  } else {
    # Fresh session with no seed yet: remove the one our own set.seed(1)
    # calls leave behind, or every random draw the caller makes afterwards
    # would silently continue from set.seed(1)'s stream (identical results
    # across fresh sessions -- e.g. a simulation run after a reduceTo() call)
    on.exit(if (exists(".Random.seed", envir = globalenv())) rm(".Random.seed", envir = globalenv()), add = TRUE)
  }

  # ============================================================================
  # HELPER FUNCTIONS
  # ============================================================================
  
  # Validate that a column is usable (numeric, has variance, no infinite values)
  is_valid_column <- function(x) {
    is.numeric(x) && !all(is.na(x)) && !any(is.infinite(x)) && sd(x, na.rm = TRUE) > 0
  }
  
  # Reverse-score items negatively correlated with the target
  flip_items <- function(x, the_items, ref_rows = seq_len(nrow(x))) {

    should_flip <- if(is.logical(the_items)) any(the_items, na.rm = TRUE) else length(the_items) > 0

    if (should_flip) {
      if (scale.vars) {
        x[, the_items] <- x[, the_items] * -1
      } else {
        # (min + max) - x, per column -- conventional reverse-keying, so a 1-5
        # item stays on 1-5 (plain max - x shifted it to 0-4, leaving returned
        # sum scores and binary cutoffs offset by one point per reversed item
        # from how users score the scale themselves). Uses training-set
        # bounds (ref_rows) so holdout data flips consistently
        ref <- x[ref_rows, the_items, drop = FALSE]
        col_bounds <- apply(ref, 2, min, na.rm = TRUE) + apply(ref, 2, max, na.rm = TRUE)
        cols_to_mod <- x[, the_items, drop = FALSE]
        x[, the_items] <- -1 * sweep(cols_to_mod, 2, col_bounds, "-")
      }
    }
    return(x)
  }

  # Pearson correlation matrix of the columns of x, from mean-imputed data
  # via one BLAS crossprod() -- fast enough to use ALL rows (cor(use = "p")
  # handles missingness per pair, which makes a full p x p matrix slow at
  # large N, hence the old 2000-row subsample -- small enough to orient weak
  # items the wrong way)
  imputed_cor <- function(x) {
    xc <- sweep(x, 2, colMeans(x, na.rm = TRUE))
    xc[is.na(xc)] <- 0
    cv <- crossprod(xc)
    sds <- sqrt(diag(cv))
    cv / outer(sds, sds)
  }

  # Mean-impute missing values, then precompute the column moments (Gram
  # matrix, sums, target dot products) needed to score any item combination
  build_gram_components <- function(data, targ) {
    col_means <- colMeans(data, na.rm = TRUE)
    search_data <- data
    na_mask <- is.na(search_data)
    search_data[na_mask] <- rep(col_means, each = nrow(search_data))[na_mask]

    valid_rows <- !is.na(targ)
    sd_valid <- search_data[valid_rows, , drop = FALSE]
    targ_valid <- targ[valid_rows]

    list(
      gram = crossprod(sd_valid),
      col_sums = colSums(sd_valid),
      col_target_dots = as.vector(crossprod(sd_valid, targ_valid)),
      sum_target = sum(targ_valid),
      sum_target_sq = sum(targ_valid^2),
      n_valid = length(targ_valid)
    )
  }

  # Moments for scoring "a fixed base block of items, plus (sign = 1) or minus
  # (sign = -1) k chosen items" with the ordinary Gram engine. Every
  # combination holds exactly k items, so the base block's own moments can be
  # spread evenly over them: the engine's plain sum over the k chosen items
  # then reproduces base +/- sum(chosen) exactly, with no C++ changes
  fold_base_into_moments <- function(gc, chosen, base, k, sign) {
    C <- gc$gram - outer(gc$col_sums, gc$col_sums) / gc$n_valid
    tt <- gc$col_target_dots - gc$col_sums * gc$sum_target / gc$n_valid
    gram <- C[chosen, chosen, drop = FALSE] + sum(C[base, base]) / k^2
    diag(gram) <- diag(gram) + 2 * sign * rowSums(C[chosen, base, drop = FALSE])
    list(gram = gram, col_sums = rep(0, length(chosen)),
         col_target_dots = sign * tt[chosen] + sum(tt[base]) / k)
  }

  # Swap polish: narrowing's cuts are permanent, so a true item it removed (or
  # kept outright) never reaches the final search. Starting from the best sets
  # found, hill-climb over the WHOLE pool with 1-item swaps, then 2-item swaps,
  # each scored exactly from the same moments as the search in O(1), until no
  # swap improves. 2-item swaps only pair up the CANDIDATE_LIST items on each
  # side whose best single swap comes closest -- same answers on the tested
  # pools, but ~40,000 candidates per step instead of up to 38 million (50 of
  # 300 items: 14s -> 0.5s).
  #
  # A climb stops at a set no 1- or 2-item swap improves, typically 5-7 items
  # from the true best, mostly as different siblings WITHIN facets. Geometrically,
  # r^2 = T^2 / (T^2 + ssy |E|^2) with T the summed item-target signal and E the
  # summed item residuals perpendicular to the target: the best set's residuals
  # cancel. A stuck set is a balanced constellation of the wrong siblings, and
  # any single swap unbalances it. So escape along the residual map's principal
  # axes (and the diagonals between them): remove the selected items at BOTH
  # poles together -- keeping the balance -- rebuild with them forbidden, then
  # let everything compete again. Deterministic; exact optimum in 98.6% of 435
  # benchmark runs (vs 79.7% for climbing alone). Returns each polished set as
  # an index string, best first
  polish_by_swaps <- function(data, targ, idx, starts) {
    MAX_STEPS <- 100           # safety cap; converges in a few steps from good starts
    CHUNK <- 1e6               # 2-swap candidates scored per block (bounds memory)
    CANDIDATE_LIST <- 20       # items per side considered for 2-item swaps
    N_AXES <- 3                # residual axes searched, plus the diagonals between them
    MAX_STRIP <- 3             # items stripped from EACH pole per escape
    MAX_PASSES <- 10           # escape passes (each restarts from any improvement)
    gc <- build_gram_components(data, targ)
    C <- gc$gram - outer(gc$col_sums, gc$col_sums) / gc$n_valid
    tt <- gc$col_target_dots - gc$col_sums * gc$sum_target / gc$n_valid
    ssy <- gc$sum_target_sq - gc$sum_target^2 / gc$n_valid
    dC <- diag(C); p <- ncol(data)
    # |r| up to the constant sqrt(ssy)
    score <- function(Tn, Qn) { s <- abs(Tn) / sqrt(pmax(Qn, 0)); s[!(Qn > 0)] <- -Inf; s }
    score_set <- function(S) score(sum(tt[S]), sum(C[S, S]))

    climb <- function(S, forbid = integer(0)) {
      for (step in seq_len(MAX_STEPS)) {
        O <- setdiff(seq_len(p), c(S, forbid))
        if (length(O) == 0) break
        a <- rowSums(C[, S, drop = FALSE]); Tn <- sum(tt[S]); Q <- sum(a[S]); s0 <- score(Tn, Q)

        # Remove i, add j: T - t_i + t_j, Q - 2a_i + C_ii + 2a_j - 2C_ij + C_jj
        s1 <- score(outer(Tn - tt[S], tt[O], "+"),
                    outer(Q - 2 * a[S] + dC[S], 2 * a[O] + dC[O], "+") - 2 * C[S, O, drop = FALSE])
        if (max(s1) > s0 * (1 + 1e-12)) {
          w <- arrayInd(which.max(s1), dim(s1)); S <- c(S[-w[1]], O[w[2]]); next
        }

        if (length(S) < 2 || length(O) < 2) break
        Sc <- if (length(S) > CANDIDATE_LIST) S[order(apply(s1, 1, max), decreasing = TRUE)[1:CANDIDATE_LIST]] else S
        Oc <- if (length(O) > CANDIDATE_LIST) O[order(apply(s1, 2, max), decreasing = TRUE)[1:CANDIDATE_LIST]] else O
        ip <- utils::combn(length(Sc), 2); jp <- utils::combn(length(Oc), 2)
        j1 <- Oc[jp[1, ]]; j2 <- Oc[jp[2, ]]
        add_j <- 2 * a[j1] + dC[j1] + 2 * a[j2] + dC[j2] + 2 * C[cbind(j1, j2)]
        best <- s0 * (1 + 1e-12); best_swap <- NULL
        rows_per_chunk <- max(1, floor(CHUNK / length(j1)))
        for (from in seq(1, ncol(ip), by = rows_per_chunk)) {
          cols_i <- from:min(from + rows_per_chunk - 1, ncol(ip))
          i1 <- Sc[ip[1, cols_i]]; i2 <- Sc[ip[2, cols_i]]
          # Remove i1, i2 then add j1, j2 (cross terms with the removed pair subtracted)
          Q_rem <- Q - 2 * a[i1] - 2 * a[i2] + dC[i1] + dC[i2] + 2 * C[cbind(i1, i2)]
          cross <- C[i1, j1, drop = FALSE] + C[i2, j1, drop = FALSE] + C[i1, j2, drop = FALSE] + C[i2, j2, drop = FALSE]
          s2 <- score(outer(Tn - tt[i1] - tt[i2], tt[j1] + tt[j2], "+"), outer(Q_rem, add_j, "+") - 2 * cross)
          if (max(s2) > best) {
            w <- arrayInd(which.max(s2), dim(s2)); best <- max(s2)
            best_swap <- c(i1[w[1]], i2[w[1]], j1[w[2]], j2[w[2]])
          }
        }
        if (is.null(best_swap)) break
        S <- c(setdiff(S, best_swap[1:2]), best_swap[3:4])
      }
      sort(S)
    }

    polished <- lapply(strsplit(starts, ","), function(z) climb(match(as.numeric(z), idx)))
    best <- polished[[which.max(vapply(polished, score_set, numeric(1)))]]
    k <- length(best)

    # Residual-compass escapes (needs room to rebuild, and a usable residual map)
    if (k >= 2 && p - k >= 2 && isTRUE(ssy > 0)) {
      E_res <- C - outer(tt, tt) / ssy
      sd_res <- sqrt(pmax(diag(E_res), 1e-300))
      axes <- eigen(E_res / outer(sd_res, sd_res), symmetric = TRUE)$vectors[, seq_len(min(N_AXES, p - 1)), drop = FALSE]
      bearings <- lapply(seq_len(ncol(axes)), function(d) axes[, d])
      if (ncol(axes) >= 2) for (u in 1:(ncol(axes) - 1)) for (v in (u + 1):ncol(axes)) for (sg in c(1, -1))
        bearings[[length(bearings) + 1]] <- (axes[, u] + sg * axes[, v]) / sqrt(2)

      rebuild <- function(S, forbid) {        # greedy refill to k with `forbid` excluded, then climb
        while (length(S) < k) {
          O <- setdiff(seq_len(p), c(S, forbid))
          a <- if (length(S)) rowSums(C[, S, drop = FALSE]) else numeric(p)
          S <- c(S, O[which.max(score(sum(tt[S]) + tt[O], sum(C[S, S]) + 2 * a[O] + dC[O]))])
        }
        climb(climb(S, forbid))                # rebuild with them forbidden, then all compete again
      }

      best_s <- score_set(best)
      for (pass in seq_len(MAX_PASSES)) {
        improved <- FALSE
        for (ld in bearings) for (m in seq_len(MAX_STRIP)) {
          pos <- best[order(-ld[best])]; pos <- pos[ld[pos] > 0]
          neg <- best[order(ld[best])]; neg <- neg[ld[neg] < 0]
          strip <- unique(c(head(pos, m), head(neg, m)))
          if (length(strip) == 0 || length(strip) >= k || p - length(strip) < k) next
          cand <- rebuild(setdiff(best, strip), strip); cs <- score_set(cand)
          if (cs > best_s * (1 + 1e-12)) { best <- cand; best_s <- cs; improved <- TRUE }
        }
        if (!improved) break
      }
    }

    unique(vapply(c(list(best), polished), function(S) paste(idx[sort(S)], collapse = ","), character(1)))
  }

  # Bounds intermediate scoring cost during Synergistic RFE, not final pool
  # size (tuned empirically against recovery-rate tests, not a formula).
  RFE_BUDGET <- rfe.budget

  # Largest pool size at or under `floor_size` whose choose(pool_size, k)
  # doesn't exceed `budget`. Pure size arithmetic, no data or scoring
  # involved -- shared by the real narrowing loop (which narrows the pool
  # this way every round) and predict_narrowing_final_pool_size below (which
  # replays the same decisions to predict, cheaply and exactly, where
  # narrowing will end up -- for the runtime estimate only).
  largest_pool_under_budget <- function(pool_size, k, budget, floor_size) {
    while (pool_size > floor_size && choose(pool_size, k) > budget) pool_size <- pool_size - 1
    pool_size
  }

  # Replays perform_synergy_ranked_elimination's round-by-round pool-size
  # decisions without any real scoring, to predict the final pool size (and
  # hence the final search's combination count) ahead of time. This matters
  # because that final count is often NOT the full `ceiling` budget: binomial
  # coefficients grow so steeply with n.items that a single pool-size
  # decrement can drop combos far below ceiling, especially for larger
  # n.items -- so assuming the full ceiling gets used can itself be wildly
  # wrong, independently of how accurate the throughput calibration is.
  predict_narrowing_final_pool_size <- function(start_pool_size, n.items, ceiling) {
    pool_size <- start_pool_size
    final_floor <- largest_pool_under_budget(start_pool_size, n.items, ceiling, n.items)
    k <- min(2, n.items)
    max_rounds <- n.items + 5
    round_i <- 0
    while (choose(pool_size, n.items) > ceiling && round_i < max_rounds) {
      round_i <- round_i + 1
      next_k <- min(k + 1, n.items)
      budget_for_next <- if (next_k >= n.items) ceiling else RFE_BUDGET
      pool_size <- max(largest_pool_under_budget(pool_size, next_k, budget_for_next, n.items), final_floor)
      k <- next_k
    }
    pool_size
  }

  # perform_synergy_ranked_elimination recomputes each round's item ranking
  # from scratch (item_best isn't accumulated across rounds), so a round
  # whose scoring doesn't itself trigger narrowing for the round after it is
  # pure waste: its ranking gets fully superseded the moment the next round
  # runs its own fresh ranking, with zero effect on which items survive.
  # Given the pool's current size, this jumps straight to the smallest k
  # whose scoring WILL matter (i.e. actually cause the following round to
  # need narrowing), skipping the dead rounds in between -- same final pool,
  # same final k, just without wastefully scoring the no-op rounds first.
  skip_to_meaningful_k <- function(pool_size, k, n.items, ceiling) {
    while (k < n.items) {
      next_k <- min(k + 1, n.items)
      budget <- if (next_k >= n.items) ceiling else RFE_BUDGET
      if (choose(pool_size, next_k) > budget) break
      k <- next_k
    }
    k
  }

  # Synergy-Ranked Recursive Feature Elimination (Synergistic RFE): exhaustively
  # scores every combination at a small k, ranks items by their best achieved
  # score, and drops the weakest before growing k -- until the pool is small
  # enough for a final exhaustive search
  perform_synergy_ranked_elimination <- function(current_cols, data, n.items, ceiling, targ, na.rm, opt.n, speed, show.progress) {

    n_total <- nrow(data)
    if (n_total > opt.n) {
      set.seed(1)
      sub_idx <- sample(seq_len(n_total), opt.n)
      data <- data[sub_idx, , drop = FALSE]
      targ <- targ[sub_idx]
    }

    used_gram <- identical(speed, "fast")

    if (used_gram) {
      gc_components <- build_gram_components(data, targ)
      gram <- gc_components$gram
      col_sums <- gc_components$col_sums
      col_target_dots <- gc_components$col_target_dots
      sum_target <- gc_components$sum_target
      sum_target_sq <- gc_components$sum_target_sq
      n_valid <- gc_components$n_valid
    } else {
      compressed_data <- compress_for_cpp(data)
    }

    RANK_KEEP_TOP <- 10000   # top combos used to rank items each round (row-scan engine only)

    # \r only returns to the start of the CURRENT visual line, not the whole
    # logical line -- if the printed text is wider than the console, it wraps
    # onto a second visual line that \r can never reach, leaving stray
    # remnants behind on later, shorter redraws. Bounding every redraw to a
    # fixed width no wider than the console guarantees it always fits one
    # visual line, so \r can fully clear/overwrite it.
    prog_width <- max(getOption("width", 80L) - 1L, 20L)

    # Complement mode: when keeping more than half the pool (e.g. 50 of 60),
    # choosing which items to keep is choosing which few to DROP -- and small-k
    # rankings say little about which items belong in a 50-item set. So rank
    # items by how good the scale is with them removed instead: score
    # "pool minus k items" exactly as narrowing scores "k items", and narrow
    # the candidates for dropping. Items that leave the candidates are kept
    # outright, and the final search only chooses among the rest
    n_drop <- length(current_cols) - n.items
    if (used_gram && n_drop < n.items) {
      cand <- current_cols
      final_floor <- largest_pool_under_budget(length(cand), n_drop, ceiling, n_drop)
      k <- min(2, n_drop)
      round_i <- 0
      while (choose(length(cand), n_drop) > ceiling && round_i < n_drop + 5) {
        round_i <- round_i + 1
        n_cand <- length(cand)
        k <- skip_to_meaningful_k(n_cand, k, n_drop, ceiling)

        if (show.progress) {
          msg <- sprintf("~{ SynergisticRFE }~ scoring removals at k = %d from pool = %d", k, n_cand)
          if (nchar(msg) > prog_width) msg <- substr(msg, 1, prog_width)
          cat("\r", formatC(msg, width = -prog_width), sep = "")
          flush.console()
        }

        folded <- fold_base_into_moments(gc_components, cand, current_cols, k, sign = -1)
        cpp_result <- process_all_combinations_cpp_gram(
          gram = folded$gram,
          col_sums = folded$col_sums,
          col_target_dots = folded$col_target_dots,
          sum_target = sum_target,
          sum_target_sq = sum_target_sq,
          n_valid = n_valid,
          n_items = k,
          num_choose_from = n_cand,
          original_indices = cand,
          keep_top = 1,
          show_progress = FALSE
        )

        # item_best is now the best |r| of the scale left after removing that
        # item (with k - 1 others): high = a good candidate for dropping
        ranked <- cand[order(cpp_result$item_best, decreasing = TRUE)]
        next_k <- min(k + 1, n_drop)
        budget_for_next <- if (next_k >= n_drop) ceiling else RFE_BUDGET
        cand <- ranked[1:max(largest_pool_under_budget(n_cand, next_k, budget_for_next, n_drop), final_floor)]
        k <- next_k
      }

      fixed <- setdiff(current_cols, cand)
      if (show.progress) {
        final_msg <- sprintf("~{ SynergisticRFE }~ converged: keeping %d items outright, optimised search space to %d items", length(fixed), length(cand))
        cat("\r", formatC(final_msg, width = -prog_width), "\n", sep = "")
      }
      return(list(pool = current_cols, fixed = fixed))
    }

    pool <- current_cols
    # Never narrow below the largest pool the final search can afford: when
    # n.items is large (e.g. 50 of 60), the look-ahead budget for the next
    # small k would otherwise shrink the pool straight to n.items, so the
    # final set was decided by small-k rankings with no final search at all
    final_floor <- largest_pool_under_budget(length(pool), n.items, ceiling, n.items)
    k <- min(2, n.items)
    max_rounds <- n.items + 5  # safety cap; should converge in at most n.items-1 rounds
    round_i <- 0

    while (choose(length(pool), n.items) > ceiling && round_i < max_rounds) {
      round_i <- round_i + 1
      n_pool <- length(pool)
      k <- skip_to_meaningful_k(n_pool, k, n.items, ceiling)

      if (show.progress) {
        msg <- sprintf("~{ SynergisticRFE }~ scoring at k = %d from pool = %d", k, n_pool)
        if (nchar(msg) > prog_width) msg <- substr(msg, 1, prog_width)
        cat("\r", formatC(msg, width = -prog_width), sep = "")
        flush.console()
      }

      # Score every combination at this round's k exhaustively. The pool
      # entering this round was already narrowed (on the previous round) to
      # keep THIS score cheap -- see the look-ahead narrowing below.
      if (used_gram) {
        cpp_result <- process_all_combinations_cpp_gram(
          gram = gram[pool, pool, drop = FALSE],
          col_sums = col_sums[pool],
          col_target_dots = col_target_dots[pool],
          sum_target = sum_target,
          sum_target_sq = sum_target_sq,
          n_valid = n_valid,
          n_items = k,
          num_choose_from = n_pool,
          original_indices = pool,
          keep_top = 1,   # items are ranked from item_best, not from a top-K list
          show_progress = FALSE
        )
      } else {
        cpp_result <- process_all_combinations_cpp_parallel_float(
          data = compressed_data[, pool, drop = FALSE],
          n_items = k,
          num_choose_from = n_pool,
          na_rm = na.rm,
          target = targ,
          original_indices = pool,
          keep_top = RANK_KEEP_TOP,
          show_progress = FALSE
        )
      }

      # Rank items by the best |r| among combinations containing them, not
      # raw frequency (a common-but-mediocre item shouldn't outrank a
      # rare-but-excellent one). The Gram engine tracks this exactly over
      # EVERY combination scored; the row-scan engine only returns a top-K
      # list, so its best is reconstructed from those (items outside it rank last)
      if (used_gram) {
        item_best <- cpp_result$item_best
      } else {
        combo_indices_list <- lapply(strsplit(cpp_result$combination, ','), as.integer)
        flat_items <- unlist(combo_indices_list)
        flat_r <- rep(abs(cpp_result$r), lengths(combo_indices_list))
        valid <- !is.na(flat_r)
        item_best_flat <- tapply(flat_r[valid], flat_items[valid], max)

        item_best <- setNames(rep(-Inf, n_pool), as.character(pool))
        item_best[names(item_best_flat)] <- item_best_flat
        item_best <- item_best[as.character(pool)]
      }
      ranked <- pool[order(item_best, decreasing = TRUE)]

      # Look-ahead narrowing: shrink the pool now so that NEXT round's score
      # (at next_k) stays within budget -- narrowing based on this round's
      # own k would be too late, since this round's score already happened
      # at the current (un-narrowed) pool size. When the next round is the
      # real target k, narrow to `ceiling` directly (not RFE_BUDGET) --
      # RFE_BUDGET only bounds the cost of intermediate scoring calls, it
      # has no business capping the final pool below what the caller's own
      # ceiling would otherwise allow.
      next_k <- min(k + 1, n.items)
      budget_for_next <- if (next_k >= n.items) ceiling else RFE_BUDGET
      target_pool_size <- max(largest_pool_under_budget(n_pool, next_k, budget_for_next, n.items), final_floor)

      pool <- ranked[1:target_pool_size]
      k <- next_k
    }

    # The last printed round shows the pool ENTERING that round, not the
    # narrowing it triggers -- if that round's narrowing already satisfies
    # ceiling, the loop exits with no further round ever printed, so the
    # actual (narrowed) result never appears anywhere above. State it
    # explicitly so "pool = 60" on the last visible round doesn't read as
    # "nothing happened" when it actually converged in that same step.
    if (show.progress) {
      final_msg <- sprintf("~{ SynergisticRFE }~ converged: optimised search space to %d items", length(pool))
      cat("\r", formatC(final_msg, width = -prog_width), "\n", sep = "")
    }

    return(list(pool = pool, fixed = integer(0)))
  }

  # One shift/scale shared by ALL columns, not one per column: rescaling
  # each column to its own 0-254 range re-weighted items by 1/range, so the
  # search optimised a different composite than the raw sum score it reports
  # (wrong best set in 35/40 tests on mixed-range continuous items). A common
  # affine map leaves every sum-score correlation -- including na.rm = TRUE
  # pro-rating -- unchanged
  compress_for_cpp <- function(data) {
    x <- as.matrix(data)

    # Handle NAs
    na_mask <- is.na(x)

    # Find range
    min_val <- min(x, na.rm = TRUE)
    max_val <- max(x, na.rm = TRUE)
    range_val <- max_val - min_val

    # Determine if the data is entirely integers (allow for floating-point noise)
    is_integer_like <- all(abs(x[!na_mask] - round(x[!na_mask])) < 1e-8)

    if (range_val == 0) {
      # Constant data
      compressed <- x * 0
    } else if (range_val <= 254 && is_integer_like) {
      # Already fits AND is integers - just shift to 0
      compressed <- round(x - min_val)
    } else {
      # Continuous decimals OR range > 254 - Scale proportionally to 0-254 buckets
      compressed <- round((x - min_val) * 254 / range_val)
    }
    storage.mode(compressed) <- "integer"

    # Mark NAs as 255
    compressed[na_mask] <- 255L

    return(compressed)
  }
  
  # Score every combination and build the leaderboard
  process_combinations_in_batches <- function(data, targ, num_choose_from,
                                              original_indices, na.rm,
                                              is_binary, n.items, ranking_metric,
                                              optimize_for, show.progress, keep_top,
                                              speed, fixed = integer(0), polish_pool = NULL) {

    used_fast_path <- identical(speed, "fast")

    if (used_fast_path) {
      # Score via the Gram-matrix shortcut (missing values mean-imputed for search only)
      gc_components <- build_gram_components(data, targ)
      mark_time("gram_matrix_build")

      # Items kept outright by complement-mode narrowing are folded into the
      # moments, so only the remaining (free) items are searched. And when
      # keeping more than half of the free items, enumerate the few to DROP
      # instead: same combinations and exact same scores, but the engine's
      # cost per combination grows steeply with k (28 of 40 ran ~20x slower
      # than the equally-sized 12 of 40)
      free <- setdiff(seq_len(num_choose_from), fixed)
      k_free <- n.items - length(fixed)
      n_drop <- length(free) - k_free
      drop_space <- n_drop >= 1 && n_drop < k_free
      search <- if (drop_space) {
        fold_base_into_moments(gc_components, free, seq_len(num_choose_from), n_drop, sign = -1)
      } else if (length(fixed) > 0) {
        fold_base_into_moments(gc_components, free, fixed, k_free, sign = 1)
      } else gc_components

      cpp_result <- process_all_combinations_cpp_gram(
        gram = search$gram,
        col_sums = search$col_sums,
        col_target_dots = search$col_target_dots,
        sum_target = gc_components$sum_target,
        sum_target_sq = gc_components$sum_target_sq,
        n_valid = gc_components$n_valid,
        n_items = if (drop_space) n_drop else k_free,
        num_choose_from = length(free),
        original_indices = original_indices[free],
        keep_top = keep_top,
        show_progress = show.progress
      )
      # Report every combination as its full item set
      if (drop_space) {
        cpp_result$combination <- vapply(strsplit(cpp_result$combination, ","), function(d)
          paste(setdiff(original_indices, as.numeric(d)), collapse = ","), character(1))
      } else if (length(fixed) > 0) {
        cpp_result$combination <- paste(paste(original_indices[fixed], collapse = ","), cpp_result$combination, sep = ",")
      }
      mark_time("cpp_scoring_call")
    } else {
      compressed_data <- compress_for_cpp(data)
      mark_time("data_compression")

      cpp_result <- process_all_combinations_cpp_parallel_float(
        data = compressed_data,
        n_items = n.items,
        num_choose_from = num_choose_from,
        na_rm = na.rm,
        target = targ,
        original_indices = original_indices,
        keep_top = keep_top,
        show_progress = show.progress
      )
      mark_time("cpp_scoring_call")
    }

    # Convert to data.frame
    leaderboard <- data.frame(
      combination = cpp_result$combination,
      r = cpp_result$r,
      stringsAsFactors = FALSE
    )

    # When optimisation left items out of this search, polish the top sets
    # over the whole pool. New sets are only ADDED (and re-scored exactly
    # below), so the reported best can never get worse; any item they bring
    # back is added to the scored columns and reported to the caller
    added <- numeric(0)
    polished <- character(0)
    if (!is.null(polish_pool)) {
      POLISH_STARTS <- 5
      polished <- polish_by_swaps(polish_pool$data, targ, polish_pool$original_indices,
                                  head(leaderboard$combination, POLISH_STARTS))
      known <- vapply(strsplit(leaderboard$combination, ","), function(z) paste(sort(as.numeric(z)), collapse = ","), character(1))
      polished <- setdiff(unique(polished), known)
      if (length(polished) > 0) {
        leaderboard <- rbind(data.frame(combination = polished, r = NA_real_, stringsAsFactors = FALSE), leaderboard)
        added <- setdiff(unique(as.numeric(unlist(strsplit(polished, ",")))), original_indices)
        if (length(added) > 0) {
          data <- cbind(data, polish_pool$data[, match(added, polish_pool$original_indices), drop = FALSE])
          original_indices <- c(original_indices, added)
        }
      }
      mark_time("swap_polish")
    }

    # Recompute exact statistics for the top `keep_top` combinations from the
    # true data (both scoring engines above work from approximated data), in
    # parallel C++: each combination's sum scores are built straight from the
    # data, then its pairwise-complete r -- and for binary targets its cutoff
    # search and AUC. This used to build N x keep_top score matrices (~7 GB
    # at N = 300,000 with a binary target) and loop the cutoff search in R on
    # one core (21 of 27 seconds of that run)
    {
      top_combos <- strsplit(leaderboard$combination, ',')
      sets <- matrix(unlist(lapply(top_combos, function(cids) match(as.numeric(cids), original_indices))) - 1L,
                     nrow = n.items)
      storage.mode(sets) <- "integer"

      if (is_binary && show.progress) {
        bc_width <- max(getOption("width", 80L) - 1L, 20L)
        cat("\r", formatC("~{  binaryCutoff  }~ searching", width = -bc_width), sep = "")
        flush.console()
      }
      rescored <- rescore_sets_cpp(as.matrix(data), as.numeric(targ), sets, na.rm, is_binary,
                                   identical(optimize_for, "binarised_r"))
      mark_time("exact_rescoring")

      leaderboard$r <- rescored$r

      if (is_binary) {
        leaderboard$`>=` <- rescored$cutoff
        leaderboard$binarised_r <- rescored$binarised_r
        leaderboard$youden_j <- rescored$youden_j
        leaderboard$auc <- rescored$auc

        # Re-rank by the chosen metric
        leaderboard <- leaderboard[order(abs(leaderboard[[ranking_metric]]),
                                         decreasing = TRUE), ]
        mark_time("binary_reranking")
      } else {
        # Re-rank by the exact r just recomputed. Both engines search on an
        # approximation (mean-imputed / 8-bit data), so their order within
        # the top keep_top can disagree with the r actually reported: under
        # 15% missingness, row 1 was not the best reported set in half of
        # the runs tested. Re-ranking recovered the exact optimum in every
        # stress test run (80/80, up to 50% missing, incl. MNAR patterns)
        leaderboard <- leaderboard[order(abs(leaderboard$r), decreasing = TRUE), ]
      }
    }

    # What the polish changed: the best |r| among the search's own sets vs.
    # among all sets, and which items the best set swapped
    polish_info <- NULL
    if (!is.null(polish_pool)) {
      r_abs <- abs(leaderboard$r)
      from_polish <- leaderboard$combination %in% polished
      i_search <- which(!from_polish)[which.max(r_abs[!from_polish])]
      i_best <- which.max(r_abs)
      set_of <- function(i) as.numeric(strsplit(leaderboard$combination[i], ",")[[1]])
      polish_info <- list(r_search = r_abs[i_search], r_polished = r_abs[i_best],
                          swapped_in = setdiff(set_of(i_best), set_of(i_search)),
                          swapped_out = setdiff(set_of(i_search), set_of(i_best)))
    }

    cpp_results <- list(leaderboard = leaderboard,
                        timings_cpp = cpp_result$timings_cpp,
                        used_fast_path = used_fast_path,
                        added = added,
                        polish_info = polish_info)

    return(cpp_results)
  }
  
  # Parse leaderboard$combination into sorted index lists, shared below so
  # the same strings aren't parsed twice
  parse_leaderboard_combinations <- function(leaderboard, original_indices) {
    comb_list_unordered <- lapply(strsplit(leaderboard$combination, ','), as.numeric)
    comb_list <- lapply(comb_list_unordered, sort)
    matched_comb_list <- lapply(comb_list, function(x) match(x, original_indices))
    return(list(comb_list = comb_list, matched_comb_list = matched_comb_list))
  }

  # Item names/indices for each leaderboard row
  extract_best_items <- function(data, parsed_combos) {
    best_names <- lapply(parsed_combos$matched_comb_list, function(x) colnames(data)[x])
    return(list(best_names = best_names, best_indices = parsed_combos$comb_list))
  }

  # Correlation of each individual item with the target, for every leaderboard row
  calculate_item_correlations <- function(data, targ, items_to_flip, cols_names, parsed_combos) {

    if (nrow(data) > 10000) { set.seed(1); it.cor.sub <- sample(1:nrow(data), 10000) } else it.cor.sub <- 1:nrow(data)

    matched_comb_list <- parsed_combos$matched_comb_list

    unflip_indices <- intersect(cols_names, items_to_flip)

    flip_items_simple <- function(x, indices){
      x[,indices] <- x[,indices]*(-1)
      return(x)
    }

    dc2 <- as.vector(cor(flip_items_simple(data[it.cor.sub,], unflip_indices), targ[it.cor.sub], 'p') )

    ind_cors <- unlist(
      lapply(
        lapply(matched_comb_list, function(x) round(dc2, 2)[x]),
        paste, collapse = ',')
    )

    return(ind_cors)
  }


  # Sum scores for a specific item set
  generate_item_scores <- function(data, best_names, na.rm, is_binary, cutoff = NULL) {
    scores_val <- rowMeans(data[, best_names, drop = FALSE], na.rm = na.rm) * length(best_names)

    # Always a data frame so column subsetting [ , 1] works
    if (is_binary) {
      return(data.frame(sum_score = scores_val,
                        binary_score = scores_val >= cutoff))
    } else {
      return(data.frame(sum_score = scores_val))
    }
  }
  
  # Score every leaderboard item set on the holdout data
  # Holdout statistics for every leaderboard row, scored in parallel C++ like
  # the training set's exact re-scoring -- but at each row's TRAINING cutoff
  # (re-searching it on the holdout would be both wrong and wasted work)
  cross_validate_leaderboard <- function(leaderboard, data, target,
                                         na.rm, is_binary, best_names) {

    if (nrow(data) == 0) stop("Holdout data is empty. Cannot cross-validate.")

    sets <- matrix(unlist(lapply(best_names, function(nm) match(nm, colnames(data)))) - 1L,
                   ncol = nrow(leaderboard))
    storage.mode(sets) <- "integer"
    held <- rescore_sets_cpp(as.matrix(data), as.numeric(target), sets, na.rm, is_binary, FALSE,
                             fixed_cutoffs = if (is_binary) as.numeric(leaderboard$`>=`) else numeric(0))

    leaderboard$r_holdout <- held$r
    if (is_binary) {
      leaderboard$binarised_r_holdout <- held$binarised_r
      leaderboard$youden_j_holdout <- held$youden_j
      leaderboard$auc_holdout <- held$auc
    }

    return(leaderboard)
  }
  
  format_duration <- function(seconds) {
    
    units <- c(
      year   = 31557600,
      month  = 2629743,
      day    = 86400,
      hour   = 3600,
      minute = 60,
      second = 1
    )
    
    values <- c()
    remainder <- seconds
    
    # Calculate amounts for each unit
    for (i in seq_along(units)) {
      if (remainder >= units[i]) {
        count <- floor(remainder / units[i])
        remainder <- remainder %% units[i]
        
        # Handle plurals
        unit_name <- names(units)[i]
        if (count > 1) unit_name <- paste0(unit_name, "s")
        
        values <- c(values, paste(format(count, big.mark = ","), unit_name))
      }
    }
    
    # Return logic
    if (length(values) == 0) return("0 seconds")
    if (length(values) == 1) return(values[1])

    return(values[1])
  }

  # "~X unit to Y unit" reads awkwardly when both bounds land on the same
  # unit (e.g. "2 hours to 11 hours") -- collapse to "~X to Y unit" in that
  # case; keep both full strings when the units differ (e.g. "45 seconds to
  # 2 minutes")
  format_duration_range <- function(low_seconds, high_seconds) {
    # format_duration() floors to whole units, so any range under a second
    # would otherwise print as the meaningless "0 to 0 seconds"
    if (high_seconds < 1) return("< 1 second")

    low <- format_duration(low_seconds)
    high <- format_duration(high_seconds)

    low_unit <- sub("^[0-9,]+ ", "", low)
    high_unit <- sub("^[0-9,]+ ", "", high)

    if (identical(sub("s$", "", low_unit), sub("s$", "", high_unit))) {
      low_count <- sub(" .*$", "", low)
      return(paste0("~",low_count, " to ", high))
    }

    paste0("~",low, " to ", high)
  }


  # ============================================================================
  # MAIN FUNCTION BODY
  # ============================================================================
  
  checkpoints <- list(start = Sys.time())
  mark_time <- function(label) {
    checkpoints[[label]] <<- Sys.time()
  }

  # Convert tibbles to standard data frames
  if (inherits(data, "tbl_df") || inherits(data, "tbl")) {
    data <- as.data.frame(data)
  }
  
  target_expr <- substitute(target)
  target_col_name <- NULL

  # Handle non-standard evaluation for target parameter
  if (!is.null(target_expr) && is.symbol(target_expr)) {
    target_name <- as.character(target_expr)

    # Check if this symbol refers to a column in data
    if (target_name %in% colnames(data)) {
      target_col_name <- target_name
      # [, name] rather than [[name]], which errors on matrix input
      target <- data[, target_name]
      # Replace column with NAs so it's filtered out but indices remain correct
      data[, target_name] <- NA
    } else {
      # Try to evaluate in parent environment
      target <- tryCatch(
        eval(target_expr, parent.frame()),
        error = function(e) {
          stop(paste0("Could not find column '", target_name, 
                      "' in data or variable '", target_name, "' in environment"))
        }
      )
    }
  }
  
  # Validate target length if provided
  if (!is.null(target) && length(target) != nrow(data)) {
    stop("Length of 'target' must match number of rows in 'data'")
  }

  # Validate target type and variance up front (factors/characters used to
  # fail deep inside cor() with "'y' must be numeric", and a constant target
  # silently returned r = NA)
  if (!is.null(target)) {
    if (is.logical(target)) target <- as.numeric(target)
    if (!is.numeric(target)) {
      stop("'target' must be numeric (or logical / 0-1 for binary classification).")
    }
    if (length(unique(target[!is.na(target)])) < 2) {
      stop("'target' has no variance (fewer than two distinct non-missing values).")
    }
  }


  # Detect binary target and notify user
  is_binary <- FALSE
  target_supplied <- !is.null(target)


  if (!is.null(target)) {
    unique_vals <- unique(target[!is.na(target)])
    is_binary <- length(unique_vals) == 2 && all(unique_vals %in% c(0, 1))

    if (length(unique_vals) == 2 && !is_binary && verbose) {
      message(sprintf("=~= Note: target has exactly two values (%s) -- recode to 0/1 to optimise for binary classification.",
                      paste(sort(unique_vals), collapse = ", ")))
    }

    # If scale.vars is TRUE with binary target, treat as continuous
    if (is_binary && scale.vars) {
      is_binary <- FALSE
      if (verbose) message("scale.vars = TRUE: treating binary target as continuous variable")
    }

    if (is_binary) {
      if (verbose) message("=~= Binary target detected (values: 0, 1): optimising for classification performance.")
    }
  }

  # Determine ranking metric and optimization method
  if (is_binary) {
    # Validate method for binary targets
    valid_binary_methods <- c("r", "binarised_r", "youden_j", "auc")

    if (is.null(method)) {
      ranking_metric <- "youden_j"
      if (verbose) message("=~= Ranking combinations by Youden's J (use method = 'binarised_r', 'r', or 'auc' to change)")
    } else {
      # Case-insensitive, unambiguous-prefix matching -- e.g. "Y", "y",
      # "youden", all resolve to "youden_j"; "r" matches only itself since
      # no other option starts with r
      matched_idx <- pmatch(tolower(method), valid_binary_methods)
      if (is.na(matched_idx)) {
        stop(paste0("For binary targets, method must be one of: ",
                    paste(valid_binary_methods, collapse = ", ")))
      }
      ranking_metric <- valid_binary_methods[matched_idx]
    }

    optimize_for <- ranking_metric

  } else {
    # Non-binary target
    ranking_metric <- "r"
    optimize_for <- NULL

    if (!is.null(method)) {
      if (verbose) message("Note: 'method' parameter ignored for non-binary targets")
    }
  }

  # Classification metrics need a wider leaderboard: earlier stages rank by r,
  # which can discard combinations with weak r but strong classification
  # performance (the top 100 by r missed the best-Youden set in 5/12 tests;
  # the top 1000 in 0/12). Binarised r is cutoff-based too, so it gets the
  # same width as Youden's J and AUC
  leaderboard_length <- if (ranking_metric %in% c("youden_j", "auc", "binarised_r")) 1000 else 100

  mark_time("target_resolution")
  
  # Ensure column names exist and save them
  if (is.null(colnames(data))) {
    colnames(data) <- paste0("Col_", 1:ncol(data))
  }
  # Name-based lookups (best_names, scoring) need unique names
  if (anyDuplicated(colnames(data))) {
    colnames(data) <- make.unique(colnames(data))
    if (verbose) message("=~ Note: duplicate column names were made unique (e.g. 'x', 'x' -> 'x', 'x.1').")
  }
  original_names <- colnames(data)  # Save original column names
  all_original_indices <- 1:ncol(data)

  # Logical (TRUE/FALSE) items are valid 0/1 items, not non-numeric ones to drop
  if (is.matrix(data) && is.logical(data)) {
    storage.mode(data) <- "double"
  } else if (is.data.frame(data)) {
    is_lgl <- vapply(data, is.logical, logical(1))
    data[is_lgl] <- lapply(data[is_lgl], as.numeric)
  }

  # if cross-validating, create row indices for training and holdout samples.
  # Done first, so every data-driven decision below -- valid columns, item
  # orientation, prefilter relevance, scaling -- uses training rows only and
  # never sees the holdout
  train_rows <- seq_len(nrow(data))
  if (cross.validate) {
    if (cross.validate < 0 || cross.validate > 1){
      stop("Error: cross.validate must be TRUE, FALSE, or a numeric value between 0 and 1.")
    }
    if (cross.validate == T) cross.validate <- 0.75
    cv_subset_size <- round(nrow(data) * cross.validate,0)
    set.seed(1)
    cv_subset <- sample(1:nrow(data), cv_subset_size)
    cv_holdout <- setdiff(1:nrow(data), cv_subset)
    train_rows <- cv_subset
    }

  mark_time("cv_split_setup")

  # Identify valid numeric columns (and, when cross-validating, columns that
  # also vary within the training rows -- a column constant there would
  # enter every search stage as a zero-variance "free" item)
  if (is.matrix(data) || is.array(data)) {
    valid_mask <- apply(data, 2, is_valid_column)
    if (cross.validate) valid_mask <- valid_mask & apply(data[train_rows, , drop = FALSE], 2, is_valid_column)
  } else {
    valid_mask <- sapply(data, is_valid_column)
    if (cross.validate) valid_mask <- valid_mask & sapply(data[train_rows, , drop = FALSE], is_valid_column)
  }
  valid_mask[is.na(valid_mask)] <- FALSE

  excluded <- setdiff(original_names[!valid_mask], target_col_name)
  if (length(excluded) > 0 && verbose) {
    message(paste0("=~ Note: excluding ", length(excluded), " column(s) that are non-numeric, constant, all-NA or contain infinite values: ",
                   paste(head(excluded, 10), collapse = ", "), if (length(excluded) > 10) ", ..." else ""))
  }

  if (sum(valid_mask) < n.items) {
    stop("Error: Input data contains fewer valid numeric columns than 'n.items'.")
  }

  mark_time("column_validation")

  # Filter to valid columns
  data <- data[, valid_mask, drop = FALSE]
  cols <- 1:ncol(data)
  original_indices <- all_original_indices[valid_mask]
  filtered_names <- colnames(data)  # Save valid column names

  mark_time("column_filtering")

  # Convert to matrix for computational efficiency
  if (!is.matrix(data)) data <- as.matrix(data)

  # flip items negatively correlated with most central item

  if (!target_supplied) {
    dc <- imputed_cor(data[train_rows, , drop = FALSE])
    centrality <- colSums(abs(dc), na.rm = TRUE)
    most_central_item <- order(centrality, decreasing = TRUE)[1]

    pivot_item <- colnames(data)[most_central_item]

    items_to_flip_TF <- dc[, most_central_item] < 0
    items_to_flip <- filtered_names[items_to_flip_TF]

    # Reverse-score items
    data <- flip_items(data, items_to_flip, train_rows)

    # Used later to prefilter weak items before optimisation
    relevance <- centrality
  }

  if (target_supplied) {
    # For external criteria: flip items negatively correlated with target
    dc <- as.vector(cor(data[train_rows, , drop = FALSE], target[train_rows], use = "p"))

    # Drop (rather than zero out) items near-perfectly correlated with the
    # target -- almost certainly the target itself or a copy of it -- or
    # with no computable correlation. Zeroed columns used to stay in the
    # pool as free "dummy" slots: a reported k-item set could really be k-1
    # items plus a constant, reported with the smaller set's r but scored
    # with the real item by anyone who then used it
    drop_mask <- is.na(dc) | abs(dc) > 0.999999
    if (any(drop_mask)) {
      if (verbose) {
        message(paste0("=~ Note: excluding ", sum(drop_mask), " item(s) with an undefined or near-perfect (|r| > 0.999999) correlation with the target: ",
                       paste(head(filtered_names[drop_mask], 10), collapse = ", ")))
      }
      if (sum(!drop_mask) < n.items) {
        stop("Error: Input data contains fewer valid numeric columns than 'n.items'.")
      }
      data <- data[, !drop_mask, drop = FALSE]
      dc <- dc[!drop_mask]
      cols <- 1:ncol(data)
      original_indices <- original_indices[!drop_mask]
      filtered_names <- filtered_names[!drop_mask]
    }

    items_to_flip_TF <- dc < 0
    items_to_flip <- filtered_names[items_to_flip_TF]

    data <- flip_items(data, items_to_flip, train_rows)
    pivot_item <- NA

    # Used later to prefilter weak items before optimisation
    relevance <- abs(dc)
  }

  # Apply standardization if requested -- here, before any search stage (not
  # just the final one), using training-row means/SDs for every row. Scaling
  # only the final pool left Synergistic RFE ranking items on the raw scale,
  # and holdout rows (and their $scores) on a different scale from training
  if (scale.vars) {
    scale_ref <- data[train_rows, , drop = FALSE]
    data <- sweep(data, 2, colMeans(scale_ref, na.rm = TRUE), "-")
    data <- sweep(data, 2, apply(scale_ref, 2, sd, na.rm = TRUE), "/")
  }

  #create target
  if (!target_supplied) target <- rowMeans(data, na.rm = na.rm)
  target_all <- target  # full-length (training + holdout), returned in $target

  if (cross.validate) {
    data_holdout <- data[cv_holdout, , drop = FALSE]
    target_holdout <- target[cv_holdout]
    data <- data[cv_subset, , drop = FALSE]
    target <- target[cv_subset]
  }

  mark_time("item_flipping")
  
  # Shared by both runtime estimates printed when verbose: when optimisation is
  # triggered, and when optimise = FALSE forces a search past ceiling
  measure_combos_per_sec <- function(k = n.items) {

    # Measure real combinations/sec on this machine to estimate runtime.
    # A single small timed call is heavily biased toward one-time fixed
    # costs (RcppParallel thread-pool startup, R<->C++ call overhead) that
    # never recur once real work is running, making the estimate look far
    # slower than reality. Instead: run a small first-look calibration
    # (after an untimed warm-up, so thread-pool startup isn't counted at
    # all), then use ITS OWN measured rate to size a second calibration
    # aimed at ~TARGET_CALIB_TIME of real work, which is enough to
    # amortise the fixed costs away. Both phases size themselves via
    # choose(), so this stays cheap and safe regardless of engine speed --
    # never based on a fixed pool-size margin that could blow up for large
    # n.items under the (much slower) row-scan engine.
    grow_pool_to_combos <- function(target_combos, start_p, max_p) {
      p <- start_p
      while (p < max_p && choose(p, k) < target_combos) p <- p + 1
      p
    }

    TINY_CALIB_COMBOS <- 2000000
    TARGET_CALIB_TIME <- 0.1  # seconds

    if (identical(speed, "fast")) {
      # Gram engine cost doesn't depend on which items, so a small sample suffices
      col_means <- colMeans(data, na.rm = TRUE)
      calib_data <- data
      na_mask <- is.na(calib_data)
      calib_data[na_mask] <- rep(col_means, each = nrow(calib_data))[na_mask]
      valid_rows <- !is.na(target)
      targ_valid <- target[valid_rows]

      run_calib_at <- function(p) {
        cc <- calib_data[valid_rows, 1:p, drop = FALSE]
        t0 <- Sys.time()
        invisible(process_all_combinations_cpp_gram(
          gram = crossprod(cc),
          col_sums = colSums(cc),
          col_target_dots = as.vector(crossprod(cc, targ_valid)),
          sum_target = sum(targ_valid),
          sum_target_sq = sum(targ_valid^2),
          n_valid = length(targ_valid),
          n_items = k,
          num_choose_from = p,
          original_indices = 1:p,
          keep_top = 1,
          show_progress = FALSE
        ))
        list(elapsed = as.numeric(difftime(Sys.time(), t0, units = "secs")), combos = choose(p, k))
      }

    } else {
      run_calib_at <- function(p) {
        calib_compressed <- compress_for_cpp(data[, 1:p, drop = FALSE])
        t0 <- Sys.time()
        invisible(process_all_combinations_cpp_parallel_float(
          data = calib_compressed,
          n_items = k,
          num_choose_from = p,
          na_rm = na.rm,
          target = target,
          original_indices = 1:p,
          keep_top = 1,
          show_progress = FALSE
        ))
        list(elapsed = as.numeric(difftime(Sys.time(), t0, units = "secs")), combos = choose(p, k))
      }
    }

    p0 <- grow_pool_to_combos(TINY_CALIB_COMBOS, k, ncol(data))
    run_calib_at(p0)  # untimed warm-up
    tiny <- run_calib_at(p0)
    tiny_rate <- tiny$combos / max(tiny$elapsed, 1e-6)

    p1 <- grow_pool_to_combos(tiny_rate * TARGET_CALIB_TIME, p0, ncol(data))
    if (p1 > p0) {
      # Repeat the sized probe rather than timing it once: each repeat
      # scores the identical combination set, so this is just extra timing
      # samples of the same work, not new work. A stray context switch,
      # page fault, or GC pause can only ever make a single sample look
      # SLOWER than true throughput, never faster -- so the max across a
      # few repeats discards that noise and reflects genuinely achievable
      # speed, rather than being dragged down by an unlucky sample.
      N_CALIB_REPEATS <- 1
      rates <- vapply(seq_len(N_CALIB_REPEATS), function(i) {
        r <- run_calib_at(p1)
        r$combos / max(r$elapsed, 1e-6)
      }, numeric(1))
      combos_per_sec <- max(rates)
    } else {
      combos_per_sec <- tiny_rate
    }
    combos_per_sec
  }

  # Check if optimization is needed
  num_combinations <- choose(ncol(data), n.items)
  fixed_cols <- integer(0)

  if (num_combinations > ceiling) {

    if (optimise) {

      # Drop items far weaker than the strongest before estimating runtime,
      # not just before optimisation runs -- otherwise the estimate below
      # assumes the final search will use the full `ceiling` budget, when
      # prefiltering alone often already brings the pool comfortably under
      # ceiling before progressive narrowing would even need to run.
      prefilter_message <- NULL
      if (!is.null(prefilter.ratio) && is.finite(prefilter.ratio)) {
        item_relevance <- relevance[cols]
        keep_mask <- item_relevance >= (max(item_relevance, na.rm = TRUE) / prefilter.ratio)
        keep_mask[is.na(keep_mask)] <- TRUE  # never drop items with undefined relevance

        # Only when the search would still choose fewer than half of what's
        # left: otherwise the prefilter decides the answer on single-item
        # relevance alone (24 of 26 survivors missed the best 24-of-36 set by
        # up to r = .04 -- long scales can need individually weak items)
        if (sum(keep_mask) >= 2 * n.items) {
          n_before <- length(cols)
          cols <- cols[keep_mask]
          if (length(cols) < n_before) {
            prefilter_message <- sprintf("=~ Prefilter: keeping %d of %d items (relevance >= 1/%g of the strongest item) before optimisation.",
                                         length(cols), n_before, prefilter.ratio)
          }
        }
      }

      # Everything down to the message below only feeds the runtime estimate
      # printed when verbose -- skip it entirely when silent (the timing probes
      # cost ~20% of a typical run with verbose = FALSE, for nothing)
      if (verbose) {

        # The fast engine searches whichever side is smaller -- the items to
        # keep, or (when keeping more than half) the items to drop -- so time
        # it at that size
        n_drop <- length(cols) - n.items
        complement <- identical(speed, "fast") && n_drop < n.items
        combos_per_sec <- measure_combos_per_sec(if (complement) n_drop else n.items)

        est_seconds <- num_combinations / combos_per_sec
        # Predict the final search's actual combination count by replaying
        # narrowing's own pool-size decisions (see predict_narrowing_final_pool_size
        # above) rather than assuming it uses the full `ceiling` budget --
        # binomial coefficients grow so steeply with n.items that a single
        # pool-size decrement can land well under ceiling, especially for
        # larger n.items, so that assumption can be wrong by orders of
        # magnitude on its own, independent of calibration accuracy.
        # (complement mode narrows the candidates for dropping with the same
        # schedule, so the same replay applies with n_drop in place of n.items)
        if (complement) {
          predicted_final_pool_size <- predict_narrowing_final_pool_size(length(cols), n_drop, ceiling)
          predicted_final_combos <- choose(predicted_final_pool_size, n_drop)
          n_kept_outright <- length(cols) - predicted_final_pool_size
        } else {
          predicted_final_pool_size <- predict_narrowing_final_pool_size(length(cols), n.items, ceiling)
          predicted_final_combos <- choose(predicted_final_pool_size, n.items)
          n_kept_outright <- 0
        }
        opt_est_seconds <- predicted_final_combos / combos_per_sec

        message(paste0("=~ This would generate ",
                       format(num_combinations, big.mark = ",",scientific = FALSE),
                       " combinations to compare (",format_duration_range(est_seconds/2, est_seconds*2),
                       " with N = ",format(nrow(data), big.mark = ",",scientific = FALSE) ,
                       "). \n=~ Synergistic RFE will be used to reduce combinations to below ",
                       format(ceiling, big.mark = ",",scientific = FALSE),
                       " (search space: ",n.items - n_kept_outright," items from ",predicted_final_pool_size,
                       if (n_kept_outright > 0) paste0(", plus ", n_kept_outright, " kept outright"),
                       "; ",format_duration_range(opt_est_seconds/2, opt_est_seconds*2),
                       ").",
                       " You can change this threshold with the 'ceiling' argument."))
        if (!is.null(prefilter_message)) message(prefilter_message)
      }

      narrowed <- perform_synergy_ranked_elimination(cols, data, n.items, ceiling, target, na.rm, opt.n, speed, show.progress)
      cols <- narrowed$pool
      fixed_cols <- narrowed$fixed

    } else {
      if (verbose) {
        est_seconds <- num_combinations / measure_combos_per_sec(if (identical(speed, "fast")) min(n.items, ncol(data) - n.items) else n.items)
        message(sprintf("=~ Note: %s combinations exceeds ceiling (%s) but optimise = FALSE. Running exhaustive search: expected completion time is %s.",
                        format(num_combinations, big.mark = ",", scientific = FALSE),
                        format(ceiling, big.mark = ",", scientific = FALSE),
                        format_duration_range(est_seconds/2, est_seconds*2)))
      }
    }
  }
  
  cols_names <- filtered_names[cols]
  
  mark_time("combination_optimisation")
  
  # Keep the whole pool for the swap polish whenever the final search won't
  # cover every item (narrowed, prefiltered, or items kept outright)
  needs_polish <- length(cols) < ncol(data) || length(fixed_cols) > 0
  searched_combinations <- choose(length(cols) - length(fixed_cols), n.items - length(fixed_cols))
  pool_data <- data
  pool_holdout <- if (cross.validate) data_holdout else NULL

  # Subset to optimized columns
  data <- data[, cols, drop = FALSE]
  if (cross.validate) { data_holdout <- data_holdout[, cols, drop = FALSE] }
  
  mark_time("column_subsetting")

  # (Standardization, if requested, was already applied before optimisation)

  mark_time("scaling")
  
  # Warn if items use very different scales (e.g. mixing 0-1 and 1-7 items)
  if (!scale.vars) {
    # apply(data, 2, ...) on a data.frame silently coerces the whole thing to
    # a matrix first (a full copy) just to iterate columns -- indexing
    # data[, i] directly avoids that copy and works the same for a matrix or
    # data.frame input
    col_ranges <- vapply(seq_len(ncol(data)), function(i) diff(range(data[, i], na.rm = TRUE)), numeric(1))

    if (max(col_ranges) / min(col_ranges) > 2 && verbose) {
      message(sprintf("Note: Wide variation in item scales detected (Ranges: %s to %s).\nConsider setting 'scale.vars = TRUE' to ensure consistent weighting.",
                      round(min(col_ranges), 2), round(max(col_ranges), 2)))
    }
  }
  
  # Update index mapping after filtering
  pool_indices <- original_indices
  original_indices <- original_indices[cols]
  
  if (n.items > ncol(data)) {
    stop("After optimisation, n.items (",n.items,") is larger than remaining item pool (",
         ncol(data)," columns). Increase ceiling (",ceiling," combinations), or reduce n.items.") 
  }
  
  num_choose_from <- ncol(data)
  
  mark_time("scale_range_check")
  
  # Process combinations and build leaderboard (always uses indices internally)
  cpp_results <- process_combinations_in_batches(data, target, num_choose_from,
                                                 original_indices, na.rm,
                                                 is_binary, n.items, ranking_metric,
                                                 optimize_for, show.progress, leaderboard_length,
                                                 speed, fixed = match(fixed_cols, cols),
                                                 polish_pool = if (needs_polish) list(data = pool_data, original_indices = pool_indices))

  # Items the swap polish brought back join the scored columns
  if (length(cpp_results$added) > 0) {
    cols <- c(cols, match(cpp_results$added, pool_indices))
    cols_names <- filtered_names[cols]
    data <- pool_data[, cols, drop = FALSE]
    if (cross.validate) { data_holdout <- pool_holdout[, cols, drop = FALSE] }
    original_indices <- pool_indices[cols]
  }
  rm(pool_data, pool_holdout)

  leaderboard <- cpp_results$leaderboard
  timings_cpp <- cpp_results$timings_cpp                                                                                                                   
  
  # Clean up leaderboard
  rownames(leaderboard) <- NULL

  if (item.set > nrow(leaderboard)) {
    stop("'item.set' (", item.set, ") is larger than the number of ranked item sets (", nrow(leaderboard), ").")
  }

  mark_time("combination_scoring")
  
  # Parse leaderboard combinations once, shared by both steps below
  parsed_combos <- parse_leaderboard_combinations(leaderboard, original_indices)

  # Extract best items
  best_items <- extract_best_items(data, parsed_combos)

  mark_time("extract_best_items")

  # Calculate item-level correlations
  ind_cors <- calculate_item_correlations(data, target, items_to_flip, cols_names, parsed_combos)
  
  # Keys straight from the orientation actually used in scoring, not
  # re-estimated from (rounded) item correlations on a separate subsample,
  # which could disagree for weak items and describe a different scale
  ind_keys <- ifelse(best_items$best_names[[item.set]] %in% items_to_flip, -1, 1)
  
  mark_time("item_correlations")
  
  # Cross-validate if requested
  if (cross.validate) {
    # Ensure holdout data actually exists
    if (!exists("data_holdout") || !exists("target_holdout")) {
      stop("Cross-validation requested but holdout data not found.")
    }
    
    leaderboard <- cross_validate_leaderboard(
      leaderboard = leaderboard,
      data = data_holdout,
      target = target_holdout,
      na.rm = na.rm,
      is_binary = is_binary,
      best_names = best_items$best_names
    )
  }
  
  
  if (r.sq) {
    if (!is_binary) {
      if (!cross.validate) {
        leaderboard$R2 <- leaderboard$r^2
      } else {
        leaderboard$R2_train <- leaderboard$r^2
        leaderboard$R2_holdout <- leaderboard$r_holdout^2
      }
    } else {
      if (!cross.validate) {
        leaderboard$sum_scored_R2 <- leaderboard$r^2
      } else {
        leaderboard$sum_scored_R2_train <- leaderboard$r^2
        leaderboard$binarised_R2_train <- leaderboard$binarised_r^2
      }
    }
  }
  
  
  mark_time("cross_validation_and_rsq")
  
  # Merge training/holdout sets back into original row order
  if (cross.validate) {
    combined_idx <- c(cv_subset, cv_holdout)
    data <- rbind(as.matrix(data), as.matrix(data_holdout))[order(combined_idx), , drop = FALSE]
  }
  
  # Generate scores if requested
  computed_scores <- NULL
  if (generate) {
      computed_scores <- generate_item_scores(
        data = data,
        best_names = best_items$best_names[[item.set]],
        na.rm = na.rm, 
        is_binary = is_binary, 
        cutoff = leaderboard$`>=`[item.set]
      )
  }
  
  # Convert combination column to names if requested
  if (item.names) {
    leaderboard$combination <- best_items$best_names
  }
  
  # Elapsed time between consecutive checkpoints, keyed by the step's descriptive label
  step_secs <- diff(vapply(checkpoints, as.numeric, numeric(1)))
  names(step_secs) <- names(checkpoints)[-1]
  total_secs <- sum(step_secs)

  timings_r <- data.frame(
    step = c(names(step_secs), "total"),
    s = round(c(step_secs, total_secs), 4),
    row.names = NULL
  )
  timings_r$percent <- round(100 * timings_r$s / total_secs, 2)

  names(timings_cpp) <- paste0('time', seq_along(timings_cpp))
  timings_cpp <- c(timings_cpp, total = sum(timings_cpp))

  timings_cpp <- cbind(s = round(timings_cpp, 4),
                       percent = round(100 * timings_cpp / timings_cpp["total"], 2))
  
  # ============================================================================
  # CONSTRUCT RESULTS OBJECT
  # ============================================================================
  
  # Helper to safely extract a metric from the leaderboard for the specific item set
  get_metric <- function(col_name) {
    if (col_name %in% colnames(leaderboard)) {
      return(leaderboard[[col_name]][item.set])
    }
    return(NA) # Returns NA if column doesn't exist (e.g., if CV changed names)
  }
  
  
  results_object <- list(
    r = ifelse(!is.null(leaderboard$r),leaderboard$r[item.set],NA),
    binarised_r = ifelse(!is.null(leaderboard$binarised_r),leaderboard$binarised_r[item.set],NA),
    youden_j = ifelse(!is.null(leaderboard$youden_j),leaderboard$youden_j[item.set],NA),
    output = leaderboard[seq_len(min(n.sets, nrow(leaderboard))), ],
    leaderboard = leaderboard,
    item_cors = ind_cors,
    best_names = best_items$best_names[[item.set]],
    best_indices = best_items$best_indices[[item.set]],
    best_item_cors = ind_cors[item.set],
    best_item_keys = ind_keys,
    scores = if (!is.null(computed_scores)) as.matrix(computed_scores)[, , drop = F] else NULL,
    target = target_all,
    original_items = original_names,
    filtered_items = filtered_names,
    final_pool_items = colnames(data),
    search = list(
      type = if (needs_polish) "optimised" else "exhaustive",
      combinations = num_combinations,
      searched = searched_combinations,
      polish = if (!is.null(cpp_results$polish_info)) within(cpp_results$polish_info, {
        swapped_in <- original_names[swapped_in]
        swapped_out <- original_names[swapped_out]
      })
    ),
    pivoting_item = pivot_item,
    timings = list(timings_r = timings_r, timings_cpp = timings_cpp),
    params = c(
      n.items = n.items,
      n.sets = n.sets,
      item.set = item.set,
      item.names = item.names,
      r.sq = r.sq,
      generate = generate,
      show.progress = show.progress,
      cross.validated = cross.validate,
      optimise = optimise,
      prefilter.ratio = prefilter.ratio,
      opt.n = opt.n,
      ceiling = ceiling,
      rfe.budget = rfe.budget,
      scale.vars = scale.vars,
      na.rm = na.rm,
      method = if (is.null(method)) NA else method,
      ranking_metric = ranking_metric,
      speed = speed,
      verbose = verbose,
      sample_size = nrow(data),
      final_pool_size = num_choose_from
    )
  )
  
  # Binary classification metrics
  if (is_binary) {
    bin_info <- list(
      cutoff = leaderboard$`>=`[item.set],
      ranking_metric = ranking_metric,
      is_cv = cross.validate
    )

    if (cross.validate) {
      bin_info$train <- list(
        binarised_r = get_metric("binarised_r"),
        sum_score_r = get_metric("r"),
        youden_j    = get_metric("youden_j"),
        auc         = get_metric("auc")
      )

      bin_info$holdout <- list(
        binarised_r = get_metric("binarised_r_holdout"),
        sum_score_r = get_metric("r_holdout"),
        youden_j    = get_metric("youden_j_holdout"),
        auc         = get_metric("auc_holdout")
      )
    } else {
      bin_info$results <- list(
        binarised_r = get_metric("binarised_r"),
        sum_score_r = get_metric("r"),
        youden_j    = get_metric("youden_j"),
        auc         = get_metric("auc")
      )
    }
    
    results_object$binary_info <- bin_info
  }

  # "\r" + full-width padding overwrites the "~{ binaryCutoff }~ searching"
  # line (if one was printed) rather than leaving it dangling above this
  if (show.progress) {
    completed_width <- max(getOption("width", 80L) - 1L, 20L)
    cat("\r", formatC("~{    reduceTo    }~ completed", width = -completed_width), "\n", sep = "")
  }

  class(results_object) <- "reduced_scale"
  return(results_object)
}

#' Print Method for reduced_scale Objects
#' @param x A \code{reduced_scale} object returned by \code{reduceTo()}
#' @param ... Unused
#' @export
print.reduced_scale <- function(x, ...) {
  is_binary <- !is.null(x$binary_info)
  
  # --- HEADER ---
  type_str <- if(as.numeric(x$params[["cross.validated"]]) > 0) "Cross-Validated" else "Optimal"
  cat("\n=~=", type_str, "Short-Form Scale Results =~=\n")
  
  if (is_binary) {
    metric_map <- c("youden_j" = "Youden's J",
                    "binarised_r" = "Binarised Correlation",
                    "sum_score_r" = "Sum Score Correlation",
                    "auc" = "AUC")
    
    m_name <- metric_map[x$binary_info$ranking_metric]
    if(is.na(m_name)) m_name <- x$binary_info$ranking_metric
    cat(paste0("Objective: Binary Classification (Ranked by ", m_name, ")\n"))
  }
  
  # --- SEARCH: guaranteed or heuristic ---
  if (!is.null(x$search)) {
    fmt <- function(n) format(n, big.mark = ",", scientific = FALSE)
    if (identical(x$search$type, "exhaustive")) {
      cat(paste0("Search: exhaustive -- the best of all ", fmt(x$search$combinations), " possible sets\n"))
    } else {
      cat(paste0("Search: optimised (Synergistic RFE + swap polish) -- ", fmt(x$search$combinations),
                 " possible sets, ", fmt(x$search$searched), " searched exhaustively; not guaranteed optimal\n"))
      pol <- x$search$polish
      if (!is.null(pol) && length(pol$swapped_in) > 0) {
        cat(sprintf("Polish: r %.5f -> %.5f (in: %s; out: %s)\n", pol$r_search, pol$r_polished,
                    paste(pol$swapped_in, collapse = ", "), paste(pol$swapped_out, collapse = ", ")))
      } else if (!is.null(pol)) {
        cat("Polish: no improving swaps found\n")
      }
    }
  }

  # --- LEADERBOARD TABLE ---
  cat("\nTop", nrow(x$output), "Combinations:\n")
  print(format(x$output, digits = 6))
  
  cat("\n=~=~~=~=~~=~~=~=~~=~=\n")
  
  # --- BEST ITEM DETAILS ---
  cat(paste0("\nSelected Set (Rank ",x$params[["item.set"]],"):"))
  if (!is.null(x$best_names)) {
    cat("\nItems (Names): ", paste(x$best_names, collapse = ", "))
  }
  if (!is.null(x$best_indices)) {
    cat("\nItems (Indices): ", paste(x$best_indices, collapse = ", "))
  }
  if (!is.null(x$best_item_cors)) {
    cat("\nItem:target correlations: ", paste(x$best_item_cors),"\n")
  }
  
  # --- BINARY PERFORMANCE BLOCK ---
  if (is_binary) {
    info <- x$binary_info
    
    cat("\n=~= Binary Classification Performance =~=")
    cat("\nOptimal Cutoff: Sum Score >=", info$cutoff)
    
    if (info$is_cv) {
      # Cross-Validated: Show Side-by-Side Comparison
      cat("\n\nMetric                 Training   Holdout")
      cat("\n-----------------------------------------")
      cat("\n=~=~~=~=~~=~=~~=~~=~+~=~~=~~=~=~~=~=~~=~=")
      cat(sprintf("\nBinarised Correlation  %8.3f  %8.3f",
                  info$train$binarised_r, info$holdout$binarised_r))
      cat(sprintf("\nYouden's J             %8.3f  %8.3f",
                  info$train$youden_j, info$holdout$youden_j))
      cat(sprintf("\nAUC                    %8.3f  %8.3f",
                  info$train$auc, info$holdout$auc))
      cat(sprintf("\n(Sum Score Correlation %8.3f  %8.3f)",
                  info$train$sum_score_r, info$holdout$sum_score_r))
      cat("\n=~=~~=~=~~=~=~~=~~=~+~=~~=~~=~=~~=~=~~=~=")
      
    } else {
      # Standard: Show Single Column
      cat("\n\nMetric                 Value")
      cat("\n-----------------------------")
      cat("\n=~=~~=~=~~=~=~+~=~=~~=~=~~=~=")
      
      cat(sprintf("\nBinarised Correlation  %.3f", info$results$binarised_r))
      cat(sprintf("\nYouden's J             %.3f", info$results$youden_j))
      cat(sprintf("\nAUC                    %.3f", info$results$auc))
      cat(sprintf("\n(Sum Score Correlation %.3f)", info$results$sum_score_r))
      cat("\n-----------------------------\n")
      cat("\n=~=~~=~=~~=~=~+~=~=~~=~=~~=~=\n")
    }
  }
  
  # --- FOOTER ---
  if (!is.null(x$scores)) {
    cat("\n(Scores generated in $scores)\n")
  }
}