#' Find Optimal Short-Form Scales
#'
#' Systematically evaluates all possible subsets of items from a larger scale to 
#' find the combination that maximises either internal consistency or correlation 
#' with an external criterion. Runs on a parallelised C++ backend with 
#' memory-optimised data structures (8-bit compression) to evaluate millions of 
#' combinations in seconds.
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
#'   score at each step. If FALSE, forces exhaustive search regardless of \code{ceiling}
#'   (can be slow)
#' @param prefilter.ratio Drops items whose relevance (correlation with the target, or
#'   item-total centrality with no target) is more than \code{prefilter.ratio} times
#'   weaker than the strongest item, before optimisation runs. Never prunes below
#'   \code{n.items} columns. Set to \code{Inf} or \code{NULL} to disable (default: 5)
#' @param opt.n The maximum number of cases (rows) to subsample during heuristic
#'   optimisation (default: 5000)
#' @param ceiling Combination threshold triggering optimisation (default: 10,000,000).
#'   A tighter ceiling narrows the item pool further before the final exhaustive search,
#'   which is faster but leaves less room to recover from an imperfect ranking. In
#'   testing, Synergistic RFE reliably found the true optimum except in
#'   extreme scenarios -- e.g. items whose value is invisible until combined with several
#'   (3+) specific others; a more generous ceiling protects against this rare case
#' @param scale.vars If TRUE, mean-centers and scales all columns (default: FALSE)
#' @param na.rm If TRUE, handles missing values via pairwise deletion (default: TRUE)
#' @param method Metric for ranking combinations (default: NULL for auto-selection):
#'   "r" for Pearson correlation, "youden_j" for Youden's Index, or "binarised_r"
#'   for correlation of binarised sum score (binary targets)
#' @param speed \code{"fast"} (default) scores combinations using a Gram-matrix
#'   shortcut, mean-imputing missing values for search only -- reported statistics are
#'   always recomputed from the true data. \code{"conservative"} scores every
#'   combination directly with pairwise deletion: no imputation, but slower
#' @param verbose If TRUE, prints informational messages (default: TRUE). Progress
#'   bars and optimisation-stage updates are controlled separately by \code{show.progress}
#'
#' @return A list of class \code{reduced_scale} containing:
#' \describe{
#'   \item{output}{Data frame of the top n.sets combinations and their performance metrics}
#'   \item{leaderboard}{Extended data frame of the top combinations (100, or 1000 when ranking by Youden's J)}
#'   \item{best_names}{Character vector of item names in the top-ranked set}
#'   \item{best_indices}{Integer vector of column indices in the top-ranked set}
#'   \item{scores}{(If generate = TRUE) A data frame containing the sum scores}
#'   \item{binary_info}{(If binary target) A list containing optimal cutoffs and classification metrics}
#'   \item{params}{Named vector of parameters used in the function call}
#' }
#'
#' @author Paddy Maher, Max Planck Institute for Human Development, MPRG Biosocial
#' @export
#' @useDynLib reduceTo, .registration = TRUE
#' @importFrom Rcpp sourceCpp
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
                     opt.n = 5000, ceiling = 1e7,
                     scale.vars = FALSE, na.rm = TRUE, method = NULL, speed = c("fast", "conservative"),
                     verbose = TRUE){

  speed <- match.arg(speed)

  # Preserve the caller's random state (reduceTo() seeds its own sampling internally)
  if (exists(".Random.seed", envir = globalenv())) {
    old_seed <- get(".Random.seed", envir = globalenv())
    on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
  }

  # ============================================================================
  # HELPER FUNCTIONS
  # ============================================================================
  
  # Validate that a column is usable (numeric, has variance, no infinite values)
  is_valid_column <- function(x) {
    is.numeric(x) && !all(is.na(x)) && !any(is.infinite(x)) && sd(x, na.rm = TRUE) > 0
  }
  
  # Reverse-score items negatively correlated with the target
  flip_items <- function(x, the_items) {

    should_flip <- if(is.logical(the_items)) any(the_items, na.rm = TRUE) else length(the_items) > 0

    if (should_flip) {
      if (scale.vars) {
        x[, the_items] <- x[, the_items] * -1
      } else {
        # max - x, per column, using training-set maxes so holdout data flips consistently
        col_maxes <- apply(data[, the_items, drop = FALSE], 2, max, na.rm = TRUE)
        cols_to_mod <- x[, the_items, drop = FALSE]
        x[, the_items] <- -1 * sweep(cols_to_mod, 2, col_maxes, "-")
      }
    }
    return(x)
  }
  
  # Reproducible row subsample (capped at 2000) used for correlation-based item flipping
  get_cor_subsample <- function(n_rows) {
    set.seed(1)
    if (n_rows > 2000) sample(1:n_rows, 2000) else 1:n_rows
  }

  # Cutoff search + AUC for binary targets, sharing ONE grouping pass.
  #
  # The previous version vectorized the cutoff search across all N rows via
  # outer(pos_scores, possible_integers, ">="), and separately grouped AUC
  # by exact unique score value. Both were still fundamentally O(N x cutoffs)
  # and O(N) respectively per combination. But item-sum scores repeat
  # heavily -- even under realistic na.rm = TRUE missingness, only ~100-300
  # distinct values typically occur out of thousands of rows (stress-tested
  # across n.items 6-15 and missingness rates 15-60%) -- so grouping FIRST
  # (one hash-based O(N) pass via unique()+match()+tabulate()) and running
  # the cutoff search and AUC over that much smaller unique-value set (U,
  # not N) does the same math on a far smaller array. Measured ~17x faster
  # than the previous version on realistic pipeline-shaped data (N=20,000,
  # 1000 combinations), verified exact via brute force across 2000 trials
  # (integer, fractional/na.rm=TRUE, continuous, and heavily-tied scores).
  #
  # Cutoff search: uses outer(uniq, possible_integers, ">=") -- a U x
  # cutoffs matrix instead of N x cutoffs -- then two matrix multiplies
  # (per-unique-value counts against the mask) give tp(c)/fp(c) for every
  # candidate cutoff at once. possible_integers is still THIS combination's
  # own observed range (not a shared grid), so there's no risk of a trivial
  # out-of-range cutoff winning a tie -- same guarantee the earlier
  # per-combination version had.
  #
  # AUC: the Mann-Whitney U identity from the previous version, reusing the
  # same P/Q unique-value counts instead of recomputing them separately:
  #   AUC * n_pos * n_neg = sum_v P[v] * (CumQ[v] + 0.5 * Q[v])
  #
  # Tie-breaking: different cutoffs achieving mathematically-equal
  # |correlation| or Youden's J is common, not a rare edge case. An exact
  # which.max() comparison lets tiny floating-point differences between
  # computation paths pick different winners among genuinely tied
  # candidates -- treating near-ties (within 1e-9) as ties and
  # deterministically keeping the smallest cutoff avoids that fragility.
  compute_binary_metrics <- function(scores, binary_target, optimize_for = "youden_j") {
    valid_idx <- !is.na(scores) & !is.na(binary_target)
    scores_clean <- scores[valid_idx]
    target_clean <- binary_target[valid_idx]

    n_pos <- sum(target_clean == 1)
    n_neg <- sum(target_clean == 0)
    possible_integers <- floor(range(scores_clean)[1]):ceiling(range(scores_clean)[2])

    if (n_pos == 0 || n_neg == 0) {
      return(list(optimal_integer_cutoff = median(possible_integers),
                  binarised_r = 0, youden_j = NA, auc = NA))
    }

    uniq <- sort(unique(scores_clean))
    bin_idx <- match(scores_clean, uniq)
    P <- tabulate(bin_idx[target_clean == 1], nbins = length(uniq))
    Q <- tabulate(bin_idx[target_clean == 0], nbins = length(uniq))

    cum_neg_before <- cumsum(Q) - Q
    auc <- sum(P * (cum_neg_before + 0.5 * Q)) / (n_pos * n_neg)

    above_mask <- outer(uniq, possible_integers, `>=`)
    tp_c <- as.vector(P %*% above_mask)
    fp_c <- as.vector(Q %*% above_mask)
    tn_c <- n_neg - fp_c
    fn_c <- n_pos - tp_c

    youden_c <- tp_c / n_pos + tn_c / n_neg - 1

    phi_num <- tp_c * tn_c - fp_c * fn_c
    phi_denom <- sqrt((tp_c + fp_c) * (tp_c + fn_c) * (tn_c + fp_c) * (tn_c + fn_c))
    phi_c <- ifelse(phi_denom == 0, NA, phi_num / phi_denom)

    search_vec <- if (optimize_for == "binarised_r") abs(phi_c) else youden_c
    best_idx <- which(search_vec >= max(search_vec, na.rm = TRUE) - 1e-9)[1]

    list(
      optimal_integer_cutoff = possible_integers[best_idx],
      binarised_r = if (is.na(phi_c[best_idx])) 0 else phi_c[best_idx],
      youden_j = youden_c[best_idx],
      auc = auc
    )
  }

  # Standalone AUC (same grouped Mann-Whitney U as inside
  # compute_binary_metrics above) for contexts that only need AUC, not a
  # cutoff search -- e.g. holdout scoring, where the cutoff was already
  # fixed from the training set and re-searching it would be both wrong and
  # wasted work. Kept separate (small duplication of the grouping math)
  # rather than sharing compute_binary_metrics, since that function's whole
  # point is fusing the search and AUC into one grouping pass for the
  # training-set path -- calling out to a shared helper here would either
  # lose that fusion or force this simpler caller through an unneeded search.
  compute_auc <- function(scores, binary_target) {
    valid <- !is.na(scores) & !is.na(binary_target)
    s <- scores[valid]
    t <- binary_target[valid]
    n_pos <- sum(t == 1)
    n_neg <- sum(t == 0)
    if (n_pos == 0 || n_neg == 0) return(NA)

    uniq <- sort(unique(s))
    bin_idx <- match(s, uniq)
    pos_counts <- tabulate(bin_idx[t == 1], nbins = length(uniq))
    neg_counts <- tabulate(bin_idx[t == 0], nbins = length(uniq))
    cum_neg_before <- cumsum(neg_counts) - neg_counts

    u_stat <- sum(pos_counts * (cum_neg_before + 0.5 * neg_counts))
    u_stat / (n_pos * n_neg)
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

  # Bounds intermediate scoring cost during Synergistic RFE, not final pool
  # size (tuned empirically against recovery-rate tests, not a formula).
  # Defined once here (rather than inside perform_synergy_ranked_elimination)
  # so predict_narrowing_final_pool_size below can replay the exact same
  # round-sizing decisions when estimating runtime, without duplicating the
  # constant and risking drift between the real run and the estimate.
  ROUND_BUDGET <- 1000000

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
    k <- min(2, n.items)
    max_rounds <- n.items + 5
    round_i <- 0
    while (choose(pool_size, n.items) > ceiling && round_i < max_rounds) {
      round_i <- round_i + 1
      next_k <- min(k + 1, n.items)
      budget_for_next <- if (next_k >= n.items) ceiling else ROUND_BUDGET
      pool_size <- largest_pool_under_budget(pool_size, next_k, budget_for_next, n.items)
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
      budget <- if (next_k >= n.items) ceiling else ROUND_BUDGET
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

    RANK_KEEP_TOP <- 10000   # top combos used to rank items each round

    # \r only returns to the start of the CURRENT visual line, not the whole
    # logical line -- if the printed text is wider than the console, it wraps
    # onto a second visual line that \r can never reach, leaving stray
    # remnants behind on later, shorter redraws. Bounding every redraw to a
    # fixed width no wider than the console guarantees it always fits one
    # visual line, so \r can fully clear/overwrite it.
    prog_width <- max(getOption("width", 80L) - 1L, 20L)

    pool <- current_cols
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
          keep_top = RANK_KEEP_TOP,
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

      # Rank items by the best |r| among returned combinations containing
      # them, not raw frequency (a common-but-mediocre item shouldn't
      # outrank a rare-but-excellent one)
      combo_indices_list <- lapply(strsplit(cpp_result$combination, ','), as.integer)
      flat_items <- unlist(combo_indices_list)
      flat_r <- rep(abs(cpp_result$r), lengths(combo_indices_list))
      valid <- !is.na(flat_r)
      item_best_flat <- tapply(flat_r[valid], flat_items[valid], max)

      item_best <- setNames(rep(-Inf, n_pool), as.character(pool))
      item_best[names(item_best_flat)] <- item_best_flat
      ranked <- pool[order(item_best[as.character(pool)], decreasing = TRUE)]

      # Look-ahead narrowing: shrink the pool now so that NEXT round's score
      # (at next_k) stays within budget -- narrowing based on this round's
      # own k would be too late, since this round's score already happened
      # at the current (un-narrowed) pool size. When the next round is the
      # real target k, narrow to `ceiling` directly (not ROUND_BUDGET) --
      # ROUND_BUDGET only bounds the cost of intermediate scoring calls, it
      # has no business capping the final pool below what the caller's own
      # ceiling would otherwise allow.
      next_k <- min(k + 1, n.items)
      budget_for_next <- if (next_k >= n.items) ceiling else ROUND_BUDGET
      target_pool_size <- largest_pool_under_budget(n_pool, next_k, budget_for_next, n.items)

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

    return(pool)
  }

  compress_for_cpp <- function(data) {
    compressed <- matrix(0L, nrow = nrow(data), ncol = ncol(data))
    
    for (i in 1:ncol(data)) {
      x <- data[, i]
      
      # Handle NAs
      na_mask <- is.na(x)
      
      # Find range
      min_val <- min(x, na.rm = TRUE)
      max_val <- max(x, na.rm = TRUE)
      range_val <- max_val - min_val
      
      # Determine if the column is entirely integers (allow for floating-point noise)
      is_integer_like <- all(abs(x[!na_mask] - round(x[!na_mask])) < 1e-8)
      
      if (range_val == 0) {
        # Constant column
        compressed[, i] <- 0L
      } else if (range_val <= 254 && is_integer_like) {
        # Already fits AND is integers - just shift to 0
        compressed[, i] <- as.integer(x - min_val)
      } else {
        # Continuous decimals OR range > 254 - Scale proportionally to 0-254 buckets
        compressed[, i] <- as.integer((x - min_val) * 254 / range_val)
      }
      
      # Mark NAs as 255
      compressed[na_mask, i] <- 255L
    }
    
    return(compressed)
  }
  
  # Score every combination and build the leaderboard
  process_combinations_in_batches <- function(data, targ, num_choose_from,
                                              original_indices, na.rm,
                                              is_binary, n.items, ranking_metric,
                                              optimize_for, show.progress, keep_top,
                                              speed) {

    used_fast_path <- identical(speed, "fast")

    if (used_fast_path) {
      # Score via the Gram-matrix shortcut (missing values mean-imputed for search only)
      gc_components <- build_gram_components(data, targ)
      mark_time("gram_matrix_build")

      cpp_result <- process_all_combinations_cpp_gram(
        gram = gc_components$gram,
        col_sums = gc_components$col_sums,
        col_target_dots = gc_components$col_target_dots,
        sum_target = gc_components$sum_target,
        sum_target_sq = gc_components$sum_target_sq,
        n_valid = gc_components$n_valid,
        n_items = n.items,
        num_choose_from = num_choose_from,
        original_indices = original_indices,
        keep_top = keep_top,
        show_progress = show.progress
      )
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

    # Recompute exact statistics for the top `keep_top` combinations from the
    # true data (both scoring engines above work from approximated data).
    # Vectorized: rather than looping rowMeans()/cor() once per combination
    # (each paying its own R-interpreter overhead), build one N x keep_top
    # "membership matrix" (1 where an item is used by that combination) and
    # get every combination's score in a single matrix multiply, then every
    # combination's correlation in a single cor() call.
    {
      n_top <- nrow(leaderboard)
      top_combos <- strsplit(leaderboard$combination, ',')
      local_cols_list <- lapply(top_combos, function(cids) match(as.numeric(cids), original_indices))

      membership <- matrix(0L, nrow = ncol(data), ncol = n_top)
      membership[cbind(unlist(local_cols_list), rep(seq_len(n_top), each = n.items))] <- 1L

      data_mat <- as.matrix(data)
      mark_time("membership_matrix")

      # Zeroed-NA sum and valid-item count per row per combination, both via
      # matrix multiply. Plain data_mat %*% membership would be wrong here:
      # in R, NA * 0 is NA, so any row with an NA ANYWHERE in the pool would
      # contaminate every combination's sum, not just combinations that
      # actually use that item -- zeroing NAs first avoids that.
      valid_mat <- (!is.na(data_mat)) * 1
      data_zeroed <- data_mat
      data_zeroed[is.na(data_mat)] <- 0

      sum_matrix <- data_zeroed %*% membership
      count_matrix <- valid_mat %*% membership

      if (na.rm) {
        # Pro-rated mean of whichever items were valid for that row, scaled
        # back up to n.items -- matches rowMeans(..., na.rm = TRUE) * n.items
        scores_matrix <- (sum_matrix / count_matrix) * n.items
      } else {
        # Matches rowMeans(..., na.rm = FALSE) * n.items: NA unless every
        # selected item was present for that row
        scores_matrix <- sum_matrix
        scores_matrix[count_matrix < n.items] <- NA
      }
      mark_time("vectorised_scoring")

      refined_r <- as.vector(suppressWarnings(cor(scores_matrix, targ, use = "pairwise.complete.obs")))
      mark_time("vectorised_correlation")

      leaderboard$r <- refined_r

      if (is_binary) {
        binarised_r <- numeric(n_top)
        cutoff <- numeric(n_top)
        youden_j <- numeric(n_top)
        auc <- numeric(n_top)

        # Per-combination cutoff search + AUC isn't linear algebra (each
        # needs its own grouping over that combination's own score
        # distribution), so this stays a loop -- but each iteration now
        # groups by unique value (U, not N) instead of scanning all N rows,
        # and computes cutoff search + AUC together from one shared grouping
        for (i in seq_len(n_top)) {
          m <- compute_binary_metrics(scores_matrix[, i], targ, optimize_for)
          binarised_r[i] <- m$binarised_r
          cutoff[i] <- m$optimal_integer_cutoff
          youden_j[i] <- m$youden_j
          auc[i] <- m$auc
        }
        mark_time("binary_metrics")

        leaderboard$`>=` <- cutoff
        leaderboard$binarised_r <- binarised_r
        leaderboard$youden_j <- youden_j
        leaderboard$auc <- auc

        # Re-rank by the chosen metric
        leaderboard <- leaderboard[order(abs(leaderboard[[ranking_metric]]),
                                         decreasing = TRUE), ]
        mark_time("binary_reranking")
      }
    }

    cpp_results <- list(leaderboard = leaderboard,
                        timings_cpp = cpp_result$timings_cpp,
                        used_fast_path = used_fast_path)

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
  cross_validate_leaderboard <- function(leaderboard, data, target,
                                         na.rm, is_binary, best_names) {

    if (nrow(data) == 0) stop("Holdout data is empty. Cannot cross-validate.")

    n_rows <- nrow(data)
    n_cols <- nrow(leaderboard)
    scores_matrix <- matrix(NA, nrow = n_rows, ncol = n_cols)

    for (i in 1:n_cols) {
      current_items <- best_names[[i]]
      if (length(current_items) == 1) {
        scores_matrix[, i] <- data[[current_items]]
      } else {
        scores_matrix[, i] <- rowMeans(data[, current_items, drop = FALSE], na.rm = na.rm) * length(current_items)
      }
    }

    if (is_binary) {
      binary_matrix <- matrix(NA, nrow = n_rows, ncol = n_cols)
      for (i in 1:n_cols) {
        binary_matrix[, i] <- as.numeric(scores_matrix[, i] >= leaderboard$`>=`[i])
      }
    }

    leaderboard$r_holdout <- as.vector(cor(scores_matrix, target, use = 'pairwise.complete.obs'))

    if (is_binary) {
      leaderboard$binarised_r_holdout <- as.vector(cor(binary_matrix, target, use = 'pairwise.complete.obs'))

      leaderboard$youden_j_holdout <- sapply(1:n_cols, function(col_idx) {
        preds <- binary_matrix[, col_idx]
        actual <- target

        valid <- !is.na(preds) & !is.na(actual)
        preds <- preds[valid]
        actual <- actual[valid]

        tp <- sum(preds == 1 & actual == 1)
        tn <- sum(preds == 0 & actual == 0)
        fp <- sum(preds == 1 & actual == 0)
        fn <- sum(preds == 0 & actual == 1)

        if ((tp + fn) == 0 || (tn + fp) == 0) return(NA)

        sensitivity <- tp / (tp + fn)
        specificity <- tn / (tn + fp)
        return(sensitivity + specificity - 1)
      })

      leaderboard$auc_holdout <- sapply(1:n_cols, function(col_idx) {
        compute_auc(scores_matrix[, col_idx], target)
      })
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
    low <- format_duration(low_seconds)
    high <- format_duration(high_seconds)

    low_unit <- sub("^[0-9,]+ ", "", low)
    high_unit <- sub("^[0-9,]+ ", "", high)

    if (identical(sub("s$", "", low_unit), sub("s$", "", high_unit))) {
      low_count <- sub(" .*$", "", low)
      return(paste0(low_count, " to ", high))
    }

    paste0(low, " to ", high)
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
  
  # Handle non-standard evaluation for target parameter
  if (!is.null(target_expr) && is.symbol(target_expr)) {
    target_name <- as.character(target_expr)
    
    # Check if this symbol refers to a column in data
    if (target_name %in% colnames(data)) {
      target <- data[[target_name]]
      # Replace column with NAs so it's filtered out but indices remain correct
      data[[target_name]] <- NA
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
  
  
  # Detect binary target and notify user
  is_binary <- FALSE
  target_supplied <- !is.null(target) 
  
  
  if (!is.null(target)) {
    unique_vals <- unique(target[!is.na(target)])
    is_binary <- length(unique_vals) == 2 && all(unique_vals %in% c(0, 1))
    
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
    valid_binary_methods <- c("r", "binarised_r", "youden_j")

    if (is.null(method)) {
      ranking_metric <- "youden_j"
      if (verbose) message("=~= Ranking combinations by Youden's J (use method = 'binarised_r' or 'r' to change)")
    } else if (method %in% valid_binary_methods) {
      ranking_metric <- method
    } else {
      stop(paste0("For binary targets, method must be one of: ",
                  paste(valid_binary_methods, collapse = ", ")))
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

  # Youden's J needs a wider leaderboard: earlier stages rank by r, which can
  # discard combinations with weak r but strong classification performance
  leaderboard_length <- if (identical(ranking_metric, "youden_j")) 1000 else 100

  mark_time("target_resolution")
  
  # Ensure column names exist and save them
  if (is.null(colnames(data))) {
    colnames(data) <- paste0("Col_", 1:ncol(data))
  }
  original_names <- colnames(data)  # Save original column names
  all_original_indices <- 1:ncol(data)
  
  # Identify valid numeric columns
  if (is.matrix(data) || is.array(data)) {
    valid_mask <- apply(data, 2, is_valid_column)
  } else {
    valid_mask <- sapply(data, is_valid_column)
  }
  valid_mask[is.na(valid_mask)] <- FALSE
  
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
  
  # if cross-validating, create row indices for training and holdout samples
  if (cross.validate) {
    if (cross.validate < 0 || cross.validate > 1){
      stop("Error: cross.validate must be TRUE, FALSE, or a numeric value between 0 and 1.")
    }
    if (cross.validate == T) cross.validate <- 0.75
    cv_subset_size <- round(nrow(data) * cross.validate,0)
    set.seed(1)
    cv_subset <- sample(1:nrow(data), cv_subset_size)
    cv_holdout <- setdiff(1:nrow(data), cv_subset)
    }
  
  mark_time("cv_split_setup")
  
  # flip items negatively correlated with most central item
  
  if (!target_supplied) {
    cor_subsample <- get_cor_subsample(nrow(data))

    dc <- cor(data[cor_subsample,], use = "p")
    centrality <- colSums(abs(dc), na.rm = TRUE)
    most_central_item <- order(centrality, decreasing = TRUE)[1]

    pivot_item <- colnames(data)[most_central_item]

    items_to_flip_TF <- dc[, most_central_item] < 0
    items_to_flip <- filtered_names[items_to_flip_TF]

    # Reverse-score items
    data <- flip_items(data, items_to_flip)

    #create target
    target <- rowMeans(data, na.rm = na.rm)

    # Used later to prefilter weak items before optimisation
    relevance <- centrality
  }

  if (target_supplied) {
    cor_subsample <- get_cor_subsample(nrow(data))

    # For external criteria: flip items negatively correlated with target
    dc <- cor(data[cor_subsample,], target[cor_subsample], use = "p")
    dc[is.na(dc)] <- 0

    items_to_flip_TF <- as.vector(dc) < 0
    items_to_flip <- filtered_names[items_to_flip_TF]
    items_to_zero <- (abs(as.vector(dc)) > 0.999999 | abs(as.vector(dc)) < 0.0001)
    data[,items_to_zero] <- 0

    data <- flip_items(data, items_to_flip)
    pivot_item <- NA

    # Used later to prefilter weak items before optimisation
    relevance <- abs(as.vector(dc))
  }
  
  if (cross.validate) {
    data_holdout <- data[cv_holdout,]
    target_holdout <- target[cv_holdout]
    data <- data[cv_subset,]
    target <- target[cv_subset]
  }
  
  mark_time("item_flipping")
  
  # Check if optimization is needed
  num_combinations <- choose(ncol(data), n.items)

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

        if (sum(keep_mask) >= n.items) {
          n_before <- length(cols)
          cols <- cols[keep_mask]
          if (length(cols) < n_before) {
            prefilter_message <- sprintf("=~ Prefilter: keeping %d of %d items (relevance >= 1/%g of the strongest item) before optimisation.",
                                         length(cols), n_before, prefilter.ratio)
          }
        }
      }

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
        while (p < max_p && choose(p, n.items) < target_combos) p <- p + 1
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
            n_items = n.items,
            num_choose_from = p,
            original_indices = 1:p,
            keep_top = 1,
            show_progress = FALSE
          ))
          list(elapsed = as.numeric(difftime(Sys.time(), t0, units = "secs")), combos = choose(p, n.items))
        }

      } else {
        run_calib_at <- function(p) {
          calib_compressed <- compress_for_cpp(data[, 1:p, drop = FALSE])
          t0 <- Sys.time()
          invisible(process_all_combinations_cpp_parallel_float(
            data = calib_compressed,
            n_items = n.items,
            num_choose_from = p,
            na_rm = na.rm,
            target = target,
            original_indices = 1:p,
            keep_top = 1,
            show_progress = FALSE
          ))
          list(elapsed = as.numeric(difftime(Sys.time(), t0, units = "secs")), combos = choose(p, n.items))
        }
      }

      p0 <- grow_pool_to_combos(TINY_CALIB_COMBOS, n.items, ncol(data))
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

      est_seconds <- num_combinations / combos_per_sec
      # Predict the final search's actual combination count by replaying
      # narrowing's own pool-size decisions (see predict_narrowing_final_pool_size
      # above) rather than assuming it uses the full `ceiling` budget --
      # binomial coefficients grow so steeply with n.items that a single
      # pool-size decrement can land well under ceiling, especially for
      # larger n.items, so that assumption can be wrong by orders of
      # magnitude on its own, independent of calibration accuracy.
      predicted_final_pool_size <- predict_narrowing_final_pool_size(length(cols), n.items, ceiling)
      predicted_final_combos <- choose(predicted_final_pool_size, n.items)
      opt_est_seconds <- predicted_final_combos / combos_per_sec

      if (verbose) {
        message(paste0("=~ This would generate ",
                       format(num_combinations, big.mark = ",",scientific = FALSE),
                       " combinations to compare (~",format_duration_range(est_seconds/2, est_seconds*2),
                       " with N = ",format(nrow(data), big.mark = ",",scientific = FALSE) ,
                       "). \n=~ Synergistic RFE will be used to reduce combinations to below ",
                       format(ceiling, big.mark = ",",scientific = FALSE),
                       " (search space: ",n.items," items from ",predicted_final_pool_size,
                       "; ~",format_duration_range(opt_est_seconds/2, opt_est_seconds*2),
                       ").",
                       " You can change this threshold with the 'ceiling' argument."))
        if (!is.null(prefilter_message)) message(prefilter_message)
      }

      cols <- perform_synergy_ranked_elimination(cols, data, n.items, ceiling, target, na.rm, opt.n, speed, show.progress)

    } else {
      if (verbose) {
        message(sprintf("=~ Note: %s combinations exceeds ceiling (%s) but optimise = FALSE -- running exhaustive search anyway. This may be slow.",
                        format(num_combinations, big.mark = ",", scientific = FALSE),
                        format(ceiling, big.mark = ",", scientific = FALSE)))
      }
    }
  }
  
  cols_names <- filtered_names[cols]
  
  mark_time("combination_optimisation")
  
  # Subset to optimized columns
  data <- data[, cols, drop = FALSE]
  if (cross.validate) { data_holdout <- data_holdout[, cols, drop = FALSE] }
  
  mark_time("column_subsetting")
  
  # Apply standardization if requested
  if (scale.vars) data <- apply(data, 2, scale)
  
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
                                                 speed)

  leaderboard <- cpp_results$leaderboard
  timings_cpp <- cpp_results$timings_cpp                                                                                                                   
  
  # Clean up leaderboard
  rownames(leaderboard) <- NULL
  
  mark_time("combination_scoring")
  
  # Parse leaderboard combinations once, shared by both steps below
  parsed_combos <- parse_leaderboard_combinations(leaderboard, original_indices)

  # Extract best items
  best_items <- extract_best_items(data, parsed_combos)

  mark_time("extract_best_items")

  # Calculate item-level correlations
  ind_cors <- calculate_item_correlations(data, target, items_to_flip, cols_names, parsed_combos)
  
  ind_keys <- 2*(as.numeric(strsplit(ind_cors[item.set], ',')[[1]]) > 0)-1
  
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
        leaderboard$sum_scored_R <- leaderboard$r^2
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
    output = leaderboard[1:n.sets, ],
    leaderboard = leaderboard,
    item_cors = ind_cors,
    best_names = best_items$best_names[[item.set]],
    best_indices = best_items$best_indices[[item.set]],
    best_item_cors = ind_cors[item.set],
    best_item_keys = ind_keys,
    scores = if (!is.null(computed_scores)) as.matrix(computed_scores)[, , drop = F] else NULL,
    target = target,
    original_items = original_names,
    filtered_items = filtered_names,
    final_pool_items = colnames(data),
    pivoting_item = pivot_item,
    timings = list(timings_r = timings_r, timings_cpp = timings_cpp),
    params = c(
      n.items = n.items,
      n.sets = n.sets,
      item.set = item.set,
      cross.validated = cross.validate,
      ranking_metric = ranking_metric,
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

  if (show.progress) cat("~{    reduceTo    }~ completed\n")

  class(results_object) <- "reduced_scale"
  return(results_object)
}

#' Print Method for reduced_scale Objects
#' @export
print.reduced_scale <- function(x, ...) {
  is_binary <- !is.null(x$binary_info)
  
  # --- HEADER ---
  type_str <- if(as.numeric(x$params[["cross.validated"]]) > 0) "Cross-Validated" else "Optimal"
  cat("\n=~=", type_str, "Short-Form Scale Results =~=\n")
  
  if (is_binary) {
    metric_map <- c("youden_j" = "Youden's J", 
                    "binarised_r" = "Binarised Correlation", 
                    "sum_score_r" = "Sum Score Correlation")
    
    m_name <- metric_map[x$binary_info$ranking_metric]
    if(is.na(m_name)) m_name <- x$binary_info$ranking_metric
    cat(paste0("Objective: Binary Classification (Ranked by ", m_name, ")\n"))
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