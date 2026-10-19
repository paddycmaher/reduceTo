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
#'   \item \strong{Knockout Search}: When the number of combinations exceeds the computational ceiling, the function trims the scale, exhaustively searches the item choices that are closest calls around the best short form found, then knocks out each of its items in turn and searches again without it -- Leave One Candidate Out, LOCO (see \code{optimise}).
#'   \item \strong{Cross-Validation}: Supports a Train/Holdout split (default 75/25) to validate findings and prevent overfitting. Reports performance metrics for both the training and holdout samples side-by-side.
#'   \item \strong{Binary Classifications}: For binary targets (0/1), automatically finds the optimal cut-off score (a whole number of points, or with \code{scale.vars = TRUE} any value) to maximise classification accuracy (Youden's J) or binarised correlation. AUC (threshold-independent) is also reported.
#' }
#'
#' @param data Matrix or data.frame containing item responses
#' @param n.items Desired number of items in the final short-form scale
#' @param target Optional target criterion. Can be specified as: NULL (default) for 
#'   internal consistency, a column name from data (quoted or not), or an external vector 
#'   (numeric criterion or binary 0/1 for classification)
#' @param method Metric for ranking combinations (default: NULL for auto-selection):
#'   "r" for Pearson correlation, "youden_j" for Youden's Index, "binarised_r"
#'   for correlation of binarised sum score, or "auc" for the area under the
#'   ROC curve (binary targets only; ignored otherwise)
#' @param cross.validate Numeric input controlling a data split into Training/Holdout
#'   sets; if TRUE or 1, uses a 75%/25% split (default: FALSE)
#' @param n.sets Number of top-performing item sets to return (default: 5)
#' @param item.names If TRUE, output lists item names instead of column numbers (default: FALSE)
#' @param r.sq If TRUE, returns R² alongside correlation (default: FALSE)
#' @param generate If TRUE, returns computed scores for selected item set (default: TRUE)
#' @param item.set Which ranked set to generate scores for, used with generate = TRUE (default: 1)
#' @param na.rm If TRUE, handles missing values via pairwise deletion (default: TRUE)
#' @param scale.vars If TRUE, mean-centers and scales all columns, using the training
#'   rows' means/SDs when cross-validating (default: FALSE). With a binary target, the cutoff
#'   can then be any value of the (standardised) sum score, not just a whole number
#' @param speed \code{"fast"} (default) scores combinations using a Gram-matrix
#'   shortcut, mean-imputing missing values for search only -- reported statistics are
#'   always recomputed from the true data. \code{"conservative"} scores every
#'   combination directly with pairwise deletion: no imputation, but slower (so its
#'   \code{ceiling} default is much tighter). Recommended when
#'   \code{na.rm = FALSE} and the data have substantial or patterned missingness (e.g.
#'   whole sections skipped), where each combination is scored on its own complete cases
#'   and mean-imputed search can miss the best set. When combinations exceed \code{ceiling},
#'   the knockout search always uses the Gram-matrix shortcut; \code{"conservative"} then also scores
#'   every combination of the final round's closest calls directly with pairwise deletion
#' @param ceiling Combination threshold triggering optimisation (default: 1,000,000,000,000;
#'   10,000,000 with \code{speed = "conservative"}). Each of the knockout search's exhaustive searches
#'   scores at most \code{knockout.budget} combinations (\code{ceiling / 1000} unless set); a run typically takes a few to
#'   converge, then one or two for each item knocked out (about 28 on the test problems).
#'   In testing on 765 problems needing a knockout search with searches of up to 10,000,000 combinations,
#'   it ranked the true best set first in all of them and recovered 99.9% of the exhaustive top 50 sets
#' @param optimise If NULL (default), runs a knockout search when combinations exceed
#'   \code{ceiling}; if TRUE, runs one for any problem, however small; if FALSE, forces exhaustive
#'   search regardless of \code{ceiling} (can be slow). The knockout search, starting from the full scale,
#'   repeatedly drops the item whose loss
#'   lowers r least until \code{n.items} remain. It then finds the closest calls: items whose
#'   swap into or out of this set, alone or with one other swap, would lower r least, and
#'   scores every combination of as many of them as \code{knockout.budget} allows, holding the
#'   other items fixed, repeating from any better set found. It then does the same with each
#'   item of the best set left out in turn (Leave One Candidate Out, LOCO) -- or, when at
#'   least 20 items are left out of the set, with each of those put back in turn. Finally, all of this runs again at a
#'   tenth of \code{knockout.budget}, a hundredth, and so on down to 1 (the sweepdown), and the best set from any of
#'   them is kept: larger budgets take bigger steps and can settle on a different near-equal best set, so without
#'   this a larger budget could occasionally return a worse one; with it, ten times the budget never does. Most of
#'   the smaller searches are read off the larger ones', adding up to about 15% to the search time. The best sets
#'   found make up the leaderboard, re-scored exactly
#' @param knockout.budget The most combinations each of the knockout search's searches may score
#'   (default: NULL, which uses \code{ceiling / 1000}). The search also runs at a tenth of it, a hundredth, and so
#'   on (see \code{optimise})
#' @param prefilter.ratio Drops items whose relevance (correlation with the target, or
#'   item-total centrality with no target) is more than \code{prefilter.ratio} times
#'   weaker than the strongest item, before the knockout search runs. Only applies when at least
#'   twice \code{n.items} items survive. Off by default (\code{Inf}): the knockout search handles weak
#'   items itself, and items that look weak alone can balance others -- in testing, a ratio
#'   of 5 removed an item of the true best set in about 1 in 10 problems needing a knockout search.
#'   Set e.g. 5 to speed up very large pools (default: Inf)
#' @param verbose If TRUE, prints informational messages (default: TRUE). Progress
#'   bars and optimisation-stage updates are controlled separately by \code{show.progress}
#' @param show.progress If TRUE, displays a live progress bar during search (default: TRUE)
#' @param seed Random seed for the cross-validation split, and for the 10,000-row subsample used for
#'   the item-target correlations when there are more rows (default: 1, so a run can be repeated exactly;
#'   the caller's random state is restored afterwards). \code{NULL} draws from the session's random
#'   state instead (advancing it, like \code{sample()})
#'
#' @return A list of class \code{reduced_scale}. Statistics describe the selected set (rank
#'   \code{item.set}) on the rows the search used -- the training rows when cross-validating:
#' \describe{
#'   \item{r}{Correlation of the selected set's sum score with the target}
#'   \item{binarised_r, youden_j, auc, cutoff}{Binary targets: classification metrics and the
#'     sum-score cutoff (\code{>=}); NA otherwise}
#'   \item{r_holdout}{Cross-validated: r on the holdout rows; NA otherwise}
#'   \item{alpha}{Cronbach's alpha of the selected set, as scored}
#'   \item{r_parent}{Correlation of the selected set's sum score with the full scale's (every
#'     valid item, keyed as in the search)}
#'   \item{selected}{The selected set: \code{rank}, \code{items} (names), \code{indices}
#'     (column numbers), \code{item_r} (each item's correlation with the target, in its
#'     original direction) and \code{keys} (-1 for reverse-keyed items)}
#'   \item{top}{Data frame of the top \code{n.sets} combinations and their metrics}
#'   \item{leaderboard}{The same for the top 100 (1000 when ranking by Youden's J, AUC or
#'     binarised r)}
#'   \item{item_r}{List of item-target correlations, one named vector per leaderboard row}
#'   \item{item_stats}{Data frame for the selected items as scored (reverse-keyed items reversed,
#'     standardised if \code{scale.vars}): \code{key}, \code{mean}, \code{sd}, \code{r_target},
#'     \code{r_rest} (with the sum of the other selected items) and \code{alpha_if_deleted}}
#'   \item{scores}{(If \code{generate = TRUE}) the selected set's sum scores (and, for binary
#'     targets, whether each meets the cutoff), for every row}
#'   \item{target}{The target used, for every row (the full scale's mean when no target is given)}
#'   \item{pool}{Item names: \code{original} (every column), \code{valid} (usable items) and
#'     \code{searched} (the pool searched, after any prefilter)}
#'   \item{pivoting_item}{With no target: the most central item, which sets the direction of
#'     reverse keying; NA otherwise}
#'   \item{n}{Rows: \code{total}, \code{train} and \code{holdout}}
#'   \item{search}{\code{type} ("exhaustive", the guaranteed best set, or "optimised",
#'     heuristic), \code{combinations} (possible sets), \code{searched} (sets scored by
#'     exhaustive searches in all) and, when optimised, \code{searches} (one row per search:
#'     its phase, its budget, the item left out or put back, open items, how many of them are chosen, kept
#'     items, combinations, the r of its centre, the best r it had found, and its seconds) and
#'     \code{loco} (one row per LOCO candidate: the best r found with it left out or put back,
#'     and how many searches that took)}
#'   \item{binary}{(Binary targets) \code{cutoff}, \code{metric}, \code{cross_validated}, and
#'     the metrics as \code{results}, or \code{train} and \code{holdout}}
#'   \item{settings}{Named list of the arguments used (and the resolved \code{ranking_metric})}
#'   \item{call, version, seconds}{The call, the package version (\code{"sourced"} when loaded
#'     with \code{source()}), and the run's total seconds}
#'   \item{timings}{\code{steps} (seconds per step of the run) and \code{engine} (the C++
#'     engine's own sections, a breakdown of the \code{search} step)}
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
#' \dontshow{RcppParallel::setThreadOptions(numThreads = 2)}
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
#' \dontshow{RcppParallel::setThreadOptions(numThreads = "auto")}

reduceTo <- function(data, n.items, target = NULL, method = NULL, cross.validate = 0,
                     n.sets = 5, item.names = FALSE, r.sq = FALSE, generate = TRUE, item.set = 1,
                     na.rm = TRUE, scale.vars = FALSE,
                     speed = c("fast", "conservative"), ceiling = 1e12, optimise = NULL, knockout.budget = NULL, prefilter.ratio = Inf,
                     verbose = TRUE, show.progress = TRUE, seed = 1){

  # Wall-clock checkpoints: each step's time is the gap since the previous one,
  # so everything from here to the finished output counts (see timings_r at the
  # end). Appended rather than keyed by label: a label marked twice used to
  # overwrite its first time in place, giving the steps around it wrong times
  ck_time <- Sys.time()
  ck_label <- "start"
  the_call <- match.call()
  mark_time <- function(label) {
    ck_time <<- c(ck_time, Sys.time())
    ck_label <<- c(ck_label, label)
  }

  speed <- match.arg(speed)

  # The row-scan engine re-reads every row for every combination, so it
  # keeps a much tighter ceiling unless it's set explicitly
  if (speed == "conservative") {
    if (missing(ceiling)) ceiling <- 1e7
  }

  # optimise: NULL = a knockout search only above ceiling, TRUE = always, FALSE = never
  if (!is.null(optimise) && !(is.logical(optimise) && length(optimise) == 1 && !is.na(optimise)))
    stop("'optimise' must be NULL, TRUE or FALSE.")
  if (!is.null(knockout.budget) && !(is.numeric(knockout.budget) && length(knockout.budget) == 1 && !is.na(knockout.budget) && knockout.budget >= 1))
    stop("'knockout.budget' must be NULL or a single number >= 1.")

  # The most combinations each of the knockout search's searches may score
  knockout_budget <- if (is.null(knockout.budget)) max(1, ceiling / 1000) else knockout.budget

  # Whole-number arguments: n.items = 0 used to return an empty result
  # silently, and non-integers were quietly rounded deep inside choose()
  if (!(is.numeric(ceiling) && length(ceiling) == 1 && !is.na(ceiling)))
    stop("'ceiling' must be a single number.")
  if (!is.null(seed) && !(is.numeric(seed) && length(seed) == 1 && is.finite(seed)))
    stop("'seed' must be NULL or a single number.")
  if (!((is.numeric(cross.validate) || is.logical(cross.validate)) && length(cross.validate) == 1 && !is.na(cross.validate)))
    stop("'cross.validate' must be TRUE, FALSE, or a single number between 0 and 1.")

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

  # Preserve the caller's random state (reduceTo() seeds its own sampling internally, unless seed = NULL)
  if (is.null(seed)) {
    # draws come from the session's random state, which they advance as any random function does
  } else if (exists(".Random.seed", envir = globalenv())) {
    old_seed <- get(".Random.seed", envir = globalenv())
    on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
  } else {
    # Fresh session with no seed yet: remove the one our own set.seed()
    # calls leave behind, or every random draw the caller makes afterwards
    # would silently continue from that stream (identical results
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
  
  # Reverse-score the chosen items (by name, position or logical mask). Returns a copy: nothing is changed in place
  flip_items <- function(x, the_items, ref_rows = seq_len(nrow(x))) {

    should_flip <- if(is.logical(the_items)) any(the_items, na.rm = TRUE) else length(the_items) > 0

    if (should_flip) {
      idx <- if (is.logical(the_items)) which(the_items) else if (is.character(the_items)) match(the_items, colnames(x)) else the_items
      idx <- as.integer(idx)
      if (scale.vars) {
        bounds <- numeric(length(idx))                   # -x
      } else {
        # (min + max) - x, per column -- conventional reverse-keying, so a 1-5
        # item stays on 1-5 (plain max - x shifted it to 0-4, leaving returned
        # sum scores and binary cutoffs offset by one point per reversed item
        # from how users score the scale themselves). Uses training-set
        # bounds (ref_rows) so holdout data flips consistently
        rows <- if (length(ref_rows) >= nrow(x)) integer(0) else as.integer(ref_rows)
        sc <- column_scan_cpp(x, rows, idx)
        bounds <- sc$min + sc$max
      }
      x <- flip_columns_cpp(x, idx, bounds)
    }
    return(x)
  }

  # Pearson correlation matrix of the columns of x (over `rows`, default all) from mean-imputed data, via
  # the fused Gram (C++ tiles + BLAS, no full-size copies) -- fast enough to use ALL rows (cor(use = "p")
  # handles missingness per pair, which makes a full p x p matrix slow at large N, hence the old 2000-row
  # subsample -- small enough to orient weak items the wrong way)
  imputed_cor <- function(x, rows = integer(0)) {
    sc <- column_scan_cpp(x, rows)
    g <- fused_gram_cpp(x, numeric(0), sc$sum / sc$n, rows)   # centred on the column means (see prep.cpp)
    cv <- g$gram - outer(g$col_sums, g$col_sums) / g$n_valid
    sds <- sqrt(diag(cv))
    cv / outer(sds, sds)
  }

  # Mean-impute missing values, then precompute the column moments (Gram
  # matrix, sums, target dot products) needed to score any item combination.
  # One C++ pass: missing cells are filled tile by tile on the way into BLAS
  build_gram_components <- function(data, targ) {
    sc <- column_scan_cpp(data)
    fused_gram_cpp(data, as.numeric(targ), sc$sum / sc$n)
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

  # The knockout search (knockout.cpp), for searches too large to run
  # exhaustively: trim the scale from the full pool, search every combination
  # of the closest calls around the best form found (repeating from any
  # better form), then do the same with each of its items knocked out in turn (LOCO)
  # (or, for long forms, each left-out item put back). Its best sets then get
  # the same exact re-scoring as an exhaustive search's. Under speed =
  # "conservative", every combination of the closest calls around the best
  # form is also scored directly with pairwise deletion first
  knockout_and_rescore <- function(data, targ, original_indices, na.rm, is_binary, n.items,
                                 ranking_metric, optimize_for, show.progress, keep_top, speed, budget) {
    prog_width <- max(getOption("width", 80L) - 1L, 20L)
    p <- ncol(data)

    # Moments from every row (missing values mean-imputed, for the search only)
    mom <- gram_moments_cpp(as.matrix(data), as.numeric(targ))
    mark_time("gram_matrix")

    # Knockout search: each of its searches tests up to `budget` combinations (ceiling / 1000; about 28
    # searches, most of them in parallel, on the benchmark battery)
    res <- knockout_cpp(mom$C, mom$tt, mom$ssy, n.items, max(1, budget),
                        keep_top = keep_top, show_progress = show.progress, width = prog_width)
    mark_time("search")
    secs_cpp <- res$secs
    combos <- apply(res$sets, 1, function(s) paste(original_indices[s], collapse = ","))
    searched <- res$searched

    if (identical(speed, "conservative") && res$pool_choose > 0 && res$pool_choose < length(res$pool_open)) {
      open <- res$pool_open; kept <- res$pool_kept
      direct <- process_all_combinations_cpp_parallel_float(
        data = compress_for_cpp(data[, c(open, kept), drop = FALSE]),
        n_items = res$pool_choose,
        num_choose_from = length(open),
        na_rm = na.rm,
        target = targ,
        original_indices = original_indices[open],
        keep_top = keep_top,
        show_progress = show.progress,
        n_fixed = length(kept)
      )
      combos <- c(combos, if (length(kept)) paste(paste(original_indices[kept], collapse = ","), direct$combination, sep = ",") else direct$combination)
      searched <- searched + choose(length(open), res$pool_choose)
      secs_cpp <- c(secs_cpp, conservative_pass = sum(direct$timings_cpp))
      mark_time("conservative_pass")
    }

    if (show.progress) {
      # searches at the budget, and those of the sweepdown that weren't read off larger ones' results
      at_top <- res$rounds$budget == max(res$rounds$budget)
      msg <- sprintf("~{ knockout }~ done: %d searches (+%d in the sweepdown), %s combinations, %.1fs", sum(at_top),
                     sum(!at_top & res$rounds$combinations > 0), format(searched, big.mark = ",", scientific = FALSE), sum(res$secs))
      # padded to overwrite the progress line, but not cut: it ends the line, so wrapping is harmless
      cat("\r", formatC(msg, width = -prog_width), "\n", sep = "")
    }

    canonical <- vapply(strsplit(combos, ","), function(z) paste(sort(as.numeric(z)), collapse = ","), character(1))
    leaderboard <- data.frame(combination = combos[!duplicated(canonical)], r = NA_real_, stringsAsFactors = FALSE)
    leaderboard <- rescore_leaderboard(leaderboard, data, targ, original_indices, n.items, na.rm,
                                       is_binary, ranking_metric, optimize_for, show.progress)

    # Item numbers to names, for the search record
    rounds <- res$rounds; rounds$changed <- colnames(data)[rounds$changed]
    items <- res$items; items$item <- colnames(data)[items$item]

    list(leaderboard = leaderboard, timings_cpp = secs_cpp, searched = searched,
         rounds = rounds, items = items)
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
                                              speed) {

    used_fast_path <- identical(speed, "fast")

    if (used_fast_path) {
      # Score via the Gram-matrix shortcut (missing values mean-imputed for search only)
      gc_components <- build_gram_components(data, targ)
      mark_time("gram_matrix")

      # When keeping more than half of the items, enumerate the few to DROP
      # instead: same combinations and exact same scores, but the engine's
      # cost per combination grows steeply with k (28 of 40 ran ~20x slower
      # than the equally-sized 12 of 40)
      n_drop <- num_choose_from - n.items
      drop_space <- n_drop >= 1 && n_drop < n.items
      search <- if (drop_space) {
        fold_base_into_moments(gc_components, seq_len(num_choose_from), seq_len(num_choose_from), n_drop, sign = -1)
      } else gc_components

      cpp_result <- process_all_combinations_cpp_gram(
        gram = search$gram,
        col_sums = search$col_sums,
        col_target_dots = search$col_target_dots,
        sum_target = gc_components$sum_target,
        sum_target_sq = gc_components$sum_target_sq,
        n_valid = gc_components$n_valid,
        n_items = if (drop_space) n_drop else n.items,
        num_choose_from = num_choose_from,
        original_indices = original_indices,
        keep_top = keep_top,
        show_progress = show.progress
      )
      # Report every combination as its full item set
      if (drop_space) {
        cpp_result$combination <- vapply(strsplit(cpp_result$combination, ","), function(d)
          paste(setdiff(original_indices, as.numeric(d)), collapse = ","), character(1))
      }
      mark_time("search")
    } else {
      compressed_data <- compress_for_cpp(data)
      mark_time("compression")

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
      mark_time("search")
    }

    leaderboard <- data.frame(
      combination = cpp_result$combination,
      r = cpp_result$r,
      stringsAsFactors = FALSE
    )
    leaderboard <- rescore_leaderboard(leaderboard, data, targ, original_indices, n.items, na.rm,
                                       is_binary, ranking_metric, optimize_for, show.progress)

    list(leaderboard = leaderboard,
         timings_cpp = cpp_result$timings_cpp,
         used_fast_path = used_fast_path)
  }

  # Recompute exact statistics for the leaderboard's combinations from the
  # true data (the search engines work from approximated data), in parallel
  # C++: each combination's sum scores are built straight from the data, then
  # its pairwise-complete r -- and for binary targets its cutoff search and
  # AUC. This used to build N x keep_top score matrices (~7 GB at N = 300,000
  # with a binary target) and loop the cutoff search in R on one core (21 of
  # 27 seconds of that run)
  rescore_leaderboard <- function(leaderboard, data, targ, original_indices, n.items, na.rm,
                                  is_binary, ranking_metric, optimize_for, show.progress) {
    top_combos <- strsplit(leaderboard$combination, ',')
    sets <- matrix(unlist(lapply(top_combos, function(cids) match(as.numeric(cids), original_indices))) - 1L,
                   nrow = n.items)
    storage.mode(sets) <- "integer"

    if (is_binary && show.progress) {
      bc_width <- max(getOption("width", 80L) - 1L, 20L)
      cat("\r", formatC("~{  binaryCutoff  }~ searching", width = -bc_width), sep = "")
      flush.console()
    }
    # Standardised (scale.vars) sum scores aren't whole numbers: their cutoff can be any value
    rescored <- rescore_sets_cpp(as.matrix(data), as.numeric(targ), sets, na.rm, is_binary,
                                 identical(optimize_for, "binarised_r"), integer_cutoffs = !scale.vars)
    mark_time("rescoring")

    leaderboard$r <- rescored$r

    if (is_binary) {
      leaderboard$`>=` <- rescored$cutoff
      leaderboard$binarised_r <- rescored$binarised_r
      leaderboard$youden_j <- rescored$youden_j
      leaderboard$auc <- rescored$auc

      # Re-rank by the chosen metric
      leaderboard <- leaderboard[order(abs(leaderboard[[ranking_metric]]),
                                       decreasing = TRUE), ]
      mark_time("reranking")
    } else {
      # Re-rank by the exact r just recomputed. Both engines search on an
      # approximation (mean-imputed / 8-bit data), so their order within
      # the top keep_top can disagree with the r actually reported: under
      # 15% missingness, row 1 was not the best reported set in half of
      # the runs tested. Re-ranking recovered the exact optimum in every
      # stress test run (80/80, up to 50% missing, incl. MNAR patterns)
      leaderboard <- leaderboard[order(abs(leaderboard$r), decreasing = TRUE), ]
    }
    leaderboard
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

  # Correlation of each individual item with the target (in its original direction), for every leaderboard row
  calculate_item_correlations <- function(data, targ, items_to_flip, cols_names, parsed_combos) {

    if (nrow(data) > 10000) { if (!is.null(seed)) set.seed(seed); it.cor.sub <- sample(1:nrow(data), 10000) } else it.cor.sub <- 1:nrow(data)

    matched_comb_list <- parsed_combos$matched_comb_list

    unflip_indices <- intersect(cols_names, items_to_flip)

    flip_items_simple <- function(x, indices){
      x[,indices] <- x[,indices]*(-1)
      return(x)
    }

    dc2 <- as.vector(cor(flip_items_simple(data[it.cor.sub, , drop = FALSE], unflip_indices), targ[it.cor.sub], 'p') )
    names(dc2) <- colnames(data)

    lapply(matched_comb_list, function(x) dc2[x])
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
    if (seconds >= 1000 * 31557600) return("more than 1,000 years")

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
  format_count <- function(x) {
    if (x < 1e15) return(format(x, big.mark = ",", scientific = FALSE))
    e <- floor(log10(x))
    sprintf("about %.1f x 10^%d", x / 10^e, e)
  }

  format_duration_range <- function(low_seconds, high_seconds) {
    # format_duration() floors to whole units, so any range under a second
    # would otherwise print as the meaningless "0 to 0 seconds"
    if (high_seconds < 1) return("< 1 second")
    if (low_seconds >= 1000 * 31557600) return("more than 1,000 years")
    if (low_seconds < 1) return(paste("<", format_duration(ceiling(high_seconds))))

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
  
  mark_time("arguments")

  # Convert tibbles to standard data frames
  if (inherits(data, "tbl_df") || inherits(data, "tbl")) {
    data <- as.data.frame(data)
  }
  if (!is.matrix(data) && !is.data.frame(data)) stop("'data' must be a matrix or data frame.")

  # Row names (e.g. participant IDs) are kept for $scores and $target at the end, but not carried through
  # the computation: with named rows every column extraction copies thousands of names (a 35 ms step at
  # N = 5,000, growing with N x items)
  orig_row_names <- if (is.data.frame(data)) { if (.row_names_info(data) > 0L) rownames(data) else NULL } else rownames(data)
  if (!is.null(orig_row_names)) rownames(data) <- NULL
  
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
  
  # A quoted column name works as an unquoted one (and so does a variable holding one)
  if (is.null(target_col_name) && is.character(target) && length(target) == 1 && !is.na(target) && nrow(data) != 1) {
    if (!(target %in% colnames(data))) stop("'target' (\"", target, "\") is not a column of 'data'.")
    target_col_name <- target
    target <- data[, target_col_name]
    data[, target_col_name] <- NA
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
    if (any(is.infinite(target))) stop("'target' contains infinite values.")
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

  mark_time("target")
  
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
    if (cv_subset_size < 3 || nrow(data) - cv_subset_size < 3) {
      stop("cross.validate = ", cross.validate, " leaves ", cv_subset_size, " training and ", nrow(data) - cv_subset_size,
           " holdout rows: each needs at least 3.")
    }
    if (!is.null(seed)) set.seed(seed)
    cv_subset <- sample(1:nrow(data), cv_subset_size)
    cv_holdout <- setdiff(1:nrow(data), cv_subset)
    train_rows <- cv_subset
    }

  mark_time("cv_split")

  # Identify valid numeric columns (and, when cross-validating, columns that
  # also vary within the training rows -- a column constant there would
  # enter every search stage as a zero-variance "free" item). One C++ pass over
  # the numeric columns gives each one's count, min, max and sum: a column is
  # usable when it has values, none infinite, and max > min (the same as sd > 0)
  is_num <- if (is.matrix(data)) rep(is.numeric(data), ncol(data)) else vapply(data, is.numeric, logical(1))
  valid_mask <- rep(FALSE, ncol(data))
  num_data <- NULL
  if (any(is_num)) {
    num_data <- if (is.matrix(data)) data else as.matrix(data[, is_num, drop = FALSE])
    if (!is.double(num_data)) storage.mode(num_data) <- "double"
    scan_all <- column_scan_cpp(num_data)
    col_ok <- scan_all$n > 0 & scan_all$has_inf == 0 & scan_all$max > scan_all$min
    if (cross.validate) {
      scan_train <- column_scan_cpp(num_data, as.integer(train_rows))
      col_ok <- col_ok & scan_train$n > 0 & scan_train$max > scan_train$min
    }
    valid_mask[is_num] <- col_ok
  }

  excluded <- setdiff(original_names[!valid_mask], target_col_name)
  if (length(excluded) > 0 && verbose) {
    message(paste0("=~ Note: excluding ", length(excluded), " column(s) that are non-numeric, constant, all-NA or contain infinite values: ",
                   paste(head(excluded, 10), collapse = ", "), if (length(excluded) > 10) ", ..." else ""))
  }

  if (sum(valid_mask) < n.items) {
    stop("Error: Input data contains fewer valid numeric columns than 'n.items'.")
  }

  mark_time("item_checks")

  # Filter to valid columns (a numeric matrix from here on)
  data <- if (all(valid_mask)) num_data else num_data[, col_ok, drop = FALSE]
  cols <- 1:ncol(data)
  original_indices <- all_original_indices[valid_mask]
  filtered_names <- colnames(data)  # Save valid column names

  mark_time("item_filter")

  # Convert to matrix for computational efficiency
  if (!is.matrix(data)) data <- as.matrix(data)

  # flip items negatively correlated with most central item

  if (!target_supplied) {
    dc <- imputed_cor(data, if (length(train_rows) < nrow(data)) as.integer(train_rows) else integer(0))
    centrality <- colSums(abs(dc), na.rm = TRUE)
    most_central_item <- order(centrality, decreasing = TRUE)[1]

    pivot_item <- colnames(data)[most_central_item]

    # Reverse-key items correlating negatively with it -- by more than rounding: an item uncorrelated with
    # it (to within 1e-12) stays as it is, rather than flipping on the sign of rounding noise, which can
    # differ between computers (and with it, the best set)
    items_to_flip_TF <- dc[, most_central_item] < -1e-12
    items_to_flip_TF[is.na(items_to_flip_TF)] <- FALSE
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

    items_to_flip_TF <- dc < -1e-12                 # (as above: not on the sign of rounding noise)
    items_to_flip <- filtered_names[items_to_flip_TF]

    data <- flip_items(data, items_to_flip, train_rows)
    pivot_item <- NA

    # Used later to prefilter weak items before optimisation
    relevance <- abs(dc)
  }

  # Apply standardization if requested -- here, before any search stage (not
  # just the final one), using training-row means/SDs for every row. Scaling
  # only the final pool left earlier search stages ranking items on the raw scale,
  # and holdout rows (and their $scores) on a different scale from training
  if (scale.vars) {
    scale_ref <- data[train_rows, , drop = FALSE]
    data <- sweep(data, 2, colMeans(scale_ref, na.rm = TRUE), "-")
    data <- sweep(data, 2, apply(scale_ref, 2, sd, na.rm = TRUE), "/")
  }

  #create target
  if (!target_supplied) target <- rowMeans(data, na.rm = na.rm)
  if (sum(!is.na(target[train_rows])) < 3) {
    stop("Fewer than 3 rows have a usable target",
         if (!target_supplied && !na.rm) " (with na.rm = FALSE, a row needs every item present)",
         ". Try na.rm = TRUE, or check the data's missing values.")
  }
  target_all <- target  # full-length (training + holdout), returned in $target

  if (cross.validate) {
    data_holdout <- data[cv_holdout, , drop = FALSE]
    target_holdout <- target[cv_holdout]
    data <- data[cv_subset, , drop = FALSE]
    target <- target[cv_subset]
  }

  mark_time("reverse_keying")
  
  # Runtime estimates printed when verbose: when optimisation is triggered, and when optimise = FALSE
  # forces a search past ceiling. The Gram engine skips whatever can't reach the leaderboard, and how
  # much it skips depends on the data and the size of the search, so its speed can't be read off a
  # small stand-in problem: it is timed on the real search, or on one shaped like the knockout's.
  # 0.02 s each (0.25 s was most of a knockout run): on IPIP problems the estimates stayed within
  # 0.6-1.5x of the actual times for 10 knockout searches and mostly 0.5-2x for 8 exhaustive ones
  PROBE_SECONDS <- 0.02
  # Every item's moments (mean-imputed), worked out once for both probes
  probe_mom <- NULL
  probe_moments <- function(cols) {
    if (is.null(probe_mom)) probe_mom <<- gram_moments_cpp(as.matrix(data), as.numeric(target))
    list(gram = probe_mom$C[cols, cols, drop = FALSE], col_sums = rep(0, length(cols)), col_target_dots = probe_mom$tt[cols],
         sum_target = 0, sum_target_sq = probe_mom$ssy, n_valid = probe_mom$n_valid)
  }

  # Seconds to score every combination of the pool `pool_cols`: the real search (on the side it would
  # enumerate) times its strongest branches and a sample of the rest (see combo_scores.cpp). The sample
  # still runs under a lower threshold than most of the search would, so it errs long, by about 2x
  # (0.3-0.9x on 8 IPIP problems of 1-50 seconds): halved
  probe_exhaustive_seconds <- function(pool_cols) {
    p <- length(pool_cols)
    gcomp <- probe_moments(pool_cols)
    n_drop <- p - n.items
    drop_space <- n_drop >= 1 && n_drop < n.items
    search <- if (drop_space) fold_base_into_moments(gcomp, seq_len(p), seq_len(p), n_drop, sign = -1) else gcomp
    k_side <- if (drop_space) n_drop else n.items
    t0 <- Sys.time()
    res <- process_all_combinations_cpp_gram(
      gram = search$gram, col_sums = search$col_sums, col_target_dots = search$col_target_dots,
      sum_target = gcomp$sum_target, sum_target_sq = gcomp$sum_target_sq, n_valid = gcomp$n_valid,
      n_items = k_side, num_choose_from = p, original_indices = seq_len(p),
      keep_top = leaderboard_length, show_progress = FALSE, max_seconds = PROBE_SECONDS)
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    if (is.na(res$est_seconds)) elapsed * choose(p, k_side) / max(res$covered, 1) else 0.5 * res$est_seconds
  }

  # Combinations/sec of the knockout's searches. Each chooses about kk of nU closest calls (at most 40),
  # nU as large as fits ceiling / 1000, so time the engine on the pool's most relevant items --
  # close calls, like the real ones -- choosing kk of up to nU (at most 2 x 10^8 combinations' worth)
  probe_knockout_rate <- function(pool_cols) {
    p <- length(pool_cols)
    kk_of <- function(n) min(n.items, p - n.items, floor(n / 2))
    nU <- 2
    while (nU < min(40, p) && choose(nU + 1, kk_of(nU + 1)) <= knockout_budget) nU <- nU + 1
    kk <- max(1, kk_of(nU))
    n2 <- nU
    while (n2 > kk + 1 && choose(n2, kk) > 2e8) n2 <- n2 - 1
    top <- pool_cols[order(relevance[pool_cols], decreasing = TRUE)[seq_len(n2)]]
    g <- probe_moments(top)
    t0 <- Sys.time()
    res <- process_all_combinations_cpp_gram(
      gram = g$gram, col_sums = g$col_sums, col_target_dots = g$col_target_dots,
      sum_target = g$sum_target, sum_target_sq = g$sum_target_sq, n_valid = g$n_valid,
      n_items = kk, num_choose_from = n2, original_indices = seq_len(n2),
      keep_top = leaderboard_length, show_progress = FALSE, max_seconds = PROBE_SECONDS,
      screen_ratio = 8)                           # as the knockout's own searches (SCREEN_RATIO_POOLS)
    secs <- if (is.na(res$est_seconds)) as.numeric(difftime(Sys.time(), t0, units = "secs")) else res$est_seconds
    choose(n2, kk) / max(secs, 1e-6)
  }

  # Combinations/sec of the row-scan engine (speed = "conservative"), which scores every combination
  # at a steady rate, so a small stand-in problem does tell. A single small timed call is heavily biased
  # toward one-time fixed costs (RcppParallel thread-pool startup, R<->C++ call overhead) that
  # never recur once real work is running, making the estimate look far
  # slower than reality. Instead: run a small first-look calibration
  # (after an untimed warm-up, so thread-pool startup isn't counted at
  # all), then use ITS OWN measured rate to size a second calibration
  # aimed at ~TARGET_CALIB_TIME of real work, which is enough to
  # amortise the fixed costs away. Both phases size themselves via
  # choose(), so this stays cheap and safe regardless of engine speed --
  # never based on a fixed pool-size margin that could blow up for large
  # n.items.
  measure_rowscan_rate <- function(k = n.items) {
    grow_pool_to_combos <- function(target_combos, start_p, max_p) {
      p <- start_p
      while (p < max_p && choose(p, k) < target_combos) p <- p + 1
      p
    }

    TINY_CALIB_COMBOS <- 2000000
    TARGET_CALIB_TIME <- 0.1  # seconds

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

    p0 <- grow_pool_to_combos(TINY_CALIB_COMBOS, k, ncol(data))
    run_calib_at(p0)  # untimed warm-up
    tiny <- run_calib_at(p0)
    tiny_rate <- tiny$combos / max(tiny$elapsed, 1e-6)

    p1 <- grow_pool_to_combos(tiny_rate * TARGET_CALIB_TIME, p0, ncol(data))
    if (p1 > p0) {
      r <- run_calib_at(p1)
      r$combos / max(r$elapsed, 1e-6)
    } else {
      tiny_rate
    }
  }

  # Check if optimization is needed: above ceiling (unless optimise = FALSE), or always with optimise =
  # TRUE (unless there is only one combination)
  num_combinations <- choose(ncol(data), n.items)
  forced <- isTRUE(optimise) && n.items < ncol(data)
  knockout <- FALSE

  if (num_combinations > ceiling || forced) {

    if (!isFALSE(optimise)) {
      knockout <- TRUE

      # Drop items far weaker than the strongest before the knockout search (and
      # before estimating its runtime): prefiltering alone often brings the pool
      # comfortably under ceiling, and then no knockout search is needed at all
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

      knockout <- forced || choose(length(cols), n.items) > ceiling

      # Everything down to the message below only feeds the runtime estimate
      # printed when verbose -- skip it entirely when silent (the timing probes
      # cost ~20% of a typical run with verbose = FALSE, for nothing)
      if (verbose && !knockout) {
        message(paste0("=~ This would generate ", format_count(num_combinations),
                       " combinations to compare; after the prefilter, the remaining ",
                       format(choose(length(cols), n.items), big.mark = ",", scientific = FALSE),
                       " are searched exhaustively."))
        if (!is.null(prefilter_message)) message(prefilter_message)
      }
      if (verbose && knockout) {

        # Every combination of the whole pool (before any prefilter), as the message reports it
        est_seconds <- if (identical(speed, "fast")) probe_exhaustive_seconds(seq_len(ncol(data))) else num_combinations / measure_rowscan_rate()

        # Knockout search: a few searches to converge, then the same search again with each item of the
        # best form knocked out (or, for a long form leaving out 20 or more, each left-out item put back),
        # each repeating 1-3 times (2.5 allows for large pools, where repeats are more common) -- every
        # search testing up to ceiling / 1000 combinations (about 3/4 of that on average), at about
        # 1.4 times the rate of the stand-in pool (the real ones are smaller, with items fixed, nested in
        # parallel; measured within 0.6-1.5x on 10 IPIP problems of 60-300 items) -- plus about a tenth for the
        # sweepdown, and, under speed = "conservative", one direct search of up to that many combinations
        n_pool <- length(cols)
        n_jobs <- if (n.items > n_pool / 2 && n_pool - n.items >= 20) n_pool - n.items else n.items
        n_searches <- round(4 + 2.5 * n_jobs)
        opt_mid <- 1.1 * n_searches * 0.75 * knockout_budget / (1.4 * probe_knockout_rate(cols))
        opt_low <- opt_mid / 2
        opt_high <- opt_mid * 2
        if (identical(speed, "conservative")) {
          direct_seconds <- knockout_budget / (num_combinations / est_seconds)
          opt_low <- opt_low + direct_seconds / 2
          opt_high <- opt_high + direct_seconds
        }

        message(paste0("=~ This would generate ",
                       format_count(num_combinations),
                       " combinations to compare (",format_duration_range(est_seconds/2, est_seconds*2),
                       " with N = ",format(nrow(data), big.mark = ",",scientific = FALSE) ,
                       "). \n=~ ", if (num_combinations > ceiling) "A knockout search" else "With optimise = TRUE, a knockout search",
                       " will be used instead: about ", n_searches,
                       " searches of up to ", format(knockout_budget, big.mark = ",", scientific = FALSE),
                       " combinations each (", format_duration_range(opt_low, opt_high), " in all).",
                       if (num_combinations > ceiling) " You can change this with the 'ceiling' argument." else ""))
        if (!is.null(prefilter_message)) message(prefilter_message)
      }

    } else {
      if (verbose) {
        est_seconds <- if (identical(speed, "fast")) probe_exhaustive_seconds(seq_len(ncol(data))) else num_combinations / measure_rowscan_rate()
        message(sprintf("=~ Note: %s combinations exceeds ceiling (%s) but optimise = FALSE. Running exhaustive search: expected completion time is %s.",
                        format_count(num_combinations),
                        format(ceiling, big.mark = ",", scientific = FALSE),
                        format_duration_range(est_seconds/2, est_seconds*2)))
      }
    }
  }
  
  cols_names <- filtered_names[cols]
  prefiltered <- length(cols) < ncol(data)
  
  mark_time("search_plan")

  # Subset to the columns still in play (all valid columns, unless prefiltered)
  parent_data <- data  # every valid item, keyed as searched (for r_parent)
  data <- data[, cols, drop = FALSE]
  if (cross.validate) { data_holdout <- data_holdout[, cols, drop = FALSE] }
  
  mark_time("pool_subset")

  # (Standardization, if requested, was already applied before optimisation)
  
  # Warn if items use very different scales (e.g. mixing 0-1 and 1-7 items). Only worked out when it
  # can be shown: the ranges come from the same one-pass scan as the item checks
  if (!scale.vars && verbose) {
    sc_pool <- column_scan_cpp(data)
    col_ranges <- sc_pool$max - sc_pool$min

    if (max(col_ranges) / min(col_ranges) > 2) {
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
  
  mark_time("range_check")
  
  # Knockout search (too many combinations), or score every combination;
  # either way the leaderboard is re-scored exactly (always uses indices internally)
  cpp_results <- if (knockout) {
    knockout_and_rescore(data, target, original_indices, na.rm, is_binary, n.items, ranking_metric,
                       optimize_for, show.progress, leaderboard_length, speed, knockout_budget)
  } else {
    process_combinations_in_batches(data, target, num_choose_from,
                                    original_indices, na.rm,
                                    is_binary, n.items, ranking_metric,
                                    optimize_for, show.progress, leaderboard_length,
                                    speed)
  }
  searched_combinations <- if (knockout) cpp_results$searched else choose(num_choose_from, n.items)

  leaderboard <- cpp_results$leaderboard
  timings_cpp <- cpp_results$timings_cpp                                                                                                                   
  
  # Clean up leaderboard
  rownames(leaderboard) <- NULL

  if (item.set > nrow(leaderboard)) {
    stop("'item.set' (", item.set, ") is larger than the number of ranked item sets (", nrow(leaderboard), ").")
  }
  
  # Parse leaderboard combinations once, shared by both steps below
  parsed_combos <- parse_leaderboard_combinations(leaderboard, original_indices)

  # Extract best items
  best_items <- extract_best_items(data, parsed_combos)

  mark_time("best_items")

  # Calculate item-level correlations
  ind_cors <- calculate_item_correlations(data, target, items_to_flip, cols_names, parsed_combos)
  
  # Keys straight from the orientation actually used in scoring, not
  # re-estimated from (rounded) item correlations on a separate subsample,
  # which could disagree for weak items and describe a different scale
  ind_keys <- ifelse(best_items$best_names[[item.set]] %in% items_to_flip, -1, 1)
  
  mark_time("item_correlations")

  # The selected set as scored (reverse-keyed items reversed, standardised if scale.vars), on the rows the
  # search used (the training rows when cross-validating): its alpha, each item's statistics, and the
  # correlation of its sum score with the full scale's (every valid item)
  sel_names <- best_items$best_names[[item.set]]
  sel_data <- data[, sel_names, drop = FALSE]
  alpha_of <- function(S) {
    m <- ncol(S)
    if (m < 2 || anyNA(S)) return(NA_real_)
    m / (m - 1) * (1 - sum(diag(S)) / sum(S))
  }
  # Each item's correlation with the mean of the others (as rowMeans(x[, -i], na.rm) would give it), each
  # item taken out of the rows' totals rather than the rest re-summed per item: linear in the set's length,
  # where re-summing was quadratic (~1 s of a 290-of-300 run)
  rest_correlations <- function(x, na.rm) {
    m <- ncol(x)
    if (m < 2) return(rep(NA_real_, m))
    present <- !is.na(x)
    tot <- rowSums(x, na.rm = TRUE); cnt <- rowSums(present)
    vapply(seq_len(m), function(i) {
      xi <- x[, i]; has <- present[, i]
      n_rest <- cnt - has
      rest <- (tot - ifelse(has, xi, 0)) / n_rest
      rest[if (na.rm) n_rest == 0 else n_rest < m - 1] <- NA     # na.rm = FALSE: any other item missing
      suppressWarnings(stats::cor(xi, rest, use = "pairwise.complete.obs"))
    }, numeric(1))
  }
  sel_cov <- stats::cov(sel_data, use = "pairwise.complete.obs")
  sel_alpha <- alpha_of(sel_cov)
  r_parent <- suppressWarnings(stats::cor(rowMeans(sel_data, na.rm = na.rm), rowMeans(parent_data, na.rm = na.rm),
                                          use = "pairwise.complete.obs"))
  item_stats <- data.frame(
    item = sel_names,
    key = ind_keys,
    mean = unname(colMeans(sel_data, na.rm = TRUE)),
    sd = unname(apply(sel_data, 2, stats::sd, na.rm = TRUE)),
    r_target = as.vector(suppressWarnings(stats::cor(sel_data, target, use = "pairwise.complete.obs"))),
    r_rest = rest_correlations(sel_data, na.rm),
    alpha_if_deleted = vapply(seq_along(sel_names), function(i) alpha_of(sel_cov[-i, -i, drop = FALSE]), numeric(1)),
    row.names = NULL
  )

  mark_time("selected_stats")
  
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
        leaderboard$sum_scored_R2_holdout <- leaderboard$r_holdout^2
        leaderboard$binarised_R2_holdout <- leaderboard$binarised_r_holdout^2
      }
    }
  }
  
  
  mark_time("cross_validation")
  
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
  
  mark_time("scores")

  # Convert combination column to names if requested
  if (item.names) {
    leaderboard$combination <- best_items$best_names
  }
  
  # ============================================================================
  # CONSTRUCT RESULTS OBJECT
  # ============================================================================
  
  # Helper to safely extract a metric from the leaderboard for the specific item set
  get_metric <- function(col_name) {
    if (col_name %in% colnames(leaderboard)) {
      return(leaderboard[[col_name]][item.set])
    }
    return(NA_real_) # NA if the column doesn't exist (e.g. binary metrics for a continuous target)
  }
  
  
  # Input row names go back onto the sum scores, and onto a target that was built from (or taken out of) the data
  scores_out <- if (!is.null(computed_scores)) as.matrix(computed_scores)[, , drop = F] else NULL
  if (!is.null(scores_out) && !is.null(orig_row_names)) rownames(scores_out) <- orig_row_names
  target_out <- target_all
  if (!is.null(orig_row_names) && (!target_supplied || !is.null(target_col_name))) names(target_out) <- orig_row_names

  # Metrics of the selected set first (NA where they don't apply), then the set itself, the leaderboard,
  # the pool and search, and the run's settings and record
  results_object <- list(
    r = get_metric("r"),
    binarised_r = get_metric("binarised_r"),
    youden_j = get_metric("youden_j"),
    auc = get_metric("auc"),
    cutoff = get_metric(">="),
    r_holdout = get_metric("r_holdout"),
    alpha = sel_alpha,
    r_parent = r_parent,
    selected = list(
      rank = item.set,
      items = sel_names,
      indices = as.integer(best_items$best_indices[[item.set]]),
      item_r = ind_cors[[item.set]],
      keys = ind_keys
    ),
    top = leaderboard[seq_len(min(n.sets, nrow(leaderboard))), ],
    leaderboard = leaderboard,
    item_r = ind_cors,
    item_stats = item_stats,
    scores = scores_out,
    target = target_out,
    pool = list(
      original = original_names,
      valid = filtered_names,
      searched = colnames(data)
    ),
    pivoting_item = pivot_item,
    n = c(total = nrow(data), train = length(train_rows), holdout = if (cross.validate) length(cv_holdout) else 0L),
    search = list(
      type = if (knockout || prefiltered) "optimised" else "exhaustive",
      combinations = num_combinations,
      searched = searched_combinations,
      searches = cpp_results$rounds,
      loco = cpp_results$items
    ),
    binary = NULL,   # binary targets only
    settings = list(
      n.items = n.items,
      target = if (!target_supplied) "internal" else if (is_binary) "binary" else "external",
      method = if (is.null(method)) NA else method,
      ranking_metric = ranking_metric,
      cross.validate = as.numeric(cross.validate),
      n.sets = n.sets,
      item.names = item.names,
      r.sq = r.sq,
      generate = generate,
      item.set = item.set,
      na.rm = na.rm,
      scale.vars = scale.vars,
      speed = speed,
      ceiling = ceiling,
      optimise = optimise,
      knockout.budget = knockout_budget,
      prefilter.ratio = prefilter.ratio,
      verbose = verbose,
      show.progress = show.progress,
      seed = seed
    ),
    call = the_call,
    version = tryCatch(if (identical(environmentName(topenv()), "reduceTo")) as.character(utils::packageVersion("reduceTo")) else "sourced",
                       error = function(e) NA_character_),
    seconds = NA_real_,  # filled in last, with the timings
    timings = NULL
  )

  # Binary classification metrics
  if (is_binary) {
    bin_info <- list(
      cutoff = leaderboard$`>=`[item.set],
      metric = ranking_metric,
      cross_validated = cross.validate > 0
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

    results_object$binary <- bin_info
  } else {
    results_object$binary <- NULL
  }

  # Seconds per step, from the call to here (a step marked more than once is summed), and the
  # C++ engine's own sections -- a breakdown of the "search" step, so not added to its total
  mark_time("output")
  step_secs <- diff(as.numeric(ck_time))
  step_secs <- tapply(step_secs, factor(ck_label[-1], levels = unique(ck_label[-1])), sum)
  total_secs <- sum(step_secs)
  timings_r <- data.frame(
    step = c(names(step_secs), "total"),
    s = round(c(as.numeric(step_secs), total_secs), 4),
    row.names = NULL
  )
  timings_r$percent <- round(100 * timings_r$s / max(total_secs, 1e-9), 2)

  timings_cpp <- c(timings_cpp, total = sum(timings_cpp))
  timings_cpp <- cbind(s = round(timings_cpp, 4),
                       percent = round(100 * timings_cpp / max(timings_cpp[["total"]], 1e-9), 2),
                       percent_of_run = round(100 * timings_cpp / max(total_secs, 1e-9), 2))
  results_object$timings <- list(steps = timings_r, engine = timings_cpp)
  results_object$seconds <- round(total_secs, 4)

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
  is_binary <- !is.null(x$binary)
  
  # --- HEADER ---
  type_str <- if (x$settings$cross.validate > 0) "Cross-Validated" else "Optimal"
  cat("\n=~=", type_str, "Short-Form Scale Results =~=\n")
  
  if (is_binary) {
    metric_map <- c("youden_j" = "Youden's J",
                    "binarised_r" = "Binarised Correlation",
                    "sum_score_r" = "Sum Score Correlation",
                    "auc" = "AUC")
    
    m_name <- metric_map[x$binary$metric]
    if(is.na(m_name)) m_name <- x$binary$metric
    cat(paste0("Objective: Binary Classification (Ranked by ", m_name, ")\n"))
  }
  
  # --- SEARCH: guaranteed (exhaustive) or heuristic (optimised) ---
  if (!is.null(x$search)) cat(paste0("Search: ", x$search$type, "\n"))

  # --- LEADERBOARD TABLE ---
  cat("\nTop", nrow(x$top), "Combinations:\n")
  print(format(x$top, digits = 6))
  
  cat("\n=~=~~=~=~~=~~=~=~~=~=\n")
  
  # --- BEST ITEM DETAILS ---
  cat(paste0("\nSelected Set (Rank ", x$selected$rank, "):"))
  cat("\nItems (Names): ", paste(x$selected$items, collapse = ", "))
  cat("\nItems (Indices): ", paste(x$selected$indices, collapse = ", "))
  cat("\nItem:target correlations: ", paste(round(x$selected$item_r, 2), collapse = ","), "\n")
  
  # --- BINARY PERFORMANCE BLOCK ---
  if (is_binary) {
    info <- x$binary
    
    cat("\n=~= Binary Classification Performance =~=")
    cat("\nOptimal Cutoff: Sum Score >=", info$cutoff)
    
    if (info$cross_validated) {
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
  invisible(x)
}
