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
#'   \item \strong{Heuristic Optimisation}: When the number of combinations exceeds the computational ceiling, the function automatically reduces the item pool through a beam search before searching.
#'   \item \strong{Cross-Validation}: Supports a Train/Holdout split (default 75/25) to validate findings and prevent overfitting. Reports performance metrics for both the training and holdout samples side-by-side.
#'   \item \strong{Binary Classifications}: For binary targets (0/1), automatically finds the optimal integer cut-off score to maximise classification accuracy (Youden's J) or binarised correlation.
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
#' @param generate If TRUE, returns computed scores for selected item set (default: FALSE)
#' @param item.set Which ranked set to generate scores for, used with generate = TRUE (default: 1)
#' @param show.progress If TRUE, displays progress bar during computation (default: TRUE)
#' @param cross.validate Numeric input controlling a data split into Training/Holdout 
#'   sets; if TRUE or 1, uses a 75\%/25\% split (default: FALSE)
#' @param optimise Controls heuristic pruning for large item pools: TRUE (default) 
#'   automatically optimises if combinations exceed ceiling; FALSE forces exhaustive 
#'   search (can be slow)
#' @param beam.width The number of top-performing subsets retained at each expansion 
#'   stage during heuristic optimisation (default: 2000)
#' @param opt.n The maximum number of cases (rows) to subsample during the heuristic 
#'   beam search (default: 5000)
#' @param ceiling Combination threshold triggering optimisation (default: 500,000)
#' @param scale.vars If TRUE, mean-centers and scales all columns (default: FALSE)
#' @param na.rm If TRUE, handles missing values via pairwise deletion (default: TRUE)
#' @param method Metric for ranking combinations (default: NULL for auto-selection): 
#'   "r" for Pearson correlation, "youden_j" for Youden's Index, or "binarised_r" 
#'   for correlation of binarised sum score (binary targets)
#'
#' @return A list of class \code{reduced_scale} containing:
#' \describe{
#'   \item{output}{Data frame of the top n.sets combinations and their performance metrics}
#'   \item{leaderboard}{Extended data frame of the top 100 combinations}
#'   \item{best_names}{Character vector of item names in the top-ranked set}
#'   \item{best_indices}{Integer vector of column indices in the top-ranked set}
#'   \item{scores}{(If generate = TRUE) A data frame containing the sum scores}
#'   \item{binary_info}{(If binary target) A list containing optimal cutoffs and classification metrics}
#'   \item{params}{List of parameters used in the function call}
#' }
#'
#' @author Paddy Maher, Max Planck Institute for Human Development
#' @export
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
                     generate = TRUE, item.set = 1, show.progress = T, cross.validate = 0, 
                     optimise = TRUE, beam.width = 2000, opt.n = 5000, ceiling = 500000, 
                     scale.vars = FALSE, na.rm = TRUE, method = NULL){
  
  # ============================================================================
  # HELPER FUNCTIONS
  # ============================================================================
  
  # Validate that a column is usable (numeric, has variance, no infinite values)
  is_valid_column <- function(x) {
    is.numeric(x) && !all(is.na(x)) && !any(is.infinite(x)) && sd(x, na.rm = TRUE) > 0
  }
  
  # Helper to apply item flipping
  flip_items <- function(x, the_items) {
    
    # 1. Safe check: handles both logical vectors (T/F) and numeric indices (1,5,9)
    should_flip <- if(is.logical(the_items)) any(the_items, na.rm = TRUE) else length(the_items) > 0
    
    if (should_flip) {
      if (scale.vars) {
        x[, the_items] <- x[, the_items] * -1
      } else {
        # FIX: Calculate max for EACH column individually
        # This handles mixed scales (e.g., flipping a 1-5 item AND a 0-10 item)
        
        # We use 'data' (the training set) to establish the maxes.
        # This ensures that holdout sets are flipped using the training parameters.
        col_maxes <- apply(data[, the_items, drop = FALSE], 2, max, na.rm = TRUE)
        
        # Apply flip: (max - x)
        # sweep(x, 2, stats, "-") calculates (x - max). 
        # We multiply by -1 to get (max - x).
        cols_to_mod <- x[, the_items, drop = FALSE]
        x[, the_items] <- -1 * sweep(cols_to_mod, 2, col_maxes, "-")
      }
    }
    return(x)
  }
  
  # Find optimal integer cutoff for binary targets
  # Can optimize for either binarised_r or youden_j
  find_optimal_cutoff_binary <- function(scores, binary_target, optimize_for = "youden_j") {
    valid_idx <- !is.na(scores) & !is.na(binary_target)
    scores_clean <- scores[valid_idx]
    target_clean <- binary_target[valid_idx]
    
    # Only test integers within observed range for efficiency
    score_range <- range(scores_clean)
    possible_integers <- floor(score_range[1]):ceiling(score_range[2])
    
    if (optimize_for == "binarised_r") {
      # Optimize for maximum binarised correlation
      calculate_metric <- function(cutoff) {
        binarised <- as.numeric(scores_clean >= cutoff)
        if (sd(binarised) == 0) return(NA)  # No variance
        return(cor(binarised, target_clean))
      }
      
      metric_values <- sapply(possible_integers, calculate_metric)
      valid_cutoffs <- !is.na(metric_values)
      
      if (!any(valid_cutoffs)) {
        optimal_cutoff <- median(possible_integers)
      } else {
        best_idx <- which.max(abs(metric_values[valid_cutoffs]))
        optimal_cutoff <- possible_integers[valid_cutoffs][best_idx]
      }
      
    } else {  # optimize_for == "youden_j"
      # Optimize for maximum Youden's J
      calculate_youden <- function(cutoff) {
        binarised <- as.numeric(scores_clean >= cutoff)
        tp <- sum(binarised == 1 & target_clean == 1)
        tn <- sum(binarised == 0 & target_clean == 0)
        fp <- sum(binarised == 1 & target_clean == 0)
        fn <- sum(binarised == 0 & target_clean == 1)
        
        if ((tp + fn) == 0 || (tn + fp) == 0) return(NA)
        
        sensitivity <- tp / (tp + fn)
        specificity <- tn / (tn + fp)
        return(sensitivity + specificity - 1)
      }
      
      youden_values <- sapply(possible_integers, calculate_youden)
      valid_cutoffs <- !is.na(youden_values)
      
      if (!any(valid_cutoffs)) {
        optimal_cutoff <- median(possible_integers)
      } else {
        best_idx <- which.max(youden_values[valid_cutoffs])
        optimal_cutoff <- possible_integers[valid_cutoffs][best_idx]
      }
    }
    
    # Calculate all metrics at the optimal cutoff
    binarised_optimal <- as.numeric(scores_clean >= optimal_cutoff)
    
    # Binarised correlation
    if (sd(binarised_optimal) == 0) {
      optimal_binarised_r <- 0
    } else {
      optimal_binarised_r <- cor(binarised_optimal, target_clean)
    }
    
    # Youden's J and components
    tp <- sum(binarised_optimal == 1 & target_clean == 1)
    tn <- sum(binarised_optimal == 0 & target_clean == 0)
    fp <- sum(binarised_optimal == 1 & target_clean == 0)
    fn <- sum(binarised_optimal == 0 & target_clean == 1)
    
    sensitivity <- tp / (tp + fn)
    specificity <- tn / (tn + fp)
    youden_j <- sensitivity + specificity - 1
    
    return(list(
      optimal_integer_cutoff = optimal_cutoff,
      binarised_r = optimal_binarised_r,
      youden_j = youden_j
    ))
  }
  
  perform_optimization <- function(current_cols, data, n.items, ceiling, targ, na.rm, beam_width, opt.n) {
    
    # --- SUBSAMPLING ---
    n_total <- nrow(data)
    if (n_total > opt.n) {
      set.seed(1)
      sub_idx <- sample(seq_len(n_total), opt.n) 
      data <- data[sub_idx, , drop = FALSE]
      targ <- targ[sub_idx]
    }
    
    k <- min(3, n.items)
    pool_indices <- current_cols
    current_combos <- t(combn(pool_indices, k))
    
    while (k <= n.items) {
      
      if (k %% 2 == 0) {
        cat(sprintf("\r~{ Beam search }~ =~= Evaluating subsets: %d of %d items", k, n.items))
        flush.console()
      } else {
        cat(sprintf("\r~{ Beam search }~ =+= Evaluating subsets: %d of %d items", k, n.items))
        flush.console()
      }
      
      # 1. Evaluate current combinations via C++
      scores <- evaluate_beam_cpp(data, current_combos, targ, na.rm)
      
      # 2. Rank and slice the top B combinations
      n_to_keep <- min(beam_width, nrow(current_combos))
      top_indices <- order(abs(scores), decreasing = TRUE)[1:n_to_keep]
      top_combos <- current_combos[top_indices, , drop = FALSE]
      
      if (k == n.items) break
      
      # 3. Expand the beam
      n_top <- nrow(top_combos)
      n_pool <- length(pool_indices)
      
      expanded <- top_combos[rep(1:n_top, each = n_pool), , drop = FALSE]
      new_items <- rep(pool_indices, times = n_top)
      expanded <- cbind(expanded, new_items)
      
      # 4. Fast duplicate check
      duplicate_mask <- rowSums(expanded[, 1:k, drop = FALSE] == new_items) > 0
      expanded <- expanded[!duplicate_mask, , drop = FALSE]
      
      # 5. Fully vectorized row sort (orders by row index first, then value)
      expanded <- matrix(
        expanded[order(row(expanded), expanded)], 
        ncol = ncol(expanded), 
        byrow = TRUE
      )
      
      # 6. Keep only unique combination paths
      current_combos <- unique(expanded)
      
      k <- k + 1
    }
    
    # --- FEATURE IMPORTANCE EXTRACTION ---
    item_frequencies <- table(top_combos)
    sorted_items <- as.numeric(names(sort(item_frequencies, decreasing = TRUE)))
    
    # --- GREEDY ASSEMBLY ---
    ranked_pool <- unique(c(sorted_items, pool_indices))
    final_pool <- ranked_pool[1:n.items] 
    
    for (i in (n.items + 1):length(ranked_pool)) {
      if (choose(i, n.items) > ceiling) {
        break
      }
      final_pool <- ranked_pool[1:i]
    }
    
    return(final_pool)
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
      
      # Determine if the column is entirely integers
      is_integer_like <- all(x[!na_mask] %% 1 == 0)
      
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
  
  # Process combinations in batches to manage memory
  process_combinations_in_batches <- function(data, targ, num_choose_from,
                                              original_indices, na.rm, 
                                              is_binary, n.items, ranking_metric, 
                                              optimize_for, show.progress) {
    
    compressed_data <- compress_for_cpp(data)
    
    # Use C++ to process all combinations and return top 100
    cpp_result <- process_all_combinations_cpp_parallel_float(
      data = compressed_data,
      n_items = n.items,
      num_choose_from = num_choose_from,
      na_rm = na.rm,
      target = targ,
      original_indices = original_indices,
      keep_top = leaderboard_length,
      show_progress = show.progress
    )
    
    # Convert to data.frame
    leaderboard <- data.frame(
      combination = cpp_result$combination,
      r = cpp_result$r,
      stringsAsFactors = FALSE
    )
    
    # For binary targets, calculate cutoff metrics for top 100
    if (is_binary) {
      # Need to recalculate scores for top 100 combinations
      top_combos <- strsplit(leaderboard$combination, ',')
      
      binarised_r <- numeric(nrow(leaderboard))
      cutoff <- numeric(nrow(leaderboard))
      youden_j <- numeric(nrow(leaderboard))
      
      for (i in 1:nrow(leaderboard)) {
        # Parse combination indices
        combo_indices <- as.numeric(top_combos[[i]])
        local_cols <- match(combo_indices, original_indices)
        
        # Calculate scores for this combination
        scores <- rowMeans(data[, local_cols, drop = FALSE], na.rm = na.rm) * n.items
        
        # Find optimal cutoff
        cutoff_info <- find_optimal_cutoff_binary(scores, targ, optimize_for)
        
        binarised_r[i] <- cutoff_info$binarised_r
        cutoff[i] <- cutoff_info$optimal_integer_cutoff
        youden_j[i] <- cutoff_info$youden_j
      }
      
      # Add binary metrics and rename correlation column
      leaderboard$`>=` <- cutoff
      leaderboard$binarised_r <- binarised_r
      leaderboard$youden_j <- youden_j
      
      # Re-rank by the specified ranking metric
      leaderboard <- leaderboard[order(abs(leaderboard[[ranking_metric]]), 
                                       decreasing = TRUE), ]
    }
    
    cpp_results <- list(leaderboard = leaderboard,
                        timings_cpp = cpp_result$timings_cpp)
    
    return(cpp_results)
  }
  
  # Extract best item identifiers from leaderboard
  extract_best_items <- function(leaderboard, item.set, data, original_indices) {
    
    # Parse all combinations - these are original indices
    comb_list_unordered <- lapply(strsplit(leaderboard$combination, ','), as.numeric)
    comb_list <- lapply( comb_list_unordered, sort )
    
    # Map to local positions in current data
    matched_comb_list <- lapply(comb_list, function(x) match(x, original_indices))
    
    # convert indices to column names
    best_names <- lapply(matched_comb_list, function(x) colnames(data)[x])
    
    return(list(best_names = best_names, best_indices = comb_list))
  }
  
  # Calculate individual item correlations for all sets in leaderboard
  calculate_item_correlations <- function(leaderboard, data, original_indices, 
                                          targ, items_to_flip) {
    
    if (nrow(data) > 10000) it.cor.sub <- sample(1:nrow(data), 10000) else it.cor.sub <- 1:nrow(data)
    
    # Parse all combinations - these are original indices
    comb_list_unordered <- lapply(strsplit(leaderboard$combination, ','), as.numeric)
    comb_list <- lapply( comb_list_unordered, sort )
    
    # Map to local positions in current data
    matched_comb_list <- lapply(comb_list, function(x) match(x, original_indices))
    
    unflip_indices <- intersect(cols_names, items_to_flip)
    
    flip_items_simple <- function(x, indices){
      x[,indices] <- x[,indices]*(-1)
      return(x)
    }
    
    dc2 <- as.vector(cor(flip_items_simple(data[it.cor.sub,], unflip_indices), targ[it.cor.sub], 'p') )
    
    # Calculate correlations for each item in each set
    ind_cors <- unlist(
      lapply(
        lapply(matched_comb_list, function(x) round(dc2, 2)[x]),
        paste, collapse = ',')
    )
    
    return(ind_cors)
  }
  
  
  # Generate scores for a specific item set
  generate_item_scores <- function(data, best_names, na.rm, is_binary, cutoff = NULL) {
    # Calculate sum scores
    scores_val <- rowMeans(data[, best_names, drop = FALSE], na.rm = na.rm) * length(best_names)
    
    # Always return a data frame so column subsetting [ , 1] works
    if (is_binary) {
      return(data.frame(sum_score = scores_val,
                        binary_score = scores_val >= cutoff))
    } else {
      return(data.frame(sum_score = scores_val))
    }
  }
  
  cross_validate_leaderboard <- function(leaderboard, data, target, 
                                         na.rm, is_binary, best_names) {
    
    # 1. Safety Check: Ensure data exists
    if (nrow(data) == 0) stop("Holdout data is empty. Cannot cross-validate.")
    
    # 2. Initialize matrices with exact dimensions
    n_rows <- nrow(data)
    n_cols <- nrow(leaderboard)
    scores_matrix <- matrix(NA, nrow = n_rows, ncol = n_cols)
    
    # 3. Fill matrix
    for (i in 1:n_cols) {
      # Use the specific items for this rank (i)
      current_items <- best_names[[i]]
      
      # Calculate scores for these items on the holdout data
      # Note: rowMeans handles the vector/matrix distinction automatically
      if (length(current_items) == 1) {
        # Single item case
        scores_matrix[, i] <- data[[current_items]]
      } else {
        # Multi item case
        scores_matrix[, i] <- rowMeans(data[, current_items, drop = FALSE], na.rm = na.rm) * length(current_items)
      }
    }
    
    # 4. Handle Binary Logic (if needed)
    if (is_binary) {
      binary_matrix <- matrix(NA, nrow = n_rows, ncol = n_cols)
      for (i in 1:n_cols) {
        binary_matrix[, i] <- as.numeric(scores_matrix[, i] >= leaderboard$`>=`[i])
      }
    }
    
    # 5. Rename Training Columns
    #train_cols <- c('r', 'youden_j', 'binarised_r')
    #existing_train_cols <- intersect(colnames(leaderboard), train_cols)
    #colnames(leaderboard)[match(existing_train_cols, colnames(leaderboard))] <- paste0(existing_train_cols, '_train')
    
    # 6. Calculate Holdout Correlations
    # We use 'pairwise.complete.obs' to handle any NAs safely
    leaderboard$r_holdout <- as.vector(cor(scores_matrix, target, use = 'pairwise.complete.obs'))
    
    #leaderboard <- leaderboard[order(leaderboard$r_train,decreasing = T),]
    
    #rownames(leaderboard) <- NULL
    
    if (is_binary) {
      
      # Binarised Correlation
      leaderboard$binarised_r_holdout <- as.vector(cor(binary_matrix, target, use = 'pairwise.complete.obs'))
      
      # Youden's J Calculation
      leaderboard$youden_j_holdout <- sapply(1:n_cols, function(col_idx) {
        preds <- binary_matrix[, col_idx]
        actual <- target
        
        # Remove NAs for calculation
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
  
  
  # ============================================================================
  # MAIN FUNCTION BODY
  # ============================================================================
  
  t0 <- Sys.time()
  
  leaderboard_length <- 100
  
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
      message("scale.vars = TRUE: treating binary target as continuous variable")
    }
    
    if (is_binary) {
      message("=~= Binary target detected (values: 0, 1): optimising for classification performance.")
    }
  }
  
  # Determine ranking metric and optimization method
  if (is_binary) {
    # Validate method for binary targets
    valid_binary_methods <- c("r", "binarised_r", "youden_j")
    
    if (is.null(method)) {
      ranking_metric <- "youden_j"
      message("=~= Ranking combinations by Youden's J (use method = 'binarised_r' or 'r' to change)")
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
      message("Note: 'method' parameter ignored for non-binary targets")
    }
  }
  
  t1 <- Sys.time()
  
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
  
  t2 <- Sys.time()
  
  # Filter to valid columns
  data <- data[, valid_mask, drop = FALSE]
  cols <- 1:ncol(data)
  original_indices <- all_original_indices[valid_mask]
  filtered_names <- colnames(data)  # Save valid column names
  
  t3 <- Sys.time()
  
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
  
  t4 <- Sys.time()
  
  # flip items negatively correlated with most central item
  
  if (!target_supplied) {
    set.seed(1)
    if (nrow(data) > 2000) cor_subsample <- sample(1:nrow(data), 2000) else cor_subsample <- 1:nrow(data)
    
    dc <- cor(data[cor_subsample,], use = "p")
    most_central_item <- order(colSums(abs(dc), na.rm = TRUE), decreasing = TRUE)[1]
    
    pivot_item <- colnames(data)[most_central_item]
    
    items_to_flip_TF <- dc[, most_central_item] < 0
    items_to_flip <- filtered_names[items_to_flip_TF]
    
    # Reverse-score items
    data <- flip_items(data, items_to_flip) 
    
    #create target
    target <- rowMeans(data, na.rm = na.rm)
  } 
  
  if (target_supplied) {
    set.seed(1)
    if (nrow(data) > 2000) cor_subsample <- sample(1:nrow(data), 2000) else cor_subsample <- 1:nrow(data)
    
    # For external criteria: flip items negatively correlated with target
    dc <- cor(data[cor_subsample,], target[cor_subsample], use = "p")
    dc[is.na(dc)] <- 0
    
    items_to_flip_TF <- as.vector(dc) < 0
    items_to_flip <- filtered_names[items_to_flip_TF]
    items_to_zero <- (abs(as.vector(dc)) > 0.999999 | abs(as.vector(dc)) < 0.0001)
    data[,items_to_zero] <- 0
    
    data <- flip_items(data, items_to_flip)  
    pivot_item <- NA
  }
  
  if (cross.validate) {
    data_holdout <- data[cv_holdout,]
    target_holdout <- target[cv_holdout]
    data <- data[cv_subset,]
    target <- target[cv_subset]
  }
  
  t5 <- Sys.time()
  
  # Check if optimization is needed
  num_combinations <- choose(ncol(data), n.items)
  
  if (num_combinations > ceiling) {
    
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    base_year <- 2024
    base_constant <- 5e-10
    # Scaling factor: Constant halves every 2 years
    # Formula: base_constant * (0.5 ^ ((current_year - base_year) / 2))
    years_passed <- current_year - base_year
    CPU_constant <- base_constant * (0.5 ^ (years_passed / 2))
    est_seconds <- num_combinations*nrow(data) * CPU_constant
    opt_est_seconds <- ceiling*nrow(data) * CPU_constant
    
    message(paste0("=~ Optimisation triggered: this dataset would generate ", 
                   format(num_combinations, big.mark = ",",scientific = FALSE), 
                   " combinations (~",format_duration(est_seconds), " to ",format_duration(est_seconds*5),
                   " with N = ",format(nrow(data), big.mark = ",",scientific = FALSE) ,
                   "). \n=~ Optimisation will be used to reduce combinations to below ", 
                   format(ceiling, big.mark = ",",scientific = FALSE),
                   " (~",format_duration(opt_est_seconds), " to ",format_duration(opt_est_seconds*5),
                   "). \n=~ To change this threshold, use the 'ceiling' argument."))
    
    cols <- perform_optimization(cols, data, n.items, ceiling, target, na.rm, beam.width, opt.n)
  }
  
  cols_names <- filtered_names[cols]
  
  t6 <- Sys.time()
  
  # Subset to optimized columns
  data <- data[, cols, drop = FALSE]
  if (cross.validate) { data_holdout <- data_holdout[, cols, drop = FALSE] }
  
  t7 <- Sys.time()
  
  # Apply standardization if requested
  if (scale.vars) data <- apply(data, 2, scale)
  
  t8 <- Sys.time()
  
  # Heuristic: Check for wide variations in item scales
  if (!scale.vars) {
    col_ranges <- apply(data, 2, function(x) diff(range(x, na.rm = TRUE)))
    
    # If the largest scale is > 3x larger than the smallest, warn the user
    # (e.g. mixing Binary (0-1) with Likert (1-7) or 100-point scales)
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
  
  t9 <- Sys.time()
  
  # Process combinations and build leaderboard (always uses indices internally)
  cpp_results <- process_combinations_in_batches(data, target, num_choose_from,
                                                 original_indices, na.rm,
                                                 is_binary, n.items, ranking_metric,
                                                 optimize_for, show.progress)
  
  leaderboard <- cpp_results$leaderboard
  timings_cpp <- cpp_results$timings_cpp                                                                                                                   
  
  # Clean up leaderboard
  rownames(leaderboard) <- NULL
  
  t10 <- Sys.time()
  
  # Extract best items
  best_items <- extract_best_items(leaderboard, item.set, data, original_indices)
  
  t11 <- Sys.time()
  
  # Calculate item-level correlations
  ind_cors <- calculate_item_correlations(leaderboard, data, 
                                          original_indices, target, items_to_flip)
  
  ind_keys <- 2*(as.numeric(strsplit(ind_cors[item.set], ',')[[1]]) > 0)-1
  
  t12 <- Sys.time()
  
  # Cross-validate if requested
  if (cross.validate) {
    # Ensure holdout data actually exists
    if (!exists("data_holdout") || !exists("target_holdout")) {
      stop("Cross-validation requested but holdout data not found.")
    }
    
    leaderboard <- cross_validate_leaderboard(
      leaderboard = leaderboard, 
      data = data_holdout,         # Explicitly pass the holdout set
      target = target_holdout,     # Explicitly pass the holdout target
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
        leaderboard$R2_train <- leaderboard$r_train^2
        leaderboard$R2_holdout <- leaderboard$r_holdout^2
      }
    } else {
      if (!cross.validate) {
        leaderboard$sum_scored_R <- leaderboard$sum_score_r^2
      } else {
        leaderboard$sum_scored_R2_train <- leaderboard$sum_score_r_train^2
        leaderboard$binarised_R2_train <- leaderboard$binarised_r_train^2
      }
    }
  }
  
  
  t13 <- Sys.time()
  
  # --- REASSEMBLE DATA FOR SCORE GENERATION ---
  # Merges Training and Holdout sets back into original row order
  if (cross.validate) {
    n_total <- nrow(data) + nrow(data_holdout)
    
    # Initialize container with original dimensions
    data_reassembled <- matrix(NA, nrow = n_total, ncol = ncol(data))
    colnames(data_reassembled) <- colnames(data)
    
    # Slot rows back into their saved original indices
    data_reassembled[cv_subset, ] <- as.matrix(data)
    
    #free up memory immediately, then continue
    rm(data)
    data_reassembled[cv_holdout, ] <- as.matrix(data_holdout)
    
    # Overwrite 'data' with the full, unscrambled dataset
    data <- data_reassembled
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
   # leaderboard$combination <- sapply(strsplit(leaderboard$combination, ','), function(indices) {
   #   idx_nums <- as.numeric(indices)
   #   paste(original_names[idx_nums], collapse = ",")
   # })
    leaderboard$combination <- best_items$best_names
  }
  
  timings_r = data.frame(
    time1 = as.numeric(difftime(t1,t0, units = "secs")),
    time2 = as.numeric(difftime(t2,t1, units = "secs")),
    time3 = as.numeric(difftime(t3,t2, units = "secs")),
    time4 = as.numeric(difftime(t4,t3, units = "secs")),
    time5 = as.numeric(difftime(t5,t4, units = "secs")),
    time6 = as.numeric(difftime(t6,t5, units = "secs")),
    time7 = as.numeric(difftime(t7,t6, units = "secs")),
    time8 = as.numeric(difftime(t8,t7, units = "secs")),
    time9 = as.numeric(difftime(t9,t8, units = "secs")),
    time10= as.numeric(difftime(t10,t9, units = "secs")),
    time11= as.numeric(difftime(t11,t10, units = "secs")),
    time12= as.numeric(difftime(t12,t11, units = "secs")),
    time13= as.numeric(difftime(t13,t12, units = "secs"))
  )
  timings_r$total <- rowSums(timings_r)
  
  timings_r = cbind(s = as.vector(round(timings_r,4)),
                    percent = as.vector(round(200*timings_r/rowSums(timings_r),2)))
  
  names(timings_cpp) <- paste0('time',1:length(timings_cpp))
  timings_cpp <- c(timings_cpp, total = sum(timings_cpp))
  
  timings_cpp = cbind(s = round(timings_cpp,4),
                      percent = as.vector(round(200*timings_cpp/sum(timings_cpp),2)))
  
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
    scores = as.matrix(computed_scores)[, , drop = F],
    target = target,
    original_names = original_names,
    filtered_names = filtered_names,
    pool_names = colnames(data),
    pivoting_item = pivot_item,
    timings = list(timings_r = timings_r, timings_cpp = timings_cpp),
    params = list(
      n.items = n.items,
      n.sets = n.sets,
      item.set = item.set,
      cross.validated = cross.validate,
      ranking_metric = ranking_metric,
      sample_size = nrow(data),
      final_pool_size = num_choose_from
    )
  )
  
  # ROBUST BINARY INFO POPULATION
  if (is_binary) {
    # 1. Always get the cutoff (it doesn't change names)
    bin_info <- list(
      cutoff = leaderboard$`>=`[item.set],
      ranking_metric = ranking_metric,
      is_cv = cross.validate
    )
    
    # 2. Extract Training Metrics (handle _train rename if CV occurred)
    if (cross.validate) {
      bin_info$train <- list(
        binarised_r = get_metric("binarised_r_train"),
        sum_score_r = get_metric("sum_score_r_train"),
        youden_j    = get_metric("youden_j_train")
      )
      
      bin_info$holdout <- list(
        binarised_r = get_metric("binarised_r_holdout"),
        sum_score_r = get_metric("sum_score_r_holdout"),
        youden_j    = get_metric("youden_j_holdout")
      )
    } else {
      # No CV, just grab standard columns
      bin_info$results <- list(
        binarised_r = get_metric("binarised_r"),
        sum_score_r = get_metric("sum_score_r"),
        youden_j    = get_metric("youden_j")
      )
    }
    
    results_object$binary_info <- bin_info
  }
  
  class(results_object) <- "reduced_scale"
  return(results_object)
}

#' Print Method for reduced_scale Objects
#' @export
print.reduced_scale <- function(x, ...) {
  is_binary <- !is.null(x$binary_info)
  
  # --- HEADER ---
  type_str <- if(x$params$cross.validated) "Cross-Validated" else "Optimal"
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
  cat(paste0("\nSelected Set (Rank ",x$params$item.set,"):"))
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