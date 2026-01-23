#' Find Optimal Short-Form Scales
#'
#' @description
#' Systematically evaluates subsets of items from a larger scale to find the 
#' combination that correlates most highly with a target criterion. Includes 
#' optimisations for handling very large numbers of item combinations through 
#' iterative item reduction and memory-efficient batch processing.
#' 
#' Optimisation modes:
#' \itemize{
#'   \item \strong{Scale Preservation} (Default): Maximize correlation with parent scale
#'   \item \strong{Criterion Validity}: Maximize prediction of external variable
#' }
#'
#' @param data Matrix or data.frame containing item responses
#' @param n.items Desired number of items in the final short-form scale
#' @param n.sets Number of top-performing item sets to return (default: 5)
#' @param item.names If TRUE, output lists item names instead of column numbers (default: FALSE)
#' @param r.sq If TRUE, returns R² alongside correlation (default: FALSE)
#' @param generate If TRUE, returns computed scores for selected item set (default: FALSE)
#' @param item.set Which ranked set to generate scores for, used with generate = TRUE (default: 1)
#' @param targ Optional target criterion vector:
#'   \itemize{
#'     \item NULL (default): Uses mean score of full scale
#'     \item Numeric: External criterion (e.g., depression severity)
#'     \item Binary (0/1): Maximizes discriminant validity
#'   }
#' @param cross.validate If TRUE, performs 5-fold cross-validation (default: FALSE)
#' @param optimise Controls heuristic pruning for large item pools:
#'   \itemize{
#'     \item NULL (default): Prompts user when combinations exceed ceiling
#'     \item TRUE: Automatically optimises if combinations exceed ceiling
#'     \item FALSE: Forces exhaustive search regardless of ceiling
#'   }
#' @param factors Number of PCA factors for optimisation (default: 1)
#' @param ceiling Combination threshold triggering optimisation (default: 1,000,000)
#' @param scale.vars If TRUE, mean-centers and scales all columns (default: FALSE)
#' @param na.rm If TRUE, handles missing values via pairwise deletion (default: TRUE)
#'
#' @return List of class \code{reduced_scale} containing:
#'   \itemize{
#'     \item output: Data frame of top n.sets with correlations
#'     \item leaderboard: Extended ranking of top 100 combinations
#'     \item item_cors: Individual item correlations for each set
#'     \item best_indices: Column numbers of top-ranked items
#'     \item best_names: Column names of top-ranked items
#'     \item best_item_cors: Item-level correlations for best set
#'     \item scores: Computed scores if generate = TRUE
#'     \item params: List of function parameters used
#'   }
#'
#' @author Paddy Maher, Goldsmiths, University of London
#' @export
#'
#' @examples
#' \dontrun{
#' library(MASS)
#' library(psych)  # Required for optimisation
#' 
#' # Create simulated data
#' set.seed(42)
#' loadings <- rnorm(10, 0.75, 0.08) * sign(rnorm(10))
#' cor_matrix <- outer(loadings, loadings)
#' diag(cor_matrix) <- 1
#' sim_data <- MASS::mvrnorm(200, rep(0, 10), cor_matrix, empirical = TRUE)
#' sim_data <- as.data.frame(sim_data)
#' colnames(sim_data) <- paste0("item_", 1:10)
#' 
#' # Reduce to 3 items (internal consistency)
#' reduceTo(sim_data, 3)
#' 
#' # Reduce with external target and cross-validation
#' binary_target <- ifelse(rowMeans(sim_data) > 0, 1, 0)
#' reduceTo(sim_data, 3, targ = binary_target, cross.validate = TRUE)
#' }

reduceTo <- function(data, n.items, n.sets = 5, item.names = FALSE, r.sq = FALSE, 
                     generate = FALSE, item.set = 1, targ = NULL, cross.validate = FALSE,
                     optimise = NULL, factors = 1, ceiling = 10^6, 
                     scale.vars = FALSE, na.rm = TRUE) {
  
  # ============================================================================
  # HELPER FUNCTIONS
  # ============================================================================
  
  # Validate that a column is usable (numeric, has variance, no infinite values)
  is_valid_column <- function(x) {
    is.numeric(x) && !all(is.na(x)) && !any(is.infinite(x)) && sd(x, na.rm = TRUE) > 0
  }
  
  # Reduce item pool when combinations exceed ceiling
  perform_optimisation <- function(current_cols, data, n.items, ceiling, targ, factors) {
    num_combs <- choose(length(current_cols), n.items)
    
    # Sample data for optimisation (max 20k rows for speed)
    n_rows <- nrow(data)
    opt_indices <- if (n_rows > 20000) sample(1:n_rows, 20000) else 1:n_rows
    
    if (is.null(targ)) {
      # PCA-based optimisation for internal consistency
      current_cols <- optimise_via_pca(current_cols, data, opt_indices, n.items, 
                                       ceiling, num_combs, factors)
    } else {
      # Correlation-based optimisation for external criteria
      current_cols <- optimise_via_correlation(current_cols, data, opt_indices, 
                                               n.items, ceiling, targ)
    }
    
    return(current_cols)
  }
  
  # PCA-based item reduction strategy
  optimise_via_pca <- function(current_cols, data, opt_indices, n.items, ceiling, 
                               num_combs, factors) {
    data_sample <- data[opt_indices, current_cols, drop = FALSE]
    
    # Remove constant columns to prevent PCA failures
    subset_variance <- apply(data_sample, 2, sd, na.rm = TRUE)
    valid_subset_cols <- subset_variance > 0
    current_cols <- current_cols[valid_subset_cols]
    data_sample <- data_sample[, valid_subset_cols, drop = FALSE]
    
    # Iteratively drop weakest items until combinations are manageable
    while (num_combs > ceiling) {
      pca_res <- psych::pca(data_sample, nfactors = factors)
      pca_loadings <- apply(abs(pca_res$loadings), 1, max)
      
      # Rank items by loading strength
      local_rank <- order(pca_loadings, decreasing = TRUE)
      
      # Calculate aggressive dropping ratio that slows as we approach ceiling
      # Uses ^(1/20) to create gentle curve, then *0.9 to ensure we don't overshoot
      dropping_ratio <- (num_combs / ceiling) ^ (1/20)
      n_to_keep <- round(length(local_rank) * (1/dropping_ratio) * 0.9)
      top_local_indices <- local_rank[1:n_to_keep]
      
      # Update working set
      current_cols <- current_cols[top_local_indices]
      data_sample <- data_sample[, top_local_indices, drop = FALSE]
      num_combs <- choose(length(current_cols), n.items)
    }
    
    return(current_cols)
  }
  
  # Correlation-based item reduction strategy
  optimise_via_correlation <- function(current_cols, data, opt_indices, n.items, 
                                       ceiling, targ) {
    # Calculate item-criterion correlations
    suppressWarnings({
      cor_res <- cor(data[opt_indices, current_cols], 
                     targ[opt_indices], 
                     use = "pairwise.complete.obs")
    })
    cor_res[is.na(cor_res)] <- 0
    
    # Rank items by absolute correlation
    item_rank <- order(abs(as.vector(cor_res)), decreasing = TRUE)
    
    # Find optimal number of items to keep
    k <- length(current_cols)
    while (choose(k, n.items) > ceiling && k > n.items) {
      k <- floor(k * 0.9)
    }
    
    # Fine-tune to get as close to ceiling as possible without exceeding
    while (choose(k + 1, n.items) <= ceiling && (k + 1) <= length(current_cols)) {
      k <- k + 1
    }
    
    # Safety check
    if (k < n.items) k <- n.items
    
    # Return top k items
    top_local_indices <- item_rank[1:k]
    return(current_cols[top_local_indices])
  }
  
  # Perform 5-fold cross-validation on leaderboard
  cross_validate_leaderboard <- function(leaderboard, data, targ, item.names, 
                                         original_indices, na.rm) {
    # Create random folds
    folds <- sample(rep(1:5, length.out = nrow(data)))
    cv_mean_r <- numeric(nrow(leaderboard))
    
    # Test each candidate set
    for (i in 1:nrow(leaderboard)) {
      # Parse combination string to column indices
      comb_str <- leaderboard$combination[i]
      comb_vec <- strsplit(comb_str, ',')[[1]]
      
      if (item.names) {
        local_cols <- match(comb_vec, colnames(data))
      } else {
        comb_nums <- as.numeric(comb_vec)
        local_cols <- match(comb_nums, original_indices)
      }
      
      # Test on each fold
      fold_rs <- numeric(5)
      for (k in 1:5) {
        test_idx <- which(folds == k)
        pred_scores <- rowMeans(data[test_idx, local_cols, drop = FALSE], na.rm = na.rm)
        fold_rs[k] <- cor(pred_scores, targ[test_idx], use = "pairwise.complete.obs")
      }
      
      cv_mean_r[i] <- mean(fold_rs, na.rm = TRUE)
    }
    
    # Re-rank by cross-validated performance
    leaderboard$cv_r <- cv_mean_r
    leaderboard <- leaderboard[order(abs(leaderboard$cv_r), decreasing = TRUE), ]
    
    return(leaderboard)
  }
  
  # Process combinations in batches to manage memory
  process_combinations_in_batches <- function(item_combinations, data, targ, 
                                              item.names, original_indices, na.rm) {
    num_combinations <- ncol(item_combinations)
    num_combinations_final <- num_combinations
    batch_size <- 10000
    num_batches <- ceiling(num_combinations / batch_size)
    leaderboard <- data.frame(combination = character(0), r = numeric(0))
    
    for (j in 1:num_batches) {
      # Progress indicator for multi-batch processing
      if (num_batches > 1) {
        cat('\rProcessing Batch:', j, 'of', num_batches, 
            '| Total combinations:', format(num_combinations_final, big.mark = ","))
        flush.console()
      }
      
      # Extract current batch
      start_col <- (j - 1) * batch_size + 1
      end_col <- min(j * batch_size, num_combinations)
      current_batch_combs <- item_combinations[, start_col:end_col, drop = FALSE]
      
      # Calculate scores for each combination in batch
      scores_batch <- matrix(NA, ncol = ncol(current_batch_combs), nrow = nrow(data))
      for (i in 1:ncol(current_batch_combs)) {
        scores_batch[, i] <- rowMeans(data[, current_batch_combs[, i], drop = FALSE], 
                                      na.rm = na.rm)
      }
      
      # Label combinations
      if (item.names) {
        colnames(scores_batch) <- apply(current_batch_combs, 2, 
                                        function(x) paste(colnames(data)[x], collapse = ","))
      } else {
        colnames(scores_batch) <- apply(current_batch_combs, 2, 
                                        function(x) paste(original_indices[x], collapse = ","))
      }
      
      # Correlate with target and update leaderboard
      cor_results_batch <- cor(scores_batch, targ, use = "pairwise")
      combined_results <- rbind(leaderboard, 
                                data.frame(combination = rownames(cor_results_batch),
                                           r = cor_results_batch[, 1]))
      leaderboard <- combined_results[order(abs(combined_results$r), decreasing = TRUE), ]
      leaderboard <- head(leaderboard, 100)  # Keep top 100
    }
    
    # Clear progress line
    if (num_batches > 1) cat('\r', rep(' ', 80), '\n', sep = '')
    
    return(leaderboard)
  }
  
  # Extract best item identifiers from leaderboard
  extract_best_items <- function(leaderboard, item.set, item.names, data, original_indices) {
    top_set_string <- leaderboard$combination[item.set]
    top_set_vec <- strsplit(top_set_string, ',')[[1]]
    
    if (item.names) {
      # String contains names: map to indices
      best_names <- top_set_vec
      local_indices <- match(best_names, colnames(data))
      best_indices <- original_indices[local_indices]
    } else {
      # String contains indices: map to names
      best_indices <- as.numeric(top_set_vec)
      local_indices <- match(best_indices, original_indices)
      best_names <- colnames(data)[local_indices]
    }
    
    return(list(best_names = best_names, best_indices = best_indices))
  }
  
  # Calculate individual item correlations for all sets in leaderboard
  calculate_item_correlations <- function(leaderboard, item.names, data, original_indices, 
                                          targ, items_to_flip) {
    # Parse all combinations
    if (item.names) {
      comb_list <- strsplit(leaderboard$combination, ',')
      matched_comb_list <- lapply(comb_list, function(x) match(x, colnames(data)))
    } else {
      comb_list <- lapply(strsplit(leaderboard$combination, ','), as.numeric)
      matched_comb_list <- lapply(comb_list, function(x) match(x, original_indices))
    }
    
    # Helper to apply item flipping
    flip_items <- function(x) {
      x[, items_to_flip] <- x[, items_to_flip] * -1
      return(x)
    }
    
    # Calculate correlations for each item in each set
    ind_cors <- unlist(
      lapply(
        lapply(matched_comb_list, 
               function(x) round(cor(flip_items(data), targ, 'pairwise'), 3)[x]),
        paste, collapse = ','))
    
    return(ind_cors)
  }
  
  # Generate scores for a specific item set
  generate_item_scores <- function(leaderboard, item.set, item.names, data, 
                                   original_indices, na.rm) {
    target_set_string <- leaderboard$combination[item.set]
    target_vec <- strsplit(target_set_string, ',')[[1]]
    
    if (item.names) {
      computed_scores <- scale(rowMeans(data[, target_vec, drop = FALSE], na.rm = na.rm))
    } else {
      target_indices_raw <- as.numeric(target_vec)
      local_indices <- match(target_indices_raw, original_indices)
      computed_scores <- scale(rowMeans(data[, local_indices, drop = FALSE], na.rm = na.rm))
    }
    
    return(computed_scores)
  }
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~
  # MAIN FUNCTION BODY ------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Convert tibbles to standard data frames
  if (inherits(data, "tbl_df") || inherits(data, "tbl")) {
    data <- as.data.frame(data)
  }
  
  # Ensure column names exist
  if (is.null(colnames(data))) {
    colnames(data) <- paste0("Col_", 1:ncol(data))
  }
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
  
  # Filter to valid columns
  data <- data[, valid_mask]
  cols <- 1:ncol(data)
  original_indices <- all_original_indices[valid_mask]
  
  # Convert to matrix for computational efficiency
  if (!is.matrix(data)) data <- as.matrix(data)
  
  # Check if optimisation is needed
  num_combinations <- choose(ncol(data), n.items)
  
  if (num_combinations > ceiling) {
    warning_msg <- paste("This will generate", 
                         format(num_combinations, big.mark = ","), 
                         "combinations.")
    
    if (is.null(optimise)) {
      # Prompt user for decision
      message(warning_msg)
      user_choice <- readline(prompt = "Enter 'c' to continue, 'o' for optimisation, or 'q' to quit: ")
      if (tolower(user_choice) == "o") {
        cols <- perform_optimisation(cols, data, n.items, ceiling, targ, factors)
      } else if (tolower(user_choice) != "c") {
        stop("Process aborted.")
      }
    } else if (optimise == TRUE) {
      message("Optimisation enabled. Reducing item set...")
      cols <- perform_optimisation(cols, data, n.items, ceiling, targ, factors)
    }
  }
  
  # Subset to optimised columns
  data <- data[, cols, drop = FALSE]
  
  # Determine which items need to be reverse-scored
  if (is.null(targ)) {
    # For internal consistency: flip items negatively correlated with most central item
    dc <- cor(data, use = "pairwise.complete.obs")
    most_central_item <- order(colSums(abs(dc), na.rm = TRUE), decreasing = TRUE)[1]
    items_to_flip <- dc[, most_central_item] < 0
  } else {
    # For external criteria: flip items negatively correlated with target
    suppressWarnings(dc <- cor(data, targ, use = "pairwise.complete.obs"))
    dc[is.na(dc)] <- 0
    items_to_flip <- as.vector(dc) < 0
    items_to_rem <- abs(as.vector(dc)) > 0.999999 | as.vector(dc) == 0
  }
  
  # Apply standardization if requested
  if (scale.vars) data <- apply(data, 2, scale)
  
  # Reverse-score items
  data[, items_to_flip] <- data[, items_to_flip] * -1
  
  # Remove items with perfect or zero correlation with target
  if (!is.null(targ)) {
    cols <- cols[!items_to_rem]
    data <- data[, !items_to_rem, drop = FALSE]
    dc <- dc[!items_to_rem, , drop = FALSE]
    items_to_flip <- items_to_flip[!items_to_rem]
  }
  
  # Update index mapping after filtering
  original_indices <- original_indices[cols]
  
  # Generate all item combinations
  item_combinations <- combn(1:ncol(data), n.items)
  
  # Set target to full scale mean if not provided
  if (is.null(targ)) targ <- rowMeans(data, na.rm = na.rm)
  
  # Process combinations and build leaderboard
  leaderboard <- process_combinations_in_batches(item_combinations, data, targ, 
                                                 item.names, original_indices, na.rm)
  
  # Cross-validate if requested
  if (cross.validate) {
    leaderboard <- cross_validate_leaderboard(leaderboard, data, targ, item.names, 
                                              original_indices, na.rm)
  }
  
  # Clean up leaderboard
  rownames(leaderboard) <- NULL
  
  # Extract best items
  best_items <- extract_best_items(leaderboard, item.set, item.names, data, original_indices)
  
  # Calculate item-level correlations
  ind_cors <- calculate_item_correlations(leaderboard, item.names, data, 
                                          original_indices, targ, items_to_flip)
  
  # Generate scores if requested
  computed_scores <- NULL
  if (generate) {
    computed_scores <- generate_item_scores(leaderboard, item.set, item.names, 
                                            data, original_indices, na.rm)
  }
  
  # Construct results object
  results_object <- list(
    output = leaderboard[1:n.sets, ],
    leaderboard = leaderboard,
    item_cors = ind_cors,
    best_indices = best_items$best_indices,
    best_names = best_items$best_names,
    best_item_cors = ind_cors[item.set],
    scores = computed_scores,
    params = list(
      n.items = n.items,
      n.sets = n.sets,
      cross.validated = cross.validate
    )
  )
  
  class(results_object) <- "reduced_scale"
  
  return(results_object)
}


#' Print Method for reduced_scale Objects
#' 
#' @param x A reduced_scale object from reduceTo()
#' @param ... Additional arguments (ignored)
#' @export
print.reduced_scale <- function(x, ...) {
  result_type <- ifelse(x$params$cross.validated, "Cross-Validated", "Optimal")
  
  cat("\n---", result_type, "Short-Form Scale Results ---\n")
  cat("Top", nrow(x$output), "Combinations:\n\n")
  print(x$output)
  
  cat("\n----------------------------------------")
  
  if (!is.null(x$best_indices)) {
    cat("\nBest Items (Indices): ", paste(x$best_indices, collapse = ", "))
  }
  
  if (!is.null(x$best_names)) {
    cat("\nBest Items (Names): ", paste(x$best_names, collapse = ", "))
  }
  
  if (!is.null(x$best_item_cors)) {
    cat("\nItem-level correlations: ", paste(x$best_item_cors))
  }
  
  if (!is.null(x$scores)) {
    cat("\nScores generated: Yes (Access via $scores)")
  }
  
  cat("\n")
}