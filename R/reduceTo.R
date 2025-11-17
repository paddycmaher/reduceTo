#' Find Optimal Short-Form Scales
#'
#' @description
#' This function systematically evaluates subsets of items from a larger scale
#' to find the combination that correlates most highly with a target criterion.
#' It includes optimizations for handling a very large number of combinations,
#' such as iterative item reduction (optimisation) and memory-efficient batch processing.
#'
#' @param data A numeric matrix or data.frame containing the item responses.
#' @param n.items The desired number of items in the final short-form scale.
#' @param n.sets The number of top-performing item sets to return. Defaults to 5.
#' @param item.names Logical. If TRUE, the output will list the item names instead
#'   of their column numbers. Defaults to FALSE.
#' @param r.sq Logical. If TRUE, the squared correlation (R-squared) is returned
#'   alongside the correlation. Defaults to FALSE.
#' @param generate Logical. If TRUE, the function returns the computed scores for
#'   the single best item set. Defaults to FALSE.
#' @param item.set An integer specifying which item set to generate scores for
#'   (e.g., 1 for the best set). Only used if `generate = TRUE`.
#' @param na.rm Logical. If TRUE, missing values are handled by pairwise deletion
#'   or row-wise removal in means. Defaults to TRUE.
#' @param targ An optional numeric vector to serve as the target criterion.
#'   If NULL (the default), the mean score of the full scale is used.
#' @param optimise Logical or NULL. Controls the optimisation process.
#'   - `NULL` (default): If combinations exceed `ceiling`, prompts the user to
#'     choose between continuing, optimising, or quitting.
#'   - `TRUE`: Automatically runs the optimisation process if combinations
#'     exceed `ceiling`.
#'   - `FALSE`: Proceeds with the full, non-optimised process, even if
#'     combinations exceed `ceiling` (after a warning).
#' @param ceiling Numeric. The number of combinations above which the `optimise`
#'   prompt or process is triggered. Defaults to 100,000.
#'
#' @return A data.frame listing the top `n.sets` of items and their correlation
#'   with the target. If `generate = TRUE`, returns a numeric vector of scores
#'   for the selected `item.set`.
#'
#' @author Paddy Maher, Goldsmiths, University of London. Contact: paddycarstenmaher@gmail.com
#'
#' @examples
#' \dontrun{
#' library(MASS) 
#' # Note: The 'psych' package is required for pca() if using optimisation.
#' library(psych)
#' 
#' # 1. Create simulated data
#' set.seed(42)
#' loadings <- c(rnorm(10,0.75,0.08)*sign(rnorm(10)))
#' target_cor_matrix <- outer(loadings, loadings)
#' diag(target_cor_matrix) <- 1
#' simulated_data <- MASS::mvrnorm(n = 200, mu = rep(0, 10), 
#'                                 Sigma = target_cor_matrix, empirical = TRUE)
#' simulated_data <- as.data.frame(simulated_data)
#' colnames(simulated_data) <- paste0("item_", 1:10)
#' 
#' # 2. Reduce the scale to 3 items
#' reduceTo(simulated_data, 3)
#' 
#' # 3. Reduce a larger set, triggering the optimisation
#' large_data <- as.data.frame(matrix(rnorm(200*30), 200, 30))
#' colnames(large_data) <- paste0("item_", 1:30)
#' reduceTo(large_data, 5, optimise = TRUE, item.names = TRUE)
#' }
#'
#' @export

reduceTo <- function (data, n.items, n.sets = 5, item.names = FALSE, r.sq = FALSE, 
                      generate = FALSE, item.set = 1, na.rm = TRUE, targ = NULL, 
                      optimise = NULL, ceiling = 10^5) 
{
  
  # --- Helper Function for Optimisation ---
  # This nested function iteratively removes the 10% of items with the lowest
  # absolute PCA loadings until the number of combinations is below the 'ceiling'.
  perform_optimisation <- function(current_cols, data, n.items, ceiling) {
    num_combs <- choose(length(current_cols), n.items)
    
    while (num_combs > ceiling) {
      
      # Run PCA on the remaining items
      # Note: Assumes 'psych' package is loaded and available
      pca_res <- psych::pca(data[, current_cols], nfactors = 1)
      
      # Find and rank items by loading
      item_rank <- order(abs(pca_res$loadings), decreasing = TRUE)
      
      # Calculate how many items to keep (top 90%)
      n_to_keep <- round(length(item_rank) * 0.9)
      
      # Get the indices of the top items
      top_item_indices <- item_rank[1:n_to_keep]
      
      # Update the list of columns to keep
      current_cols <- current_cols[top_item_indices]
      
      # Recalculate combinations for the next 'while' loop check
      num_combs <- choose(length(current_cols), n.items)
    }
    
    return(current_cols) # Return the final, reduced set of column indices
  }
  
  # --- Initial Setup ---
  cols <- 1:ncol(data) # Start with all column indices
  num_combinations <- choose(ncol(data), n.items)
  warning_msg <- paste("This will generate", format(num_combinations, big.mark = ","), 
                       "combinations and may be very slow.")
  
  # --- Handle Large Combinations & Optimisation ---
  # This block checks if the number of combinations is too large and handles
  # the 'optimise' logic (prompt, auto-run, or skip).
  if (num_combinations > ceiling) {
    
    # Case 1: optimise = NULL (default). Prompt the user.
    if (is.null(optimise)) {
      message(warning_msg)
      user_choice <- readline(prompt = "Enter 'c' to continue, 'o' for optimisation, or 'q' to quit: ")
      
      if (tolower(user_choice) == "c") {
        message("Continuing with the full process...")
      } else if (tolower(user_choice) == "o") {
        message("Running optimisation...")
        cols <- perform_optimisation(cols, data, n.items, ceiling)
      } else {
        stop("Process aborted by user.")
      }
      
      # Case 2: optimise = TRUE. Run optimisation automatically.
    } else if (optimise == TRUE) {
      message("Optimisation is enabled. Reducing item set...")
      cols <- perform_optimisation(cols, data, n.items, ceiling)
      
      # Case 3: optimise = FALSE. Show warning but continue.
    } else if (optimise == FALSE) {
      message(warning_msg)
    }
  }
  
  # Recalculate number of combinations after potential optimisation
  num_combinations_final <- choose(length(cols), n.items)
  
  # --- Data Preparation ---
  # Flip all items to be positively correlated with the most central item
  dc <- cor(data, use = "pairwise.complete.obs")
  most_central_item <- order(colSums(abs(dc), na.rm = TRUE), decreasing = TRUE)[1]
  items_to_flip <- dc[, most_central_item] < 0
  data[, items_to_flip] <- data[, items_to_flip] * -1
  data <- as.matrix(data) # Convert to matrix for faster subsetting
  
  
  # --- Batch Processing ---
  # This section is designed to handle a large number of combinations
  # without running out of memory.
  
  # Generate all combinations for the (potentially reduced) set of columns
  item_combinations <- combn(sort(cols), n.items)
  
  # Ensure num_combinations is accurate post-optimisation
  num_combinations <- ncol(item_combinations) 
  
  # Initialize a 'leaderboard' to store only the top N sets
  leaderboard <- data.frame(r = numeric(0), combination = character(0))
  
  # Define batch size for processing
  batch_size <- 10000 
  num_batches <- ceiling(num_combinations / batch_size)
  
  # Create the target vector once
  if (is.null(targ)) {
    targ <- rowMeans(data, na.rm = na.rm)
  }
  
  for (j in 1:num_batches) {
    
    # Define the start and end columns for the current batch
    start_col <- (j - 1) * batch_size + 1
    end_col <- min(j * batch_size, num_combinations)
    
    # Get the actual combinations for this batch
    current_batch_combs <- item_combinations[, start_col:end_col, drop = FALSE]
    
    # Calculate scores for all combinations in this batch
    scores_batch <- matrix(NA, ncol = ncol(current_batch_combs), nrow = nrow(data))
    for (i in 1:ncol(current_batch_combs)) {
      scores_batch[, i] <- rowMeans(data[, current_batch_combs[, i], drop = FALSE], na.rm = na.rm)
    }
    
    # Name the combinations (items or indices)
    if (item.names) {
      colnames(scores_batch) <- apply(current_batch_combs, 2, function(x) paste(colnames(data)[x], collapse = ","))
    } else {
      colnames(scores_batch) <- apply(current_batch_combs, 2, paste, collapse = ",")
    }
    
    # Correlate this batch's scores with the target
    cor_results_batch <- cor(scores_batch, targ, use = "pairwise")
    
    # Combine the new results with the existing leaderboard
    combined_results <- rbind(leaderboard, data.frame(r = cor_results_batch[,1], 
                                                      combination = rownames(cor_results_batch)))
    
    # Re-order and trim the leaderboard to keep only the top 'n.sets'
    leaderboard <- combined_results[order(abs(combined_results$r), decreasing = TRUE), ]
    leaderboard <- head(leaderboard, n.sets)
    
    # Update progress bar
    cat('\rProcessing Batch:', j, 'of', num_batches, 
        '| Total combinations:', format(num_combinations_final, big.mark = ","))
    flush.console()
  }
  
  # Clear the progress bar
  cat('\r', rep(' ', 80), '\n', sep = '')
  
  # Set row names of leaderboard to the combination string for output
  rownames(leaderboard) <- leaderboard$combination
  leaderboard <- leaderboard[, "r", drop = FALSE] # Keep as data.frame
  
  # --- Return Desired Output ---
  
  if (generate) {
    # Get the combination string from the leaderboard (e.g., "1,5,9")
    best_set_string <- rownames(leaderboard)[item.set]
    
    # Convert string to numeric indices
    best_set_indices <- as.numeric(strsplit(best_set_string, ',')[[1]])
    
    # Check if conversion failed (e.g., if item.names was TRUE)
    if (any(is.na(best_set_indices))) {
      stop(paste("Cannot use generate=TRUE with item.names=TRUE.",
                 "Please re-run with item.names=FALSE to generate scores.",
                 "Failed to parse:", best_set_string))
    }
    
    # Return the row means for the requested item set
    return(rowMeans(data[, best_set_indices, drop = FALSE], na.rm = na.rm))
  }
  
  if (r.sq) {
    # If r-squared is requested, calculate and bind it
    r_squared <- leaderboard$r^2
    colnames(r_squared) <- "R2"
    final_output <- cbind(leaderboard, r_squared)
    return(head(final_output, n.sets))
  } else {
    # Otherwise, return the correlations
    return(head(leaderboard, n.sets))
  }
}