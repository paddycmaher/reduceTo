#' Find Optimal Short-Form Scales
#'
#' @description
#' This function systematically evaluates all possible subsets of a given number of items
#' from a larger scale to find the combination that correlates most highly with a
#' target criterion (by default, the total score of the full scale). It includes
#' an initial step to automatically reverse-code items as needed based on their
#' correlation with the most central item.
#'
#' @param data A numeric matrix or data.frame containing the item responses.
#' @param n.items The desired number of items in the final short-form scale.
#' @param n.sets The number of top-performing item sets to return. Defaults to 5.
#' @param item.names Logical. If TRUE, the output will list the item names instead
#'   of their column numbers in the supplied matrix. Defaults to FALSE.
#' @param r.sq Logical. If TRUE, the squared correlation (R-squared) is returned
#'   alongside the correlation. Defaults to FALSE.
#' @param generate Logical. If TRUE, the function returns the computed scores for
#'   the single best item set instead of the correlation list. Defaults to FALSE.
#' @param item.set An integer specifying which item set to generate scores for
#'   (e.g., 1 for the best set, 2 for the second-best). Only used if `generate = TRUE`.
#' @param na.rm Logical. If TRUE, missing values are handled by pairwise deletion
#'   or row-wise removal in means. Defaults to TRUE.
#' @param targ An optional numeric vector to serve as the target criterion for the
#'   correlations. If NULL (the default), the mean score of the full scale is used.
#'
#' @return Depending on the arguments, either a data.frame listing the top `n.sets`
#'   of items and their correlation with the target, or a numeric vector of scores
#'   generated from the `item.set`.
#'
#' @author Paddy Maher, Goldsmiths, University of London. Contact: paddycarstenmaher@gmail.com
#'
#' @examples
#' # Create some sample data
#' set.seed(123)
#' sample_data <- as.data.frame(matrix(rnorm(200), ncol = 10))
#' colnames(sample_data) <- paste0("item_", 1:10)
#'
#' # Find the top 5 three-item scales
#' # reduceTo(sample_data, n.items = 3, n.sets = 5, item.names = TRUE)
#'

reduceTo <- function(data, n.items, n.sets = 5, item.names = FALSE, r.sq = FALSE, generate = FALSE, item.set = 1, na.rm = TRUE, targ = NULL) {
  
  # Warn user about performance for large number of combinations
  num_combinations <- choose(ncol(data), n.items)
  if (num_combinations > 50000) {
    warning(paste("This will generate", num_combinations, "combinations and may be very slow or exceed memory limits."))
  }
  
  # --- Data Preparation ---
  # Flip all items to be positively correlated with the most central item
  dc <- cor(data, use = 'pairwise.complete.obs')
  most_central_item <- order(colSums(abs(dc), na.rm = TRUE), decreasing = TRUE)[1]
  items_to_flip <- dc[, most_central_item] < 0
  data[, items_to_flip] <- data[, items_to_flip] * -1
  
  # --- Main Logic: Find All Combinations and Calculate Scores ---
  item_combinations <- combn(1:ncol(data), n.items)
  
  # Pre-allocate matrix for results
  comb_scores <- matrix(NA, nrow = nrow(data), ncol = num_combinations)
  
  # Generate mean scores across all item sets and participants
  for (i in 1:num_combinations) {
    comb_scores[, i] <- rowMeans(data[, item_combinations[, i]], na.rm = na.rm)
  }
  
  # --- Name the Results ---
  # Create informative names for each short scale
  combination_names <- apply(item_combinations, 2, paste, collapse = ",")
  if (item.names) {
    combination_names <- apply(item_combinations, 2, function(x) paste(colnames(data)[x], collapse = ","))
  }
  colnames(comb_scores) <- combination_names
  
  # --- Correlation with Target ---
  # If no target scale is provided, use the mean of the full scale as the target
  if (is.null(targ)) {
    targ <- rowMeans(data, na.rm = na.rm)
  }
  
  # Correlate each short scale with the target scale
  cor_results <- cor(comb_scores, targ, use = 'pairwise')
  
  # Order results by absolute correlation size
  ordered_results <- as.matrix(cor_results[order(abs(cor_results[, 1]), decreasing = TRUE), ])
  colnames(ordered_results) <- "r"
  
  # --- Return Desired Output ---
  if (generate) {
    # Find the specific item set requested
    best_set_name <- rownames(ordered_results)[item.set]
    # Return the generated scores for that set
    return(comb_scores[, best_set_name])
  }
  
  if (r.sq) {
    # If r-squared is requested, calculate and bind it
    r_squared <- ordered_results^2
    colnames(r_squared) <- "R2"
    final_output <- cbind(ordered_results, r_squared)
    return(head(final_output, n.sets))
  } else {
    # Otherwise, return the correlations
    return(head(ordered_results, n.sets))
  }
}
