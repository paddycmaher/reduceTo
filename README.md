# reduceTo: Optimal Short-Form Scale Construction

An R function for systematically finding the best subset of items from a larger scale through exhaustive combinatorial search with intelligent optimizations.

## Overview

`reduceTo()` evaluates all possible combinations of items to find those that best correlate with a target criterion. It's designed for psychometric applications where you need to create shorter versions of existing scales while maximizing validity.

## Key Features

- **Exhaustive Search**: Evaluates all possible item combinations to find the optimal subset
- **Dual Optimization Modes**:
  - Scale Preservation (default): Maximize correlation with parent scale total score
  - Criterion Validity: Maximize prediction of external variables
- **Smart Optimizations**: Handles very large item pools through iterative pruning
- **Cross-Validation**: Built-in k-fold validation to assess generalizability
- **Memory Efficient**: Batch processing for handling millions of combinations
- **Flexible Targets**: Works with continuous, binary, or no external criteria

## Installation

```r
# Source the function
source("reduceTo.R")

# Required dependencies
library(psych)  # Only needed if using optimization
library(MASS)   # For example simulations
```

## Basic Usage

```r
# Reduce a 10-item scale to the best 3 items
result <- reduceTo(data = my_data, n.items = 3)

# View results
print(result)

# Access best items
result$best_indices  # Column numbers
result$best_names    # Column names
```

## Function Arguments

### Core Parameters

- **`data`**: Matrix or data.frame of item responses
- **`n.items`**: Number of items desired in short form
- **`n.sets`**: Number of top-performing sets to return (default: 5)

### Target Specification

- **`targ`**: Target criterion vector
  - `NULL` (default): Uses full scale mean score
  - Numeric vector: External criterion (e.g., clinical severity)
  - Binary vector (0/1): Discriminant validity optimization

### Output Options

- **`item.names`**: Return item names instead of indices (default: FALSE)
- **`r.sq`**: Include R² alongside correlation (default: FALSE)
- **`generate`**: Return computed scores for best set (default: FALSE)
- **`item.set`**: Which ranked set to generate scores for (default: 1)

### Validation & Optimization

- **`cross.validate`**: Perform 5-fold cross-validation (default: FALSE)
  - Recommended for N < 1000
  - Re-ranks results by holdout performance
- **`optimise`**: Control heuristic pruning for large pools
  - `NULL` (default): Prompt user when combinations exceed ceiling
  - `TRUE`: Automatically optimize if needed
  - `FALSE`: Force exhaustive search
- **`ceiling`**: Combination threshold for optimization (default: 1,000,000)
- **`factors`**: Number of PCA factors for optimization (default: 1)

### Data Preprocessing

- **`scale.vars`**: Standardize all columns (default: FALSE)
- **`na.rm`**: Handle missing data via pairwise deletion (default: TRUE)

## Examples

### Example 1: Basic Scale Reduction

```r
# Create simulated data
set.seed(42)
loadings <- c(rnorm(10, 0.75, 0.08) * sign(rnorm(10)))
target_cor_matrix <- outer(loadings, loadings)
diag(target_cor_matrix) <- 1
data <- MASS::mvrnorm(n = 200, mu = rep(0, 10), 
                      Sigma = target_cor_matrix, empirical = TRUE)
data <- as.data.frame(data)
colnames(data) <- paste0("item_", 1:10)

# Find best 3-item version
result <- reduceTo(data, n.items = 3)
```

### Example 2: External Criterion Validation

```r
# Optimize for predicting depression diagnosis
diagnosis <- c(0, 1, 1, 0, 1, ...)  # Binary outcome

result <- reduceTo(
  data = depression_scale,
  n.items = 5,
  targ = diagnosis,
  cross.validate = TRUE
)
```

### Example 3: Generate Scores

```r
# Get scores for the best item set
result <- reduceTo(
  data = my_data,
  n.items = 4,
  generate = TRUE,
  item.names = TRUE
)

# Use the scores in subsequent analyses
short_form_scores <- result$scores
```

## Return Object

The function returns a `reduced_scale` object containing:

- **`output`**: Data frame of top n.sets with correlations
- **`leaderboard`**: Extended ranking of top 100 combinations
- **`item_cors`**: Individual item correlations for each set
- **`best_indices`**: Column numbers of top-ranked items
- **`best_names`**: Column names of top-ranked items
- **`best_item_cors`**: Correlations for individual items in best set
- **`scores`**: Computed scores (if `generate = TRUE`)
- **`params`**: List of function parameters used

## How It Works

### Standard Mode (Small Item Pools)

1. Generates all possible combinations of n.items from the full pool
2. Computes sum scores for each combination
3. Correlates each combination with target criterion
4. Ranks combinations by absolute correlation
5. Returns top performers

### Optimization Mode (Large Item Pools)

When combinations exceed the ceiling threshold, the function uses iterative pruning:

**For Internal Consistency (targ = NULL)**:
- Performs PCA on item pool
- Ranks items by maximum loading across factors
- Progressively removes weakest items until combinations are manageable

**For External Criteria (targ provided)**:
- Calculates item-criterion correlations
- Ranks items by absolute correlation
- Retains strongest predictors until combinations drop below ceiling

### Cross-Validation Process

When `cross.validate = TRUE`:

1. Splits data into 5 random folds
2. For each candidate set in leaderboard:
   - Calculates scores in each test fold
   - Correlates with held-out criterion values
3. Re-ranks by average cross-validated correlation
4. Penalizes overfitting

## Performance Notes

- **Memory Usage**: Uses batch processing to handle millions of combinations
- **Speed**: ~1,000,000 combinations can be processed in seconds
- **Sampling**: Optimization uses max 20,000 rows for speed
- **Progress**: Shows progress bar for multi-batch operations

## When to Use Each Feature

**Cross-Validation**: Essential for small samples (N < 1000) or when overfitting is a concern

**Optimization**: Necessary when item pool is large (e.g., 30+ items with n.items < 10)

**External Criterion**: Use when short form serves specific predictive purpose rather than just scale abbreviation

**Binary Target**: Particularly useful for diagnostic cutoff optimization or maximizing discriminant validity

## Methodological Notes

- Binary targets effectively maximize point-biserial correlation, functionally similar to IRT Information maximization at a threshold
- The function respects item directionality in the data (no automatic reverse-scoring)
- Combinations that produce no variance are automatically filtered
- Missing data handled via pairwise deletion in correlations

## Author

Paddy Maher  
Goldsmiths, University of London  
Contact: paddycarstenmaher@gmail.com

## Validation and Quality Assurance

The function includes:
- Robust column validation (numeric, non-constant, finite values)
- Protection against constant/infinite values causing correlation errors
- Automatic handling of tibble/data.frame formats
- Safe subset sampling during optimization
- Memory-efficient batch processing architecture

## Tips for Best Results

1. **Start Conservative**: Begin with reasonable n.items (not too small)
2. **Check Item Loadings**: Ensure items tap the same construct
3. **Use Cross-Validation**: Especially important for small samples
4. **Examine Multiple Sets**: Review top 5-10 solutions, not just #1
5. **Consider Theory**: Statistical optimality should align with construct theory
6. **Validate Externally**: Test selected items in independent samples

## License

This function is provided as-is for research and applied use in scale development and psychometric applications.