# reduceTo: High-Performance Combinatorial Scale Optimisation

**Find the optimal subset of items from larger scales through parallelised exhaustive search with intelligent beam-search optimisation.**

An R package for psychometric scale reduction that guarantees finding the best-performing item combinations, with a memory-optimised C++ backend.

------------------------------------------------------------------------

## Overview

`reduceTo()` solves the combinatorial optimisation problem of selecting `n` items from a larger pool that maximise correlation with a target criterion. reduceTo can be used to shorten existing psychological scales, build diagnostic screeners, or derive scales based on an external criterion.

### Key Features

-   **Guarantees Optimality**: Exhaustive search within feasible space (no heuristic approximations)
-   **Blazing Fast**: Parallelised C++ backend with 8-bit compression processes 1M+ combinations/second
-   **Intelligent Scaling**: Automatic hybrid beam search for large item pools (100+ items)
-   **Production-Ready**: Built-in cross-validation, progress tracking, robust error handling

------------------------------------------------------------------------

## Installation

``` r
# Install from GitHub
devtools::install_github("paddycm/reduceTo")
```

**Requirements:** R ≥ 4.0, C++11 compiler, RcppParallel

------------------------------------------------------------------------

## Quick Start

``` r
library(reduceTo)

# Basic: Reduce 50-item scale to best 5 items
result <- reduceTo(data = my_scale_data, n.items = 5)

# With external criterion
result <- reduceTo(
  data = symptom_items,
  n.items = 8,
  target = diagnosis,
  cross.validate = TRUE
)

# Large item pool (automatic beam search)
result <- reduceTo(
  data = item_bank_200,
  n.items = 10,
  optimise = TRUE
)
```

------------------------------------------------------------------------

## Core Functionality

### Optimisation Modes

**Internal Consistency (Default)**

``` r
# Maximise correlation with full-scale total score
result <- reduceTo(data, n.items = 5)
```

**Criterion Validity**

``` r
# Maximise prediction of external variable
result <- reduceTo(data, n.items = 5, target = outcome_variable)
```

**Binary Classification**

``` r
# Optimise for diagnostic accuracy (auto-detected for 0/1 vectors)
diagnosis <- c(0, 1, 0, 1, 1, ...)
result <- reduceTo(data, n.items = 6, target = diagnosis)
# Returns: optimal cutoff, sensitivity, specificity, Youden's J
```

### Intelligent Optimisation for Large Pools

When exhaustive search becomes intractable, reduceTo employs **hybrid beam search → exhaustive**:

``` r
# Choose 10 from 200 items (2.5 trillion combinations)
result <- reduceTo(
  data = large_item_bank,
  n.items = 10,
  optimise = TRUE,
  beam.width = 2000,
  ceiling = 10000000
)
```

**Process:** 1. Prefilter: drops items far weaker than the strongest by relevance 2. Beam search: builds 3 → 4 → ... → 10 items, keeping the top combinations at each stage (`beam.width`, auto-scaled by default) 3. Item extraction: identifies the most promising items 4. Exhaustive search: guarantees optimum within refined pool

### Cross-Validation

``` r
result <- reduceTo(
  data = assessment_data,
  n.items = 7,
  target = clinical_outcome,
  cross.validate = TRUE    # 75/25 train/holdout split
)
```

------------------------------------------------------------------------

## Key Parameters

### Essential

| Parameter | Description                                    | Default    |
|-----------|------------------------------------------------|------------|
| `data`    | Matrix/data.frame of item responses            | *required* |
| `n.items` | Number of items in short form                  | *required* |
| `target`  | Target criterion (NULL = internal consistency) | `NULL`     |
| `n.sets`  | Number of top combinations to return           | `5`        |

### Optimisation

| Parameter         | Description                                                          | Default    |
|-------------------|------------------------------------------------------------------------|----------|
| `optimise`        | Enable beam search for large pools                                   | `TRUE`     |
| `prefilter.ratio` | Before beam search, drop items whose relevance is more than this many times weaker than the strongest item (set `Inf`/`NULL` to disable) | `5` |
| `beam.width`      | Top combinations kept per stage; `NULL` scales it to `ceiling` and pool size automatically | `NULL` |
| `ceiling`         | Combination threshold for optimisation                               | `500,000`  |
| `opt.n`           | Max rows for beam search (speeds up large N)                         | `5000`     |
| `speed`           | `"fast"` mean-imputes missing data to score combinations via a Gram-matrix shortcut (reported statistics are always recomputed from the true data); `"conservative"` uses pairwise deletion throughout with no imputation | `"fast"` |

### Output

| Parameter | Description | Default |
|------------------------|----------------------------|--------------------|
| `item.names` | Return names vs indices | `FALSE` |
| `generate` | Compute scores for best set | `TRUE` |
| `cross.validate` | Enable train/holdout split | `FALSE` |
| `method` | Ranking metric (binary: `"r"`, `"youden_j"`, `"binarised_r"`) | `NULL` |

------------------------------------------------------------------------

## Return Object

``` r
result <- reduceTo(data, n.items = 5, target = outcome)

result$output              # Top n.sets combinations with metrics
result$best_indices        # Column numbers: c(3, 7, 12, 18, 24)
result$best_names          # Column names: c("item3", "item7", ...)
result$scores              # Computed sum scores
result$binary_info         # Cutoff, sensitivity, specificity (binary only)
```

------------------------------------------------------------------------

## Examples

### Basic Scale Reduction

``` r
# Find best 5-item short form from 20-item scale
result <- reduceTo(
  data = personality_scale,
  n.items = 5,
  item.names = TRUE
)

print(result)
# Selected items: Item_7, Item_12, Item_3, Item_18, Item_15
# Correlation with full scale: r = 0.94
```

### Clinical Screening Tool

``` r
# Optimise 12-item depression screener for diagnosis
result <- reduceTo(
  data = depression_items,
  n.items = 12,
  target = diagnosis,
  cross.validate = TRUE,
  method = "youden_j"
)

# Review performance
print(result$binary_info)
# Optimal cutoff: 7
# Sensitivity: 0.91, Specificity: 0.88
# Holdout Youden's J: 0.79

# Use in practice
scores <- result$scores
predictions <- ifelse(scores >= 7, "Likely", "Unlikely")
```

### Large Item Bank

``` r
# Select 10 from 150 items (5.9 × 10^15 combinations!)
result <- reduceTo(
  data = item_bank_150,
  n.items = 10,
  target = ability,
  optimise = TRUE,
  beam.width = 3000
)

# Beam search identifies ~45 promising items
# Exhaustive on C(45, 10) = 3.2M combinations
# Total time: ~30 seconds
```

## Performance Benchmarks

Measured on N = 5,000 participants (small cases run directly; large cases use `reduceTo()`'s own on-machine calibrated estimate, since running them without optimisation isn't feasible):

| Problem    | Combinations       | Without Optimisation | With Beam Search (default settings) |
|------------|---------------------|-----------------------|--------------------------------------|
| C(20, 3)   | 1,140               | 0.0s (measured)       | — (below `ceiling`, runs directly)   |
| C(30, 5)   | 142,506             | 0.3s (measured)       | — (below `ceiling`, runs directly)   |
| C(40, 10)  | 847,660,528         | \~40 min -- 3 hrs     | 6.0s                                 |
| C(100, 10) | 17.3 trillion       | \~1 -- 7 years        | 9.7s                                 |
| C(200, 10) | 22.5 quadrillion    | \~2,088 -- 10,443 years | 13.1s                              |

Actual times depend on your hardware, sample size, and missingness (see `speed` above); `reduceTo()` prints a calibrated estimate for your own machine and dataset before falling back to beam search.

## Methodological Notes

### Best Practices

-   Review top 5-10 solutions, not just #1 - similar performance allows choosing more theoretically valid items
-   Garbage in, garbage out. Ensure your parent scale is valid
-   Use cross-validation for small samples (N \< 500)

### Binary Targets

-   **Point-biserial r**: Equivalent to Cohen's d, correlates with AUC
-   **Youden's J**: Maximises sensitivity + specificity - 1
-   **Binarised r**: Applies optimal cutoff first, may find different solutions

------------------------------------------------------------------------

## Citation

```         
Maher, P. (2026). reduceTo: High-Performance Combinatorial Scale Optimisation.
R package version 1.2.0. https://github.com/paddycmaher/reduceTo
```

------------------------------------------------------------------------

## Author

**Paddy Maher**\
Max Planck Institute for Human Development, MPRG Biosocial\
[paddycarstenmaher\@gmail.com](mailto:paddycarstenmaher@gmail.com)

------------------------------------------------------------------------

**License:** MIT \| **Version:** 1.2.0 \| **Updated:** July 2026
