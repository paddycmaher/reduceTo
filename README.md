# reduceTo: High-Performance Combinatorial Scale Optimisation

**Find the optimal unit-weighted subset of items from any item bank through ultra-fast exhaustive search.**

------------------------------------------------------------------------

## Overview

`reduceTo()` solves the combinatorial optimisation problem of selecting `n` items from a larger pool that maximise correlation with a target criterion. reduceTo can be used to shorten existing psychological scales, build diagnostic screeners, or derive scales based on an external criterion.

### Key Features

- **Exhaustive**: Tries every combination in the chosen search space to find the optimal item set
- **Rapid**: Parallelised C++ backend that scores combinations from precomputed column moments (a Gram matrix)
- **Scales**: For very large, intractable item reduction problems (100+ items), (optionally) triggers a cautious optimisation algorithm to reduce the search space to a manageable size
- **Robust**: Unit-weighting mitigates overfitting, and also is representative of common real-world sum-score usages
- **Production-Ready**: Built-in cross-validation option, progress tracking, robust error handling

------------------------------------------------------------------------

## Installation

``` r
# Install from GitHub
devtools::install_github("paddycmaher/reduceTo")
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
  target = diagnosis
)
```

------------------------------------------------------------------------

## Core Functionality

### Modes

**Parent Scale Preservation (Default)**

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

When exhaustive search becomes intractable, reduceTo narrows the search area first, before moving to **exhaustive search**, using **Synergy-Ranked Recursive Feature Elimination** (Synergistic RFE): exhaustively scores every combination at a small k, keeps the best-performing items, then grows k and repeats against the shrinking pool. This is computationally cheap because it relies on the same Gram-matrix approach, and no combinations are discarded without being scored.

``` r
# Choose 10 from 200 items (2.5 trillion combinations)
result <- reduceTo(data = large_item_bank, n.items = 10)
```

**Process:**

1\. Prefilter: drops any junk items far weaker than the strongest item by relevance (default: 5x)

2\. Synergistic RFE: exhaustively scores combinations at k = 2, 3, ... , ranking items by their best achieved score and dropping the weakest, until the remaining pool is small enough

3\. Exhaustive search: finds the best set in the refined pool by brute force

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

| Parameter | Description                                         | Default    |
|----------------|----------------------------------------|----------------|
| `data`    | Matrix/data.frame of item responses                 | *required* |
| `n.items` | Number of items in short form                       | *required* |
| `target`  | Target criterion (NULL = parent scale preservation) | `NULL`     |
| `n.sets`  | Number of top combinations to show in output        | `5`        |

### Optimisation

| Parameter | Description | Default |
|----------------|-----------------------------------------|----------------|
| `optimise` | If `TRUE`, runs Synergistic RFE to narrow the item pool via exhaustive small-k scoring when combinations exceed `ceiling`; if `FALSE`, forces exhaustive search regardless of `ceiling` | `TRUE` |
| `prefilter.ratio` | Before optimisation runs, drop items whose relevance is more than this many times weaker than the strongest item (set `Inf`/`NULL` to disable) | `5` |
| `ceiling` | Combination threshold for the final search | `10,000,000` |
| `rfe.budget` | Combination threshold per intermediate Synergistic RFE round (as opposed to `ceiling`, which bounds only the final search) | `10,000,000` |
| `opt.n` | Max rows to subsample during optimisation (speeds up large N) | `20000` |
| `speed` | `"fast"` mean-imputes missing data to score combinations via a Gram-matrix shortcut (reported statistics are always recomputed from the true data); `"conservative"` uses pairwise deletion throughout with no imputation | `"fast"` |

### Output

| Parameter | Description | Default |
|------------------------|----------------------------|--------------------|
| `item.names` | Return names vs indices | `FALSE` |
| `generate` | Compute scores for best set | `TRUE` |
| `cross.validate` | Enable train/holdout split | `FALSE` |
| `method` | Ranking metric (binary: `"r"`, `"youden_j"`, `"binarised_r"`, `"auc"`) | `NULL` |
| `show.progress` | Displays live progress bar and optimisation-stage updates | `TRUE` |
| `verbose` | Print informational messages (binary detection, optimisation triggers, prefilter notices, etc.) -- set `FALSE` to silence these while keeping `show.progress`'s live updates | `TRUE` |

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
  target = ability
)

# Synergistic RFE identifies the strongest ~45 items
# Exhaustive search on C(45, 10) = 3.2M combinations
# Total time: ~30 seconds
```

## Performance Benchmarks

### Gram-Matrix Engine vs. Row-Scan (isolated scoring speed)

Measured on a massive dataset (300 items, N = 300,000), scoring all C(300, 3) = 4,455,100 combinations directly against a continuous target, without optimisation. The same precomputed input fed to both engines, isolating the scoring step itself from pool-narrowing or other R-side overhead:

| Engine | Combinations/sec | Time for 4.45M combinations |
|-------------------------------------|------------------|------------------|
| `speed = "fast"` (Gram matrix) | 100M+/s | \~0.28s (incl. one-time Gram precompute) |
| `speed = "conservative"` (Row-scan algorithm) | \~9,190/s | \~8.1 min |

**\~1,800x faster** than a standard row-scan engine for this case. This is close to the ceiling case for the Gram matrix approach (small `n.items`, large N, since row-scan cost scales with N per combination while the Gram engine's is O(n.items\^2) regardless of N); real end-to-end runs below see smaller, but still large, gains once pool narrowing and R-side overhead are included.

### reduceTo() vs. Plain R

As a comparison, the "Base R Only" column below estimates the best-case scenario for base-R implementation (a vectorised per-combination `rowMeans()` + `cor()` loop), throughput-measured on a real sample of combinations and extrapolated to the full combination count. Assessed on real data, the IPIP-NEO Neuroticism scale (60 items, N = 5,000):

| Selecting | Combinations   | Base R Only  | reduceTo() (default settings) |
|-----------|----------------|--------------|-------------------------------|
| 3 of 60   | 34,220         | **~2.4 sec**    | **0.03s**                         |
| 5 of 60   | 5,461,512      | **~8.2 min**    | **0.06s**                         |
| 8 of 60   | 2,558,620,845  | **~3.7 days**   | **0.97s** (with optimisation)     |
| 10 of 60  | 75,394,027,566 | **~4.2 months** | **1.22s** (with optimisation)     |

Collectively, the C++ backend, Gram-matrix scoring, and Synergistic RFE let reduceTo turn a months-long base-R search into just over a second.

Your mileage will vary with your hardware and use case, but `reduceTo()` computes a live ETA.

## Methodological Notes

### Best Practices

- Garbage in, garbage out. Please, please ensure your parent scale is valid before shortening it
- Review the best 5-10 solutions and check whether items you think are 'bad' are being included
- Use cross-validation for smaller samples (N \< 500) where overfitting may be a concern

### Optimisation Reliability

The default optimisation settings were empirically calibrated in both real and deliberately-adversarial simulated datasets (200M+ combinations each). In practice, the heuristic approach only failed to find the true optimal item bundle in extreme scenarios, such as when an item's value is invisible unless combined with several (3+) other specific items. In these cases, the true solution could still be found by raising the `ceiling` value. Ordinary item structures (including simple weak/opposite-signed pairs) recovered the true optimum every time tested, across a wide range of `ceiling` settings.

### Binary Targets

- **Point-biserial r**: Equivalent to Cohen's d, correlates with AUC
- **Youden's J**: Maximises sensitivity + specificity - 1
- **Binarised r**: Applies optimal cutoff first, may find different solutions
- **AUC**: Threshold-independent; always reported alongside the others (`leaderboard$auc`, `binary_info$results$auc`) and selectable via `method = "auc"`

------------------------------------------------------------------------

## Citation

```         
Maher, P. (2026). reduceTo: High-Performance Combinatorial Scale Optimisation.
R package version 1.3.0. https://github.com/paddycmaher/reduceTo
```

------------------------------------------------------------------------

## Author

**Paddy Maher**\
Max Planck Institute for Human Development, MPRG Biosocial\
[paddycarstenmaher\@gmail.com](mailto:paddycarstenmaher@gmail.com)

------------------------------------------------------------------------

**License:** MIT \| **Version:** 1.3.0 \| **Updated:** August 2026
