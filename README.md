# reduceTo: Exact High-Performance Combinatorial Scale Reduction

**Find the exact optimal unit-weighted subset of items from any item set through an ultra-fast exhaustive search.**

------------------------------------------------------------------------

## Overview

`reduceTo()` solves the combinatorial optimisation problem of selecting the exact set of *`n`* items from any item pool that maximises correlation with a selected target criterion. reduceTo can be used to shorten existing psychological scales, build diagnostic screeners, or derive scales from item banks based on an external criterion.

### Key Features

- **Exhaustive**: Tests every combination in the chosen search space to find the optimal item set
- **Rapid**: Parallelised C++ backend that scores combinations from precomputed column moments (a Gram matrix)
- **Scales**: For very large, intractable item reduction problems, (optionally) runs a knockout search to find the best solutions algorithmically
- **Robust**: Unit-weighting mitigates overfitting, and also is representative of common real-world sum-score usages
- **Production-Ready**: Built-in cross-validation option, progress tracking, robust error handling
- **User Friendly**: Automatic data preprocessing, auto-detection of reverse-keyed items, auto-detection and reporting for binary outcomes, simple UI

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
------------------------------------------------------------------------

## Shortening Scales for Validity Preservation

A scale's central axis, i.e. the average vector of all its items, is its 'centroid' (Little, Lindenberger & Nesselroade, 1999), a vector that defines which specific dimension of human psychology it captures.

The similarity of any two scales is determined by the cosine of the angle between their centroids: that is, their Pearson correlation, *r*. Whenever shortening a scale with the goal of preserving the parent scale's validity, maximising *r* is by definition the optimal approach: any other criterion may distort the psychological construct captured by the scale by more than is necessary, discarding signal and reorienting the centroid down a new, different dimension.

A short form might achieve a high correlation with its parent scale (*r*<sub>parent</sub>) because its items are all perfectly parallel to the parent's centroid, or (more commonly) because its items balance one other – like the legs of a chair, or the struts and walls in a house. This counterbalancing means that, rather than some items being statically 'good' or 'bad', certain item sets work well together, and others do not. Additionally, just as in stool design, it means that the best three-item solution does not necessarily share any features with the best four-item solution. Items need to be weighed dynamically, in combination with one another, rather than on their individual merits.

## The Algorithm

### Exhaustive Search

Because maximisation of *r* is a problem with a verifiable solution, optimally reducing a psychometric scale to any given shorter length is a straightforward computational task. reduceTo therefore approaches scale reduction by assessing every single possible combination of *n* items, and identifying the set that has the highest *r*<sub>parent</sub>.

Small reductions are computationally trivial, but calculation size explodes as scale lengths increase. Reducing 12 items to 6 requires searching 924 combinations; but reducing 24 to 12 requires 2.7 million. However, modern CPUs can carry out billions of calculations a second for algorithms written in compiled languages. For this reason, reduceTo runs pre-processing in R, but hands off the hard part to a parallelised C++ engine. This engine precomputes a Gram matrix from which to evaluate item sets, rather than using the raw data, dramatically increasing efficiency, and making runtime effectively independent of sample size. This alone would make it possible to, with some patience, find exact solutions for most scale reduction problems a psychometrician could encounter.

However, beyond raw speed, reduceTo additionally does not need to inspect every combination by hand. Because it builds item sets up one step at a time (and because it starts with the most promising items first), it is often possible to prove mathematically that no remaining combination of items from the current branch can reach the current top 100. In these cases, the engine cuts this branch, often discarding millions of combinations that were not worth inspecting individually. This gives identical results to a manual exhaustive search, but in some cases reduces the search space by 99.9%, speeding up computation even further, and making almost any realistic problem tractable.

### Knockout Search

reduceTo's exhaustive search backend is extremely fast, running at tens of billions of operations a second. However, for massive problems where exhaustive search isn't tractable, reduceTo automatically switches to a **Knockout search**.

To see which walls an old house needs most, you could (in theory) remove each one in turn and measure how the house slumps. reduceTo knocks items out of the source scale in the same way, assessing how each knockout moves the scale's centroid. This is used to walk directly to a first answer, which is rarely the true solution; from this starting point, nearby combinations are explored to ensure no better answers are missed.

1\. Iterative knockout: Starting from the full scale, knock out the item whose loss moves the centroid least, and repeat until `n.items` remain.

2\. Scan: For every item across the entire pool, assess how much swapping them in/out of the form, either alone or along with one other item, would lower the short form's *r*. Items are ranked from lowest-impact changes (i.e. least confidence about whether the house really needs this wall or not) to highest-impact.

3\. Search: Starting at the top of this list, score as many combinations as `knockout.budget` (`ceiling / 1000` by default) allows, holding the other items fixed, with the same Gram-matrix engine as the exhaustive search. If an improved short form is found during this process, repeat the scan & search process from this starting point.

4\. Leave One Candidate Out (LOCO): Steps 1-3 identify the strongest item set in many cases, but can fall into a local optimum, missing a better solution that's just out of reach. To force further exploration, each item of the current best short form is knocked out in turn, and steps 2 and 3 are repeated with that item completely banned.* 

5\. Sweepdown: Finally, although they almost always do in practice, larger knockout budgets do not *automatically* find better solutions than smaller ones. For the sake of any edge cases, a very cheap additional step happens, in which the whole process (steps 1-4) is repeated at smaller budgets, sweeping through every order of magnitude from the knockdown budget down to 10<sup>1</sup>.

The best 100 short forms are added to a leaderboard, and reported to the user.

**(Note: When very long scales are reduced by less than 50% but more than 20 items, each left-out item is instead forced back into the scale (Put One Candidate In - POCI), as opposed to an item from the scale being forced out.)*

### Cross-Validation

Because reduced scales are sum-scored, overfitting is not generally a significant issue for reduceTo-reduced scales. However, for very large item reductions in small samples, where derived scales may suffer from winner's curse, reduceTo contains an option to split the data into a discovery and holdout sample. Results in both are reported, giving an estimate of the degree of overfitting. Holdout *r* values are representative of real-world performance, and should be reported in your write up. Default split is set at 75/25, but this is adjustable with e.g. `cross.validate = 0.6` for a 60/40 split.

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

In the order they appear in `reduceTo()`: the essentials first, then output, data handling, search and console settings.

### Essential

| Parameter | Description | Default |
|------------------------|------------------------|------------------------|
| `data` | Matrix/data.frame of item responses | *required* |
| `n.items` | Number of items in short form | *required* |
| `target` | Target criterion (NULL = parent scale preservation) | `NULL` |
| `method` | Ranking metric (binary: `"r"`, `"youden_j"`, `"binarised_r"`, `"auc"`) | `NULL` |
| `cross.validate` | Enable train/holdout split | `FALSE` |

### Output

| Parameter    | Description                                  | Default |
|--------------|----------------------------------------------|---------|
| `n.sets`     | Number of top combinations to show in output | `5`     |
| `item.names` | Return names vs indices                      | `FALSE` |
| `r.sq`       | Also report R²                               | `FALSE` |
| `generate`   | Compute scores for the selected set          | `TRUE`  |
| `item.set`   | Which ranked set to generate scores for      | `1`     |

### Data handling

| Parameter    | Description                                | Default |
|--------------|--------------------------------------------|---------|
| `na.rm`      | Handle missing values by pairwise deletion | `TRUE`  |
| `scale.vars` | Standardise every item before scoring      | `FALSE` |

### Search

| Parameter | Description | Default |
|------------------------|------------------------|------------------------|
| `speed` | `"fast"` mean-imputes missing data to score combinations via a Gram-matrix shortcut (reported statistics are always recomputed from the true data); `"conservative"` uses pairwise deletion throughout with no imputation (recommended with `na.rm = FALSE` and substantial or patterned missingness) -- in a knockout search, it also scores every combination of the final closest calls directly | `"fast"` |
| `ceiling` | Combination threshold triggering a knockout search | `1,000,000,000,000` (`10,000,000` with `speed = "conservative"`) |
| `optimise` | If `NULL`, runs a knockout search when combinations exceed `ceiling`; `TRUE` runs one for any problem; `FALSE` forces exhaustive search regardless of `ceiling` | `NULL` |
| `knockout.budget` | The most combinations each of the knockout search's searches scores; `NULL` uses `ceiling / 1000` | `NULL` |
| `prefilter.ratio` | Before a knockout search, drop items whose relevance is more than this many times weaker than the strongest item, when at least twice `n.items` survive. | `Inf` |

### Console

| Parameter | Description | Default |
|------------------------|------------------------|------------------------|
| `verbose` | Print informational messages (binary detection, optimisation triggers, prefilter notices, etc.) -- set `FALSE` to silence these while keeping `show.progress`'s live updates | `TRUE` |
| `show.progress` | Displays live progress bar and optimisation-stage updates | `TRUE` |

------------------------------------------------------------------------

## Return Object

``` r
result <- reduceTo(data, n.items = 5, target = outcome)

result$r                   # The selected set's correlation with the target (and r_holdout, auc, cutoff ...)
result$alpha               # Its Cronbach's alpha
result$r_parent            # Its correlation with the full scale
result$selected            # The set: items, indices, item_r (item-target r), keys (-1 = reverse-keyed)
result$top                 # Top n.sets combinations with metrics (result$leaderboard: the top 100)
result$item_stats          # Per selected item: key, mean, sd, r_target, r_rest, alpha_if_deleted
result$scores              # Computed sum scores
result$binary              # Cutoff and classification metrics (binary targets only)
result$search              # "exhaustive" (guaranteed best) or "optimised" (heuristic), its searches, and LOCO's results
result$settings            # The arguments used (plus result$call, $version, $seconds, $timings)
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
# Select 10 from 150 items (1.2 × 10^15 combinations!)
result <- reduceTo(
  data = item_bank_150,
  n.items = 10,
  target = ability
)

# The knockout search trims the bank to 10 items, searches every combination of
# the closest calls (up to 1 billion at a time), then again with each item knocked out
# Total time: under a second (0.5s for 10 of 150 IPIP items, N = 5,000)
```

## Performance Benchmarks

### Gram-Matrix Engine vs. Row-Scan (isolated scoring speed)

Measured on a massive dataset (300 items, N = 300,000), scoring all C(300, 3) = 4,455,100 combinations directly against a continuous target, without optimisation. The same precomputed input fed to both engines, isolating the scoring step itself from the knockout search or other R-side overhead:

| Engine | Combinations/sec | Time for 4.45M combinations |
|------------------------|------------------------|------------------------|
| `speed = "fast"` (Gram matrix) | 1.6B+/s | \~0.29s |
| `speed = "conservative"` (Row-scan algorithm) | \~9,800/s | \~7.6 min |

**\~1,600x faster** than a standard row-scan engine for this case. This is close to the ceiling case for the Gram matrix approach (small `n.items` and large *N*, since row-scan cost scales with *N* per combination while the Gram engine's is roughly constant per combination, regardless of *N* or `n.items`).

### reduceTo() vs. Plain R

As a comparison, the "Base R Only" column below estimates the best-case scenario for base-R implementation (a vectorised per-combination `rowMeans()` + `cor()` loop), throughput-measured on a real sample of combinations and extrapolated to the full combination count. Assessed on real data, the IPIP-NEO Neuroticism scale (60 items, N = 5,000); reduceTo times are medians of 15 runs on an Apple M2 Pro:

| Selecting | Combinations       | Base R Only      | reduceTo() (default settings) |
|------------------|------------------|------------------|-------------------|
| 3 of 60   | 34,220             | **\~2.4 sec**    | **0.01s**                     |
| 5 of 60   | 5,461,512          | **\~8.2 min**    | **0.01s**                     |
| 8 of 60   | 2,558,620,845      | **\~3.7 days**   | **0.04s** (exhaustive)        |
| 10 of 60  | 75,394,027,566     | **\~4.2 months** | **0.60s** (exhaustive)        |
| 12 of 60  | 1,399,358,844,975  | **\~9 years**    | **0.60s** (knockout search)   |
| 15 of 60  | 53,194,089,192,720 | **\~400 years**  | **0.60s** (knockout search)   |

Collectively, the C++ backend, Gram-matrix scoring, branch-cutting, and knockout search let reduceTo shorten what might be a months-long base-R search to seconds.

Your exact mileage will vary with your hardware and use case, but reduceTo also computes and displays a live ETA.

## Methodological Notes

### Optimisation Reliability

Benchmarked against exhaustive search on 2,211 problems with known top-50 sets (real IPIP-NEO, MEAQ, clinical and loneliness item pools of 20-62 items, single- and multi-construct, internal and external targets, plus synthetic synergy, suppressor and method-factor traps; up to 2.5 x 10\^11 combinations):

| Ceiling | Problems needing a knockout search | True solution ranked first | True top 5 recovered | True top 50 recovered |
|---------------|---------------|---------------|---------------|---------------|
| 10\^8 | 765 | 100% | 99.9% | 99.9% |
| 10\^6 | 1,678 | 99.9% | 99.8% | 99.5% |

The default ceiling is now 10\^12: problems up to a trillion combinations are searched exhaustively, and each knockout search scores up to 10\^9 combinations (`ceiling / 1000`), 100 times more than in the benchmark above, where each search scored up to 10\^7 (10\^8 row) or 10\^5 (10\^6 row).

### Binary Targets

- **Point-biserial r**: Equivalent to Cohen's d, correlates with AUC
- **Youden's J**: Maximises sensitivity + specificity - 1
- **Binarised r**: Applies optimal cutoff first, may find different solutions
- **Cutoffs**: Reported on the conventional sum-score scale - reverse-keyed items are scored (min + max) - x, e.g. 6 - x for 1-5 items
- **AUC**: Threshold-independent; always reported alongside the others (`leaderboard$auc`, `binary_info$results$auc`) and selectable via `method = "auc"`

### Best Practices

- Garbage in, garbage out. Please, please ensure your parent scale is actually valid before you reduce it
- Use your own judgement. Review the best solutions and check whether any items you think are 'bad' are being included; if they are, this indicates a problem with the parent scale
- Consider cross-validation for smaller samples (*N* \< 300) where overfitting may be a concern

## FAQs

- Don't Gram matrices require non-missing data?
*Yes, they do. The engine runs on mean-imputed data to generate the top-100 list. These, however, are always re-scored at the very end*

------------------------------------------------------------------------

## Citation

```         
Maher, P. (2026). reduceTo: High-Performance Combinatorial Scale Optimisation.
R package version 1.7.2. https://github.com/paddycmaher/reduceTo
```

------------------------------------------------------------------------

## Author

**Paddy Maher**\
Max Planck Institute for Human Development, MPRG Biosocial\
[paddycarstenmaher\@gmail.com](mailto:paddycarstenmaher@gmail.com)

------------------------------------------------------------------------

**License:** MIT \| **Version:** 1.7.2 \| **Updated:** October 2026
