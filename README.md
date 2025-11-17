# **R Function: reduceTo for Optimal Short-Scale Derivation**

This repository contains the R code for the reduceTo function, a tool for psychometric analysis designed to identify the optimal subset of items for creating a short-form scale.

**Description**

In psychometric research, there is often a need to shorten existing scales while retaining the maximum possible psychometric information. The reduceTo function automates this process by systematically evaluating all possible subsets of a given size from a larger pool of items. It identifies the item combination that correlates most highly with a specified criterion, which defaults to the total score of the full scale.

**The function includes two key features:**

**Automated Item Reversal:** It first inspects the correlation matrix to identify the most central item and automatically reverse-codes any items that are negatively correlated with it, ensuring all items are scored in the same direction.

**Systematic Evaluation:** It generates every possible combination of items for the desired short-scale length, computes a score for each combination, and correlates these scores with a target criterion to rank them by validity.

**Installation / Usage**

No external packages are required to use this function; it relies entirely on base R.

Source the Function: Download the reduceTo.R file from this repository and load it into your R session using the source() command:

**source("path/to/your/folder/reduceTo.R")**

**Run the Function:** Call the function on your data, specifying the data frame and the desired number of items for the short scale.

# **Example**

Here is a reproducible example using simulated data.

**1. Load the function**

source("reduceTo.R")

**2. Create some sample data**

**Let's simulate a 10-item scale administered to 200 people. We'll make items 1-6 correlate positively, and items 7-10 correlate negatively.**

set.seed(123)

n_people <- 200

n_items <- 10

**Create a latent "true score" for each person**

true_score <- rnorm(n_people, mean = 0, sd = 1)

**Generate item responses based on the true score**

item_data <- as.data.frame(replicate(n_items, true_score + rnorm(n_people, mean = 0, sd = 0.8)))

colnames(item_data) <- paste0("item_", 1:10)

**Flip items 7-10 to be negatively correlated**

item_data[, 7:10] <- item_data[, 7:10] * -1

**3. Use the function to find the best 3-item scale**

**The function should identify that items 7-10 need to be flipped back, and then find the combination of 3 items that best correlates with the total score.**

best_scales <- reduce.to(

  data = item_data,
  
  n.items = 3,       # We want a 3-item scale
  
  n.sets = 5,        # Show us the top 5 best combinations
  
  item.names = TRUE, # Show the item names, not just numbers
  
  r.sq = TRUE        # Also show the R-squared value
  
)

**4. View the results**

print(best_scales)

**The output will show a ranked list of the best 3-item combinations and their correlation (r) and variance explained (r2) with the full 10-item scale score.**

**How to Cite**

If you use this function in your research, please cite the software using the DOI provided by Zenodo. This ensures that the method is transparent and reproducible.

Maher, P (2025). reduceTo: An R function for optimal short-scale derivation (Version v1.1.0) [Computer software]. Zenodo. https://doi.org/10.5281/zenodo.15764037

**License**
This project is licensed under the MIT License. You are free to use, modify, and distribute this software for any purpose, provided the original copyright and permission notice are included.
