# Derive bootstrap method for a statistic

Infers whether a user-supplied indicator function (`fun`) behaves as a
**group-specific** statistic (computed independently within each
category) or a **whole-cube** statistic (requiring pooled information
across all categories).

## Usage

``` r
derive_bootstrap_method(
  df,
  fun,
  ...,
  cat_var,
  min_cat = 2,
  max_cat = 5,
  index = -1
)
```

## Arguments

- df:

  A dataframe.

- fun:

  A function which, when applied to `df` returns the statistic(s) of
  interest. This function must return a dataframe with a column
  `diversity_val` containing the statistic of interest.

- ...:

  Additional arguments passed on to `fun`.

- cat_var:

  A character vector specifying the grouping variable(s) used by `fun`.
  The function `fun(df, ...)` should return a row per group.

- min_cat:

  Integer. The minimum number of categories to include in the "short"
  dataset used for comparison. Defaults to `2`.

- max_cat:

  Integer. The maximum number of categories to include in the "long"
  dataset. Defaults to `5`.

- index:

  Integer. Position from which the `max_cat` categories will be selected
  for processing. Use `-1` (default) use the final `min_cat` and
  `max_cat` categories in the dataset.

## Value

A single character string: either

- `"group_specific"` or

- `"whole_cube"`

indicating the appropriate bootstrapping strategy.

## Details

The function evaluates bootstrap method by comparing the statistic
computed on two filtered subsets of the data:

- **Short subset**: contains `min_cat` consecutive categories.

- **Long subset**: contains `max_cat` consecutive categories.

Both subsets end at the same category index to ensure overlap.

The indicator function is evaluated on both subsets. If the indicator
values for the overlapping categories match exactly, the function is
considered **group-specific**. Otherwise, it is assumed to require
**whole-cube** bootstrapping.

## See also

Other indicator_uncertainty_helper:
[`boot_list_to_dataframe()`](https://b-cubed-eu.github.io/dubicube/reference/boot_list_to_dataframe.md),
[`bootstrap_cube_raw()`](https://b-cubed-eu.github.io/dubicube/reference/bootstrap_cube_raw.md),
[`calculate_boot_ci_from_boot()`](https://b-cubed-eu.github.io/dubicube/reference/calculate_boot_ci_from_boot.md),
[`resolve_bootstrap_method()`](https://b-cubed-eu.github.io/dubicube/reference/resolve_bootstrap_method.md)

## Examples

``` r
# 1. Calculate mean per group
sepal_length_per_species <- function(x, f) {
  out_df <- aggregate(Sepal.Length ~ Species, x, f)
  names(out_df) <- c("Species", "diversity_val") # Rename columns
  return(out_df)
}
sepal_length_per_species(iris, mean)
#>      Species diversity_val
#> 1     setosa         5.006
#> 2 versicolor         5.936
#> 3  virginica         6.588

# The mean is calculated per species, so we expect the group-specific method
derive_bootstrap_method(
  df = iris,
  fun = sepal_length_per_species,
  cat_var = "Species",
  min_cat = 1,
  max_cat = 3,
  mean
)
#> [1] "group_specific"

# 2. Calculate indicator based on whole dataset
sepal_length_per_species2 <- function(x, f) {
  out_df <- aggregate(Sepal.Length ~ Species, x, f)
  out_df$Sepal.Length <- out_df$Sepal.Length / nrow(out_df)
  names(out_df) <- c("Species", "diversity_val") # Rename columns
  return(out_df)
}
sepal_length_per_species2(iris, mean)
#>      Species diversity_val
#> 1     setosa      1.668667
#> 2 versicolor      1.978667
#> 3  virginica      2.196000

# Now we expect the whole-cube method
derive_bootstrap_method(
  df = iris,
  fun = sepal_length_per_species2,
  cat_var = "Species",
  min_cat = 1,
  max_cat = 3,
  mean
)
#> [1] "whole_cube"
```
