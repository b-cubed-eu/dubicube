# Resolve bootstrap method including use of the boot package

Resolves the effective bootstrap method to be used by
[`bootstrap_cube()`](https://b-cubed-eu.github.io/dubicube/reference/bootstrap_cube.md),
combining:

## Usage

``` r
resolve_bootstrap_method(
  df,
  fun,
  ...,
  cat_var,
  ref_group = NA,
  method = "smart"
)
```

## Arguments

- df:

  A dataframe.

- fun:

  A function which, when applied to `df`, returns the statistic(s) of
  interest. This function must return a dataframe with a column
  `diversity_val`.

- ...:

  Additional arguments passed to `fun`.

- cat_var:

  A character vector specifying the grouping variable(s) used by `fun`.

- ref_group:

  A value indicating the reference group. If `NA` (default),
  bootstrapping may be delegated to the boot package.

- method:

  Character string specifying the bootstrap method. One of
  `"whole_cube"`, `"group_specific"`, `"boot_whole_cube"`,
  `"boot_group_specific"`, or `"smart"` (default).

## Value

A single character string giving the resolved bootstrap method:

- `"whole_cube"`

- `"group_specific"`

- `"boot_whole_cube"`

- `"boot_group_specific"`

## Details

- the scope of the indicator (group-specific vs whole-cube), and

- whether a reference group is used.

When `method = "smart"`, the scope of the indicator is inferred using
[`derive_bootstrap_method()`](https://b-cubed-eu.github.io/dubicube/reference/derive_bootstrap_method.md).
If no reference group is specified (`ref_group = NA`) and exactly one
grouping variable is used (`length(cat_var) == 1`), the corresponding
`boot_*` method is selected.

The resolution follows these rules:

1.  If `method` is not `"smart"`, it is returned unchanged.

2.  If `method = "smart"`, the indicator scope is inferred using
    [`derive_bootstrap_method()`](https://b-cubed-eu.github.io/dubicube/reference/derive_bootstrap_method.md).

3.  If more than one grouping variable is specified
    (`length(cat_var) > 1`), bootstrapping via the boot package is
    disabled and the inferred non-boot method is returned.

4.  If exactly one grouping variable is used and `ref_group = NA`, the
    resolved method is prefixed with `"boot_"`, resulting in
    `"boot_group_specific"` or `"boot_whole_cube"`.

5.  If a reference group is specified, the non-boot variants
    `"group_specific"` or `"whole_cube"` are returned.

## See also

Other indicator_uncertainty_helper:
[`boot_list_to_dataframe()`](https://b-cubed-eu.github.io/dubicube/reference/boot_list_to_dataframe.md),
[`bootstrap_cube_raw()`](https://b-cubed-eu.github.io/dubicube/reference/bootstrap_cube_raw.md),
[`calculate_boot_ci_from_boot()`](https://b-cubed-eu.github.io/dubicube/reference/calculate_boot_ci_from_boot.md),
[`derive_bootstrap_method()`](https://b-cubed-eu.github.io/dubicube/reference/derive_bootstrap_method.md)

## Examples

``` r
# Example 1: Group-specific indicator without a reference group
# Mean sepal length per species (calculated independently per group)
mean_sepal_length <- function(x) {
  out_df <- aggregate(Sepal.Length ~ Species, x, mean)
  names(out_df) <- c("Species", "diversity_val")
  out_df
}

resolve_bootstrap_method(
  df = iris,
  fun = mean_sepal_length,
  cat_var = "Species",
  ref_group = NA,
  method = "smart"
)
#> [1] "boot_group_specific"

# Example 2: Group-specific indicator with a reference group
resolve_bootstrap_method(
  df = iris,
  fun = mean_sepal_length,
  cat_var = "Species",
  ref_group = "setosa",
  method = "smart"
)
#> [1] "group_specific"

# Example 3: Indicator that depends on the whole cube
# The statistic per species depends on all species together
scaled_sepal_length <- function(x) {
  out_df <- aggregate(Sepal.Length ~ Species, x, mean)
  out_df$Sepal.Length <- out_df$Sepal.Length / nrow(out_df)
  names(out_df) <- c("Species", "diversity_val")
  out_df
}

resolve_bootstrap_method(
  df = iris,
  fun = scaled_sepal_length,
  cat_var = "Species",
  ref_group = NA,
  method = "smart"
)
#> [1] "boot_whole_cube"
```
