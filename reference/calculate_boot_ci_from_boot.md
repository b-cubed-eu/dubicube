# Calculate confidence intervals from a 'boot' object

This function calculates multiple types of confidence intervals (normal,
basic, percentile, BCa) for a boot object using
[`boot::boot.ci()`](https://rdrr.io/pkg/boot/man/boot.ci.html).

## Usage

``` r
calculate_boot_ci_from_boot(
  boot_obj,
  type = c("norm", "basic", "perc", "bca"),
  conf = 0.95,
  h = function(t) t,
  hinv = function(t) t,
  boot_args = list()
)
```

## Arguments

- boot_obj:

  A `boot` object (from the boot package).

- type:

  A character vector specifying the type(s) of confidence intervals to
  compute. Options include:

  - `"perc"`: Percentile interval

  - `"bca"`: Bias-corrected and accelerated interval

  - `"norm"`: Normal interval

  - `"basic"`: Basic interval

  - `"all"`: Compute all available interval types (default)

- conf:

  A numeric value specifying the confidence level of the intervals.
  Default is `0.95` (95 % confidence level).

- h:

  A function defining a transformation. The intervals are calculated on
  the scale of `h(t)` and the inverse function `hinv` applied to the
  resulting intervals. It must be a function of one variable only. The
  default is the identity function.

- hinv:

  A function, like `h`, which returns the inverse of `h`. It is used to
  transform the intervals calculated on the scale of `h(t)` back to the
  original scale. The default is the identity function. If `h` is
  supplied but `hinv` is not, then the intervals returned will be on the
  transformed scale.

- boot_args:

  Named list of additional arguments to pass to
  [`boot::boot.ci()`](https://rdrr.io/pkg/boot/man/boot.ci.html).

## Value

A tidy dataframe with columns:

- `stat_index`: Index of statistic in the boot object

- `est_original`: Original estimate from full dataset

- `int_type`: Interval type

- `ll`: Lower confidence limit

- `ul`: Upper confidence limit

- `conf`: Confidence level

## See also

Other indicator_uncertainty_helper:
[`boot_list_to_dataframe()`](https://b-cubed-eu.github.io/dubicube/reference/boot_list_to_dataframe.md),
[`bootstrap_cube_raw()`](https://b-cubed-eu.github.io/dubicube/reference/bootstrap_cube_raw.md),
[`derive_bootstrap_method()`](https://b-cubed-eu.github.io/dubicube/reference/derive_bootstrap_method.md),
[`resolve_bootstrap_method()`](https://b-cubed-eu.github.io/dubicube/reference/resolve_bootstrap_method.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(boot)

# Function to compute the mean
mean_fun <- function(data, indices) {
  mean(data[indices])
}

# Bootstrap mean of the 'mpg' variable in mtcars
set.seed(123)
boot_obj <- boot(data = mtcars$mpg, statistic = mean_fun, R = 1000)

# Calculate confidence intervals for all types
ci_df <- calculate_boot_ci_from_boot(
  boot_obj = boot_obj,
  type = "all",
  conf = 0.95
)
ci_df
} # }
```
