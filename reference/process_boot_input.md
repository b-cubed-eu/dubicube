# Normalize and dispatch bootstrap input

Internal helper that standardizes bootstrap input supplied to
[`calculate_bootstrap_ci()`](https://b-cubed-eu.github.io/dubicube/reference/calculate_bootstrap_ci.md)
and handles early dispatch for `boot` objects. It contains no
statistical logic itself, but controls workflow routing.

## Usage

``` r
process_boot_input(
  bootstrap_results,
  grouping_var,
  type,
  conf,
  h,
  hinv,
  no_bias,
  boot_args
)
```

## Arguments

- bootstrap_results:

  A dataframe with bootstrap replicates, a single `boot` object, or a
  list of `boot` objects.

- grouping_var:

  Optional character vector of grouping variables used to identify
  indicators or strata.

- type:

  Character vector specifying the type(s) of confidence interval to
  compute (e.g. `"perc"`, `"bca"`, `"norm"`, `"basic"`).

- conf:

  Numeric confidence level between 0 and 1.

- h:

  Optional transformation function applied before CI calculation.

- hinv:

  Optional inverse transformation function.

- no_bias:

  Logical scalar. If `TRUE`, bias correction is skipped and `boot`
  objects are converted to a dataframe. If `FALSE`, confidence intervals
  are computed directly from the `boot` objects.

- boot_args:

  Optional list of additional arguments passed to
  [`boot::boot.ci()`](https://rdrr.io/pkg/boot/man/boot.ci.html).

## Value

A list with two elements:

- `data`: A dataframe of bootstrap replicates, or `NULL` if confidence
  intervals were computed directly

- `result`: A dataframe of confidence intervals, or `NULL` if the
  dataframe workflow should continue.

## Details

The function supports three input modes:

- Data frame input:

  Returned unchanged and passed on to the dataframe-based CI workflow.

- `boot` input with `no_bias = TRUE`:

  `boot` objects are converted to a dataframe of bootstrap replicates
  and passed on to the dataframe-based CI workflow.

- `boot` input with `no_bias = FALSE`:

  Confidence intervals are computed directly using
  [`boot::boot.ci()`](https://rdrr.io/pkg/boot/man/boot.ci.html) logic
  and returned immediately.
