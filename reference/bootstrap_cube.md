# Perform bootstrapping over a data cube for a calculated statistic

This function generate `samples` bootstrap replicates of a statistic
applied to a data cube. It resamples the data cube and computes a
statistic `fun` for each bootstrap replicate, optionally comparing the
results to a reference group (`ref_group`).

## Usage

``` r
bootstrap_cube(
  data_cube,
  fun,
  ...,
  grouping_var,
  samples = 1000,
  ref_group = NA,
  seed = NA,
  processed_cube = TRUE,
  method = "smart",
  progress = FALSE,
  boot_args = list()
)
```

## Arguments

- data_cube:

  A data cube object (class 'processed_cube' or 'sim_cube', see
  [`b3gbi::process_cube()`](https://b-cubed-eu.github.io/b3gbi/reference/process_cube.html))
  or a dataframe (cf. `$data` slot of 'processed_cube' or 'sim_cube').
  If `processed_cube = TRUE` (default), this must be a processed or
  simulated data cube that contains a `$data` element.

- fun:

  A function which, when applied to `data_cube$data` returns the
  statistic(s) of interest (or just `data_cube` in case of a dataframe).
  This function must return a dataframe with a column `diversity_val`
  containing the statistic of interest.

- ...:

  Additional arguments passed on to `fun`.

- grouping_var:

  A character vector specifying the grouping variable(s) for the
  bootstrap analysis. The function `fun(data_cube$data, ...)` should
  return a row per group. The specified variables must not be redundant,
  meaning they should not contain the same information (e.g.,
  `"time_point"` (1, 2, 3) and `"year"` (2000, 2001, 2002) should not be
  used together if `"time_point"` is just an alternative encoding of
  `"year"`).

- samples:

  The number of bootstrap replicates. A single positive integer. Default
  is 1000.

- ref_group:

  A string indicating the reference group to compare the statistic with.
  Default is `NA`, meaning no reference group is used.

- seed:

  A positive numeric value setting the seed for random number generation
  to ensure reproducibility. If `NA` (default), then
  [`set.seed()`](https://rdrr.io/r/base/Random.html) is not called at
  all. If not `NA`, then the random number generator state is reset (to
  the state before calling this function) upon exiting this function.

- processed_cube:

  Logical. If `TRUE` (default), the function expects `data_cube` to be a
  data cube object with a `$data` slot. If `FALSE`, the function expects
  `data_cube` to be a dataframe.

- method:

  A character string specifying the bootstrap method. Options include:

  - `"smart"`: Automatically select the appropriate bootstrap method
    based on indicator behaviour and the presence of a reference group
    (default).

  - `"boot_whole_cube"`: Perform whole-cube bootstrap using
    [`boot::boot()`](https://rdrr.io/pkg/boot/man/boot.html). Cannot be
    used with `ref_group`.

  - `"boot_group_specific"`: Perform group-specific bootstrap using
    [`boot::boot()`](https://rdrr.io/pkg/boot/man/boot.html). Cannot be
    used with `ref_group`.

  - `"whole_cube"`: Perform whole-cube bootstrap without using the boot
    package. Can be used with `ref_group`.

  - `"group_specific"`: Perform group-specific bootstrap without using
    the boot package. Can be used with `ref_group`.

- progress:

  Logical. Whether to show a progress bar. Set to `TRUE` to display a
  progress bar, `FALSE` (default) to suppress it.

- boot_args:

  Named list of additional arguments passed to
  [`boot::boot()`](https://rdrr.io/pkg/boot/man/boot.html).

## Value

A dataframe containing the bootstrap results with the following columns:

- `sample`: Sample ID of the bootstrap replicate

- `est_original`: The statistic based on the full dataset per group

- `rep_boot`: The statistic based on a bootstrapped dataset (bootstrap
  replicate)

- `est_boot`: The bootstrap estimate (mean of bootstrap replicates per
  group)

- `se_boot`: The standard error of the bootstrap estimate (standard
  deviation of the bootstrap replicates per group)

- `bias_boot`: The bias of the bootstrap estimate per group

If `method` resolves to `"boot_whole_cube"` or `"boot_group_specific"`,
the returned value is a named list of objects of class `"boot"`, as
produced by [`boot::boot()`](https://rdrr.io/pkg/boot/man/boot.html).

## Details

Bootstrapping is a statistical technique used to estimate the
distribution of a statistic by resampling with replacement from the
original data (Davison & Hinkley, 1997; Efron & Tibshirani, 1994). In
the case of data cubes, each row is sampled with replacement. Below are
the common notations used in bootstrapping:

1.  **Original Sample Data**: \\\mathbf{X} = \\X_1, X_2, \ldots, X_n\\\\

    - The initial set of data points. Here, \\n\\ is the sample size.
      This corresponds to the number of cells in a data cube or the
      number of rows in tabular format.

2.  **Statistic of Interest**: \\\theta\\

    - The parameter or statistic being estimated, such as the mean
      \\\bar{X}\\, variance \\\sigma^2\\, or a biodiversity indicator.
      Let \\\hat{\theta}\\ denote the estimated value of \\\theta\\
      calculated from the complete dataset \\\mathbf{X}\\.

3.  **Bootstrap Sample**: \\\mathbf{X}^\* = \\X_1^\*, X_2^\*, \ldots,
    X_n^\*\\\\

    - A sample of size \\n\\ drawn with replacement from the original
      sample \\\mathbf{X}\\. Each \\X_i^\*\\ is drawn independently from
      \\\mathbf{X}\\.

    - A total of \\B\\ bootstrap samples are drawn from the original
      data. Common choices for \\B\\ are 1000 or 10,000 to ensure a good
      approximation of the distribution of the bootstrap replications
      (see further).

4.  **Bootstrap Replication**: \\\hat{\theta}^\*\_b\\

    - The value of the statistic of interest calculated from the
      \\b\\-th bootstrap sample \\\mathbf{X}^\*\_b\\. For example, if
      \\\theta\\ is the sample mean, \\\hat{\theta}^\*\_b =
      \bar{X}^\*\_b\\.

5.  **Bootstrap Statistics**:

- **Bootstrap Estimate of the Statistic**:
  \\\hat{\theta}\_{\text{boot}}\\

  - The average of the bootstrap replications:

\$\$\hat{\theta}\_{\text{boot}} = \frac{1}{B} \sum\_{b=1}^B
\hat{\theta}^\*\_b\$\$

- **Bootstrap Bias**: \\\text{Bias}\_{\text{boot}}\\

  - This bias indicates how much the bootstrap estimate deviates from
    the original sample estimate. It is calculated as the difference
    between the average bootstrap estimate and the original estimate:

\$\$\text{Bias}\_{\text{boot}} = \frac{1}{B} \sum\_{b=1}^B
(\hat{\theta}^\*\_b - \hat{\theta}) = \hat{\theta}\_{\text{boot}} -
\hat{\theta}\$\$

- **Bootstrap Standard Error**: \\\text{SE}\_{\text{boot}}\\

  - The standard deviation of the bootstrap replications, which
    estimates the variability of the statistic.

There are two methods for bootstrapping:

- Whole-cube bootstrapping: resampling all rows in the cube, regardless
  of grouping. For indicators that are use data across groups.

- Group-specific bootstrapping: resampling rows only within a group of
  interest (e.g., a species, year, or habitat). For indicators that are
  calculated independently per group.

The default smart option (`method = "smart"`) determines both (i)
whether the indicator is group-specific or whole-cube, and (ii) whether
the boot package should be used.

The decision is made by calculating the statistic on larger and smaller
subsets of the data (containing respectively more and fewer groups in
`grouping_var`). If indicator values for the common groups are
identical, the indicator is treated as group-specific; otherwise, it is
treated as whole-cube.

If no reference group is used (`ref_group = NA`), `method = "smart"`
resolves to `"boot_group_specific"` or `"boot_whole_cube"`, both of
which use [`boot::boot()`](https://rdrr.io/pkg/boot/man/boot.html). If a
reference group is specified, `method = "smart"` resolves to
`"group_specific"` or `"whole_cube"` and bootstrapping is handled
internally.

## References

Davison, A. C., & Hinkley, D. V. (1997). Bootstrap Methods and their
Application (1st ed.). Cambridge University Press.
[doi:10.1017/CBO9780511802843](https://doi.org/10.1017/CBO9780511802843)

Efron, B., & Tibshirani, R. J. (1994). An Introduction to the Bootstrap
(1st ed.). Chapman and Hall/CRC.
[doi:10.1201/9780429246593](https://doi.org/10.1201/9780429246593)

## See also

Other indicator_uncertainty:
[`add_effect_classification()`](https://b-cubed-eu.github.io/dubicube/reference/add_effect_classification.md),
[`calculate_acceleration()`](https://b-cubed-eu.github.io/dubicube/reference/calculate_acceleration.md),
[`calculate_bootstrap_ci()`](https://b-cubed-eu.github.io/dubicube/reference/calculate_bootstrap_ci.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# After processing a data cube with b3gbi::process_cube()

# Function to calculate statistic of interest
# Mean observations per year
mean_obs <- function(x) {
  out_df <- aggregate(obs ~ year, x, mean) # Calculate mean obs per year
  names(out_df) <- c("year", "diversity_val") # Rename columns
  return(out_df)
}
mean_obs(processed_cube$data)

# Perform bootstrapping
bootstrap_mean_obs <- bootstrap_cube(
  data_cube = processed_cube,
  fun = mean_obs,
  grouping_var = "year",
  samples = 1000,
  seed = 123
)
} # }
```
