# Perform bootstrapping over a dataframe for a calculated statistic

This function generate `samples` bootstrap replicates of a statistic
applied to a dataframe. It resamples the data cube and computes a
statistic `fun` for each bootstrap replicate, optionally comparing the
results to a reference group (`ref_group`). Bootstrapping happens over
the whole dataset `data_cube`.

## Usage

``` r
bootstrap_cube_raw(
  data_cube,
  fun,
  ...,
  grouping_var,
  samples = 1000,
  ref_group = NA,
  seed = NA,
  progress = FALSE
)
```

## Arguments

- data_cube:

  A dataframe.

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

- progress:

  Logical. Whether to show a progress bar. Set to `TRUE` to display a
  progress bar, `FALSE` (default) to suppress it.

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

## References

Davison, A. C., & Hinkley, D. V. (1997). Bootstrap Methods and their
Application (1st ed.). Cambridge University Press.
[doi:10.1017/CBO9780511802843](https://doi.org/10.1017/CBO9780511802843)

Efron, B., & Tibshirani, R. J. (1994). An Introduction to the Bootstrap
(1st ed.). Chapman and Hall/CRC.
[doi:10.1201/9780429246593](https://doi.org/10.1201/9780429246593)

## See also

Other indicator_uncertainty_helper:
[`boot_list_to_dataframe()`](https://b-cubed-eu.github.io/dubicube/reference/boot_list_to_dataframe.md),
[`calculate_boot_ci_from_boot()`](https://b-cubed-eu.github.io/dubicube/reference/calculate_boot_ci_from_boot.md),
[`derive_bootstrap_method()`](https://b-cubed-eu.github.io/dubicube/reference/derive_bootstrap_method.md),
[`resolve_bootstrap_method()`](https://b-cubed-eu.github.io/dubicube/reference/resolve_bootstrap_method.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Function to calculate statistic of interest
# Mean observations per year
mean_obs <- function(x) {
  out_df <- aggregate(obs ~ year, x, mean) # Calculate mean obs per year
  names(out_df) <- c("year", "diversity_val") # Rename columns
  return(out_df)
}
mean_obs(data)

# Perform bootstrapping
bootstrap_mean_obs <- bootstrap_cube_raw(
  data_cube = data,
  fun = mean_obs,
  grouping_var = "year",
  samples = 1000,
  seed = 123
)
head(bootstrap_mean_obs)
} # }
```
