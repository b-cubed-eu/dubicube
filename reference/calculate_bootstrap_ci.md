# Calculate confidence intervals for a dataframe with bootstrap replicates

This function calculates confidence intervals for a dataframe containing
bootstrap replicates based on different methods, including percentile
(`perc`), bias-corrected and accelerated (`bca`), normal (`norm`), and
basic (`basic`). The function also supports a `boot` object from the
boot package.

## Usage

``` r
calculate_bootstrap_ci(
  bootstrap_samples_df,
  grouping_var = NULL,
  type = c("perc", "bca", "norm", "basic"),
  conf = 0.95,
  h = function(t) t,
  hinv = function(t) t,
  no_bias = FALSE,
  aggregate = TRUE,
  data_cube = NA,
  fun = NA,
  ...,
  ref_group = NA,
  influence_method = ifelse(is.element("bca", type), "usual", NA),
  progress = FALSE,
  boot_args = list()
)
```

## Arguments

- bootstrap_samples_df:

  A dataframe with bootstrap replicates, or a list of `boot` objects.
  For dataframes, each row is a bootstrap sample; must include columns
  `rep_boot`, `est_original`, and the grouping variables. For `boot`
  objects, the function uses
  [`boot::boot.ci()`](https://rdrr.io/pkg/boot/man/boot.ci.html)
  internally.

- grouping_var:

  A character vector specifying the grouping variable(s) for the
  bootstrap analysis. The function `fun(data_cube$data, ...)` should
  return a row per group. The specified variables must not be redundant,
  meaning they should not contain the same information (e.g.,
  `"time_point"` (1, 2, 3) and `"year"` (2000, 2001, 2002) should not be
  used together if `"time_point"` is just an alternative encoding of
  `"year"`). This variable is used to split the dataset into groups for
  separate confidence interval calculations.

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

- no_bias:

  Logical. If `TRUE` intervals are centered around the original
  estimates (bias is ignored). Default is `FALSE`.Cannot be used with a
  boot method.

- aggregate:

  Logical. If `TRUE` (default), the function returns distinct confidence
  limits per group. If `FALSE`, the confidence limits are added to the
  original bootstrap dataframe `bootstrap_samples_df`.

- data_cube:

  Only used when `type = "bca"` and no boot method is used. A data cube
  object (class 'processed_cube' or 'sim_cube', see
  [`b3gbi::process_cube()`](https://b-cubed-eu.github.io/b3gbi/reference/process_cube.html))
  or a dataframe (cf. `$data` slot of 'processed_cube' or 'sim_cube').
  As used by
  [`bootstrap_cube()`](https://b-cubed-eu.github.io/dubicube/reference/bootstrap_cube.md).

- fun:

  Only used when `type = "bca"` and no boot method is used. A function
  which, when applied to `data_cube$data` returns the statistic(s) of
  interest (or just `data_cube` in case of a dataframe). This function
  must return a dataframe with a column `diversity_val` containing the
  statistic of interest. As used by
  [`bootstrap_cube()`](https://b-cubed-eu.github.io/dubicube/reference/bootstrap_cube.md).

- ...:

  Additional arguments passed on to `fun`.

- ref_group:

  Only used when `type = "bca"`. A string indicating the reference group
  to compare the statistic with. Default is `NA`, meaning no reference
  group is used. As used by
  [`bootstrap_cube()`](https://b-cubed-eu.github.io/dubicube/reference/bootstrap_cube.md).

- influence_method:

  A string specifying the method used for calculating the influence
  values.

  - `"usual"`: Negative jackknife (default if BCa is selected).

  - `"pos"`: Positive jackknife

- progress:

  Logical. Whether to show a progress bar for jackknifing. Set to `TRUE`
  to display a progress bar, `FALSE` (default) to suppress it.

- boot_args:

  Named list of additional arguments passed to
  [`boot::boot.ci()`](https://rdrr.io/pkg/boot/man/boot.ci.html).

## Value

A dataframe containing the bootstrap results with the following columns:

- `est_original`: The statistic based on the full dataset per group

- rep_boo

- `est_boot`: The bootstrap estimate (mean of bootstrap replicates per
  group)

- `se_boot`: The standard error of the bootstrap estimate (standard
  deviation of the bootstrap replicates per group)

- `bias_boot`: The bias of the bootstrap estimate per group

- `int_type`: The interval type

- `ll`: The lower limit of the confidence interval

- `ul`: The upper limit of the confidence interval

- `conf`: The confidence level of the interval When `aggregate = FALSE`,
  the dataframe contains the columns from `bootstrap_samples_df` with
  one row per bootstrap replicate.

## Details

We consider four different types of intervals (with confidence level
\\\alpha\\). The choice for confidence interval types and their
calculation is in line with the boot package in R (Canty & Ripley, 1999)
to ensure ease of implementation. They are based on the definitions
provided by Davison & Hinkley (1997, Chapter 5) (see also DiCiccio &
Efron, 1996; Efron, 1987).

1.  **Percentile**: Uses the percentiles of the bootstrap distribution.

    \$\$CI\_{perc} = \left\[ \hat{\theta}^\*\_{(\alpha/2)},
    \hat{\theta}^\*\_{(1-\alpha/2)} \right\]\$\$

    where \\\hat{\theta}^\*\_{(\alpha/2)}\\ and
    \\\hat{\theta}^\*\_{(1-\alpha/2)}\\ are the \\\alpha/2\\ and
    \\1-\alpha/2\\ percentiles of the bootstrap distribution,
    respectively.

2.  **Bias-Corrected and Accelerated (BCa)**: Adjusts for bias and
    acceleration

    Bias refers to the systematic difference between the observed
    statistic from the original dataset and the center of the bootstrap
    distribution of the statistic. The bias correction term is
    calculated as follows:

    \$\$\hat{z}\_0 = \Phi^{-1}\left(\frac{\\(\hat{\theta}^\*\_b \<
    \hat{\theta})}{B}\right)\$\$

    where \\\\\\ is the counting operator, counting the number of times
    \\\hat{\theta}^\*\_b\\ is smaller than \\\hat{\theta}\\, and
    \\\Phi^{-1}\\ the inverse cumulative density function of the
    standard normal distribution.\\B\\ is the number of bootstrap
    samples.

    Acceleration quantifies how sensitive the variability of the
    statistic is to changes in the data. See
    [`calculate_acceleration()`](https://b-cubed-eu.github.io/dubicube/reference/calculate_acceleration.md)
    on how this is calculated.

    - \\a=0\\: The statistic's variability does not depend on the data
      (e.g., symmetric distribution)

    - \\a\>0\\: Small changes in the data have a large effect on the
      statistic's variability (e.g., positive skew)

    - \\a\<0\\: Small changes in the data have a smaller effect on the
      statistic's variability (e.g., negative skew).

    The bias and acceleration estimates are then used to calculate
    adjusted percentiles.

    \\\alpha_1 = \Phi\left( \hat{z}\_0 + \frac{\hat{z}\_0 +
    z\_{\alpha/2}}{1 - \hat{a}(\hat{z}\_0 + z\_{\alpha/2})} \right)\\,
    \\\alpha_2 = \Phi\left( \hat{z}\_0 + \frac{\hat{z}\_0 + z\_{1 -
    \alpha/2}}{1 - \hat{a}(\hat{z}\_0 + z\_{1 - \alpha/2})} \right)\\

    So, we get

    \$\$CI\_{bca} = \left\[ \hat{\theta}^\*\_{(\alpha_1)},
    \hat{\theta}^\*\_{(\alpha_2)} \right\]\$\$

3.  **Normal**: Assumes the bootstrap distribution of the statistic is
    approximately normal

    \$\$CI\_{norm} = \left\[\hat{\theta} - \text{Bias}\_{\text{boot}} -
    \text{SE}\_{\text{boot}} \times z\_{1-\alpha/2}, \hat{\theta} -
    \text{Bias}\_{\text{boot}} + \text{SE}\_{\text{boot}} \times
    z\_{1-\alpha/2} \right\]\$\$

    where \\z\_{1-\alpha/2}\\ is the \\1-\alpha/2\\ quantile of the
    standard normal distribution.

4.  **Basic**: Centers the interval using percentiles

    \$\$CI\_{basic} = \left\[ 2\hat{\theta} -
    \hat{\theta}^\*\_{(1-\alpha/2)}, 2\hat{\theta} -
    \hat{\theta}^\*\_{(\alpha/2)} \right\]\$\$

    where \\\hat{\theta}^\*\_{(\alpha/2)}\\ and
    \\\hat{\theta}^\*\_{(1-\alpha/2)}\\ are the \\\alpha/2\\ and
    \\1-\alpha/2\\ percentiles of the bootstrap distribution,
    respectively.

## References

Canty, A., & Ripley, B. (1999). boot: Bootstrap Functions (Originally by
Angelo Canty for S) \[Computer software\].
<https://CRAN.R-project.org/package=boot>

Davison, A. C., & Hinkley, D. V. (1997). Bootstrap Methods and their
Application (1st ed.). Cambridge University Press.
[doi:10.1017/CBO9780511802843](https://doi.org/10.1017/CBO9780511802843)

DiCiccio, T. J., & Efron, B. (1996). Bootstrap confidence intervals.
Statistical Science, 11(3).
[doi:10.1214/ss/1032280214](https://doi.org/10.1214/ss/1032280214)

Efron, B. (1987). Better Bootstrap Confidence Intervals. Journal of the
American Statistical Association, 82(397), 171–185.
[doi:10.1080/01621459.1987.10478410](https://doi.org/10.1080/01621459.1987.10478410)

Efron, B., & Tibshirani, R. J. (1994). An Introduction to the Bootstrap
(1st ed.). Chapman and Hall/CRC.
[doi:10.1201/9780429246593](https://doi.org/10.1201/9780429246593)

## See also

Other indicator_uncertainty:
[`add_effect_classification()`](https://b-cubed-eu.github.io/dubicube/reference/add_effect_classification.md),
[`bootstrap_cube()`](https://b-cubed-eu.github.io/dubicube/reference/bootstrap_cube.md),
[`calculate_acceleration()`](https://b-cubed-eu.github.io/dubicube/reference/calculate_acceleration.md)

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

# Calculate confidence limits
# Percentile interval
ci_mean_obs <- calculate_bootstrap_ci(
  bootstrap_samples_df = bootstrap_mean_obs,
  grouping_var = "year",
  type = "perc",
  conf = 0.95
)
ci_mean_obs
} # }
```
