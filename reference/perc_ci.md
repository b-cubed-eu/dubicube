# Calculate percentile bootstrap confidence interval

This function calculates a percentile confidence interval from a
bootstrap sample. It is used by
[`calculate_bootstrap_ci()`](https://b-cubed-eu.github.io/dubicube/reference/calculate_bootstrap_ci.md).

## Usage

``` r
perc_ci(t, conf = 0.95, h = function(t) t, hinv = function(t) t)
```

## Arguments

- t:

  Numeric vector of bootstrap replicates.

- conf:

  A numeric value specifying the confidence level of the interval.
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

## Value

A matrix with four columns:

- `conf`: confidence level

- `rk_lower`: rank of lower endpoint (interpolated)

- `rk_upper`: rank of upper endpoint (interpolated)

- `ll`: lower confidence limit

- `ul`: lower confidence limit

## Details

\$\$CI\_{perc} = \left\[ \hat{\theta}^\*\_{(\alpha/2)},
\hat{\theta}^\*\_{(1-\alpha/2)} \right\]\$\$

where \\\hat{\theta}^\*\_{(\alpha/2)}\\ and
\\\hat{\theta}^\*\_{(1-\alpha/2)}\\ are the \\\alpha/2\\ and
\\1-\alpha/2\\ percentiles of the bootstrap distribution, respectively.

## Note

This function is adapted from the internal function `perc.ci()` in the
boot package (Canty & Ripley, 1999).

## References

Canty, A., & Ripley, B. (1999). boot: Bootstrap Functions (Originally by
Angelo Canty for S) \[Computer software\].
<https://CRAN.R-project.org/package=boot>

Davison, A. C., & Hinkley, D. V. (1997). Bootstrap Methods and their
Application (1st ed.). Cambridge University Press.
[doi:10.1017/CBO9780511802843](https://doi.org/10.1017/CBO9780511802843)

## See also

Other interval_calculation:
[`basic_ci()`](https://b-cubed-eu.github.io/dubicube/reference/basic_ci.md),
[`bca_ci()`](https://b-cubed-eu.github.io/dubicube/reference/bca_ci.md),
[`norm_ci()`](https://b-cubed-eu.github.io/dubicube/reference/norm_ci.md)

## Examples

``` r
set.seed(123)
boot_reps <- rnorm(1000)      # bootstrap replicates
t0 <- mean(boot_reps)         # observed statistic

# Percentile CI
perc_ci(boot_reps, conf = 0.95)
#>      conf rk_lower rk_upper       ll       ul
#> [1,] 0.95    25.03   975.98 -1.94292 2.049767
```
