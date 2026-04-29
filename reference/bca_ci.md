# Calculate Bias-Corrected and Accelerated (BCa) bootstrap confidence interval

This function calculates a Bias-Corrected and Accelerated (BCa)
confidence interval from a bootstrap sample. It is used by
[`calculate_bootstrap_ci()`](https://b-cubed-eu.github.io/dubicube/reference/calculate_bootstrap_ci.md).

## Usage

``` r
bca_ci(t0, t, a, conf = 0.95, h = function(t) t, hinv = function(t) t)
```

## Arguments

- t0:

  Original statistic.

- t:

  Numeric vector of bootstrap replicates.

- a:

  Acceleration constant. See also
  [`calculate_acceleration()`](https://b-cubed-eu.github.io/dubicube/reference/calculate_acceleration.md).

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

Adjusts for bias and acceleration. Bias refers to the systematic
difference between the observed statistic from the original dataset and
the center of the bootstrap distribution of the statistic. The bias
correction term is calculated as follows:

\$\$\hat{z}\_0 = \Phi^{-1}\left(\frac{\\(\hat{\theta}^\*\_b \<
\hat{\theta})}{B}\right)\$\$

where \\\\\\ is the counting operator, counting the number of times
\\\hat{\theta}^\*\_b\\ is smaller than \\\hat{\theta}\\, and
\\\Phi^{-1}\\ the inverse cumulative density function of the standard
normal distribution.\\B\\ is the number of bootstrap samples.

Acceleration quantifies how sensitive the variability of the statistic
is to changes in the data. See
[`calculate_acceleration()`](https://b-cubed-eu.github.io/dubicube/reference/calculate_acceleration.md)
on how this is calculated.

- \\a=0\\: The statistic's variability does not depend on the data
  (e.g., symmetric distribution)

- \\a\>0\\: Small changes in the data have a large effect on the
  statistic's variability (e.g., positive skew)

- \\a\<0\\: Small changes in the data have a smaller effect on the
  statistic's variability (e.g., negative skew).

The bias and acceleration estimates are then used to calculate adjusted
percentiles.

\\\alpha_1 = \Phi\left( \hat{z}\_0 + \frac{\hat{z}\_0 +
z\_{\alpha/2}}{1 - \hat{a}(\hat{z}\_0 + z\_{\alpha/2})} \right)\\,
\\\alpha_2 = \Phi\left( \hat{z}\_0 + \frac{\hat{z}\_0 + z\_{1 -
\alpha/2}}{1 - \hat{a}(\hat{z}\_0 + z\_{1 - \alpha/2})} \right)\\

So, we get

\$\$CI\_{bca} = \left\[ \hat{\theta}^\*\_{(\alpha_1)},
\hat{\theta}^\*\_{(\alpha_2)} \right\]\$\$

## Note

This function is adapted from the internal function `bca.ci()` in the
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
[`norm_ci()`](https://b-cubed-eu.github.io/dubicube/reference/norm_ci.md),
[`perc_ci()`](https://b-cubed-eu.github.io/dubicube/reference/perc_ci.md)

## Examples

``` r
set.seed(123)
boot_reps <- rnorm(1000)
t0 <- mean(boot_reps)

# Example acceleration value (normally estimated via jackknife)
a <- 0.01

# BCa bootstrap CI
bca_ci(t0, boot_reps, a, conf = 0.95)
#>      conf rk_lower rk_upper        ll       ul
#> [1,] 0.95    27.62   978.46 -1.930133 2.112914
```
