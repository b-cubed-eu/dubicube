# Calculate normal bootstrap confidence interval

This function calculates a normal confidence interval from a bootstrap
sample. It is used by
[`calculate_bootstrap_ci()`](https://b-cubed-eu.github.io/dubicube/reference/calculate_bootstrap_ci.md).

## Usage

``` r
norm_ci(
  t0,
  t,
  conf = 0.95,
  h = function(t) t,
  hinv = function(t) t,
  no_bias = FALSE
)
```

## Arguments

- t0:

  Original statistic.

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

- no_bias:

  Logical. If `TRUE` intervals are centered around the original
  estimates (bias is ignored). Default is `FALSE`.

## Value

A matrix with four columns:

- `conf`: confidence level

- `ll`: lower confidence limit

- `ul`: lower confidence limit

## Details

\$\$CI\_{norm} = \left\[\hat{\theta} - \text{Bias}\_{\text{boot}} -
\text{SE}\_{\text{boot}} \times z\_{1-\alpha/2}, \hat{\theta} -
\text{Bias}\_{\text{boot}} + \text{SE}\_{\text{boot}} \times
z\_{1-\alpha/2} \right\]\$\$

where \\z\_{1-\alpha/2}\\ is the \\1-\alpha/2\\ quantile of the standard
normal distribution.

## Note

This function is adapted from the function
[`norm.ci()`](https://rdrr.io/pkg/boot/man/norm.ci.html) in the boot
package (Canty & Ripley, 1999).

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
[`perc_ci()`](https://b-cubed-eu.github.io/dubicube/reference/perc_ci.md)

## Examples

``` r
set.seed(123)
boot_reps <- rnorm(1000)
t0 <- mean(boot_reps)

# Normal-based CI
norm_ci(t0, boot_reps, conf = 0.90)
#>      conf        ll       ul
#> [1,]  0.9 -1.615065 1.647321

# Without bias correction
norm_ci(t0, boot_reps, conf = 0.90, no_bias = TRUE)
#>      conf        ll       ul
#> [1,]  0.9 -1.615065 1.647321
```
