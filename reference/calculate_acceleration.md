# Calculate acceleration for a statistic in a dataframe

This function calculates acceleration values, which quantify the
sensitivity of a statistic’s variability to changes in the dataset.
Acceleration is used for bias-corrected and accelerated (BCa) confidence
intervals in
[`calculate_bootstrap_ci()`](https://b-cubed-eu.github.io/dubicube/reference/calculate_bootstrap_ci.md).

## Usage

``` r
calculate_acceleration(
  data_cube,
  fun,
  ...,
  grouping_var,
  ref_group = NA,
  influence_method = "usual",
  processed_cube = TRUE,
  progress = FALSE
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
  containing the statistic of interest. As used by
  [`bootstrap_cube()`](https://b-cubed-eu.github.io/dubicube/reference/bootstrap_cube.md).

- ...:

  Additional arguments passed on to `fun`.

- grouping_var:

  A character vector specifying the grouping variable(s) for the
  bootstrap analysis. The function `fun(data_cube$data, ...)` should
  return a row per group. The specified variables must not be redundant,
  meaning they should not contain the same information (e.g.,
  `"time_point"` (1, 2, 3) and `"year"` (2000, 2001, 2002) should not be
  used together if `"time_point"` is just an alternative encoding of
  `"year"`). This variable is used to split the dataset into groups for
  separate acceleration calculations.

- ref_group:

  A string indicating the reference group to compare the statistic with.
  Default is `NA`, meaning no reference group is used. As used by
  [`bootstrap_cube()`](https://b-cubed-eu.github.io/dubicube/reference/bootstrap_cube.md).

- influence_method:

  A string specifying the method used for calculating the influence
  values.

  - `"usual"`: Negative jackknife (default if BCa is selected).

  - `"pos"`: Positive jackknife

- processed_cube:

  Logical. If `TRUE` (default), the function expects `data_cube` to be a
  data cube object with a `$data` slot. If `FALSE`, the function expects
  `data_cube` to be a dataframe.

- progress:

  Logical. Whether to show a progress bar for jackknifing. Set to `TRUE`
  to display a progress bar, `FALSE` (default) to suppress it.

## Value

A dataframe containing the acceleration values per `grouping_var`.

## Details

Acceleration quantifies how sensitive the variability of a statistic
\\\theta\\ is to changes in the data.

- \\a=0\\: The statistic's variability does not depend on the data
  (e.g., symmetric distribution)

- \\a\>0\\: Small changes in the data have a large effect on the
  statistic's variability (e.g., positive skew)

- \\a\<0\\: Small changes in the data have a smaller effect on the
  statistic's variability (e.g., negative skew).

It is used for BCa confidence interval calculation, which adjust for
bias and skewness in bootstrapped distributions (Davison & Hinkley,
1997, Chapter 5). See also the `empinf()` function of the boot package
in R (Canty & Ripley, 1999)). The acceleration is calculated as follows:

\$\$\hat{a} = \frac{1}{6} \frac{\sum\_{i = 1}^{n}(I_i^3)}{\left(
\sum\_{i = 1}^{n}(I_i^2) \right)^{3/2}}\$\$

where \\I_i\\ denotes the influence of data point \\x_i\\ on the
estimation of \\\theta\\. \\I_i\\ can be estimated using jackknifing.
Examples are (1) the negative jackknife: \\I_i = (n-1)(\hat{\theta} -
\hat{\theta}\_{-i})\\, and (2) the positive jackknife \\I_i =
(n+1)(\hat{\theta}\_{-i} - \hat{\theta})\\ (Frangos & Schucany, 1990).
Here, \\\hat{\theta}\_{-i}\\ is the estimated value leaving out the
\\i\\’th data point \\x_i\\. The boot package also offers infinitesimal
jackknife and regression estimation. Implementation of these jackknife
algorithms can be explored in the future.

If a reference group is used, jackknifing is implemented in a different
way. Consider \\\hat{\theta} = \hat{\theta}\_1 - \hat{\theta}\_2\\ where
\\\hat{\theta}\_1\\ is the estimate for the indicator value of a
non-reference period (sample size \\n_1\\) and \\\hat{\theta}\_2\\ is
the estimate for the indicator value of a reference period (sample size
\\n_2\\). The acceleration is now calculated as follows:

\$\$\hat{a} = \frac{1}{6} \frac{\sum\_{i = 1}^{n_1 + n_2}(I_i^3)}{\left(
\sum\_{i = 1}^{n_1 + n_2}(I_i^2) \right)^{3/2}}\$\$

\\I_i\\ can be calculated using the negative or positive jackknife. Such
that

\\\hat{\theta}\_{-i} = \hat{\theta}\_{1,-i} - \hat{\theta}\_2 \text{ for
} i = 1, \ldots, n_1\\, and

\\\hat{\theta}\_{-i} = \hat{\theta}\_{1} - \hat{\theta}\_{2,-i} \text{
for } i = n_1 + 1, \ldots, n_1 + n_2\\

## References

Canty, A., & Ripley, B. (1999). boot: Bootstrap Functions (Originally by
Angelo Canty for S) \[Computer software\].
<https://CRAN.R-project.org/package=boot>

Davison, A. C., & Hinkley, D. V. (1997). Bootstrap Methods and their
Application (1st ed.). Cambridge University Press.
[doi:10.1017/CBO9780511802843](https://doi.org/10.1017/CBO9780511802843)

Frangos, C. C., & Schucany, W. R. (1990). Jackknife estimation of the
bootstrap acceleration constant. Computational Statistics & Data
Analysis, 9(3), 271–281.
[doi:10.1016/0167-9473(90)90109-U](https://doi.org/10.1016/0167-9473%2890%2990109-U)

## See also

Other indicator_uncertainty:
[`add_effect_classification()`](https://b-cubed-eu.github.io/dubicube/reference/add_effect_classification.md),
[`bootstrap_cube()`](https://b-cubed-eu.github.io/dubicube/reference/bootstrap_cube.md),
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

# Calculate acceleration
acceleration_df <- calculate_acceleration(
  data_cube = processed_cube,
  fun = mean_obs,
  grouping_var = "year",
  progress = FALSE
)
acceleration_df
} # }
```
