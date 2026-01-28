# Add effect classifications to a dataframe by comparing the confidence intervals with a reference and thresholds

This function adds classified effects to a dataframe as ordered factor
variables by comparing the confidence intervals with a reference and
thresholds.

## Usage

``` r
add_effect_classification(
  df,
  cl_columns,
  threshold,
  reference = 0,
  coarse = TRUE
)
```

## Arguments

- df:

  A dataframe containing summary data of confidence limits. Two columns
  are required containing lower and upper limits indicated by the
  `cl_columns` argument. Any other columns are optional.

- cl_columns:

  A vector of 2 column names in `df` indicating respectively the lower
  and upper confidence limits (e.g. `c("lcl", "ucl")`).

- threshold:

  A vector of either 1 or 2 thresholds. A single threshold will be
  transformed into `reference + c(-abs(threshold), abs(threshold))`.

- reference:

  The null hypothesis value to compare confidence intervals against.
  Defaults to 0.

- coarse:

  Logical, defaults to `TRUE`. If `TRUE`, add a coarse classification to
  the dataframe.

## Value

The returned value is a modified version of the original input dataframe
`df` with additional columns `effect_code` and `effect` containing
respectively the effect symbols and descriptions as ordered factor
variables. In case of `coarse = TRUE` (by default) also
`effect_code_coarse` and `effect_coarse` containing the coarse
classification effects.

## Details

This function is a wrapper around `effectclass::classify()` and
[`effectclass::coarse_classification()`](https://inbo.github.io/effectclass/reference/coarse_classification.html)
from the effectclass package (Onkelinx, 2023). They classify effects in
a stable and transparent manner.

|  |  |  |  |
|----|----|----|----|
| Symbol | Fine effect / trend | Coarse effect / trend | Rule |
| `++` | strong positive effect / strong increase | positive effect / increase | confidence interval above the upper threshold |
| `+` | positive effect / increase | positive effect / increase | confidence interval above reference and contains the upper threshold |
| `+~` | moderate positive effect / moderate increase | positive effect / increase | confidence interval between reference and the upper threshold |
| `~` | no effect / stable | no effect / stable | confidence interval between thresholds and contains reference |
| `-~` | moderate negative effect / moderate decrease | negative effect / decrease | confidence interval between reference and the lower threshold |
| `-` | negative effect / decrease | negative effect / decrease | confidence interval below reference and contains the lower threshold |
| `--` | strong negative effect / strong decrease | negative effect / decrease | confidence interval below the lower threshold |
| `?+` | potential positive effect / potential increase | unknown effect / unknown | confidence interval contains reference and the upper threshold |
| `?-` | potential negative effect / potential decrease | unknown effect / unknown | confidence interval contains reference and the lower threshold |
| `?` | unknown effect / unknown | unknown effect / unknown | confidence interval contains the lower and upper threshold |

## References

Onkelinx, T. (2023). effectclass: Classification and visualisation of
effects \[Computer software\]. <https://inbo.github.io/effectclass/>

## See also

Other indicator_uncertainty:
[`bootstrap_cube()`](https://b-cubed-eu.github.io/dubicube/reference/bootstrap_cube.md),
[`calculate_acceleration()`](https://b-cubed-eu.github.io/dubicube/reference/calculate_acceleration.md),
[`calculate_bootstrap_ci()`](https://b-cubed-eu.github.io/dubicube/reference/calculate_bootstrap_ci.md)

## Examples

``` r
# Example dataset
ds <- data.frame(
  mean = c(0, 0.5, -0.5, 1, -1, 1.5, -1.5, 0.5, -0.5, 0),
  sd = c(1, 0.5, 0.5, 0.5, 0.5, 0.25, 0.25, 0.25, 0.25, 0.5)
)
ds$lcl <- qnorm(0.05, ds$mean, ds$sd)
ds$ucl <- qnorm(0.95, ds$mean, ds$sd)

add_effect_classification(
 df = ds,
 cl_columns = c("lcl", "ucl"),
 threshold = 1,
 reference = 0,
 coarse = TRUE
)
#>    mean   sd         lcl         ucl effect_code effect_code_coarse
#> 1   0.0 1.00 -1.64485363  1.64485363           ?                  ?
#> 2   0.5 0.50 -0.32242681  1.32242681          ?+                  ?
#> 3  -0.5 0.50 -1.32242681  0.32242681          ?-                  ?
#> 4   1.0 0.50  0.17757319  1.82242681           +                  +
#> 5  -1.0 0.50 -1.82242681 -0.17757319           -                  -
#> 6   1.5 0.25  1.08878659  1.91121341          ++                  +
#> 7  -1.5 0.25 -1.91121341 -1.08878659          --                  -
#> 8   0.5 0.25  0.08878659  0.91121341          +~                  +
#> 9  -0.5 0.25 -0.91121341 -0.08878659          -~                  -
#> 10  0.0 0.50 -0.82242681  0.82242681           ~                  ~
#>                effect effect_coarse
#> 1             unknown       unknown
#> 2  potential increase       unknown
#> 3  potential decrease       unknown
#> 4            increase      increase
#> 5            decrease      decrease
#> 6     strong increase      increase
#> 7     strong decrease      decrease
#> 8   moderate increase      increase
#> 9   moderate decrease      decrease
#> 10             stable        stable
```
