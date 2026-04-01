# Plot cube diagnostics

Visualises diagnostic results returned by
[`diagnose_cube()`](https://b-cubed-eu.github.io/dubicube/reference/diagnose_cube.md).
The plot summarises the number of diagnostics per severity level and
cube dimension.

## Usage

``` r
# S3 method for class 'cube_diagnostics'
plot(x, type = "severity", ...)
```

## Arguments

- x:

  A `cube_diagnostics` object returned by
  [`diagnose_cube()`](https://b-cubed-eu.github.io/dubicube/reference/diagnose_cube.md).

- type:

  Type of plot. Options are `"severity"` (default), `"dimension"`, or
  `"heatmap"`.

- ...:

  Additional arguments passed to other methods (currently unused).

## Value

A `ggplot` object.

## Details

Three visualisations are supported:

- `"severity"`: Number of diagnostics per severity level.

- `"dimension"`: Diagnostics grouped by cube dimension.

- `"heatmap"`: Severity levels per diagnostic rule and dimension.

## See also

Other diagnostic_methods:
[`print.cube_diagnostics()`](https://b-cubed-eu.github.io/dubicube/reference/print.cube_diagnostics.md),
[`summary.cube_diagnostics()`](https://b-cubed-eu.github.io/dubicube/reference/summary.cube_diagnostics.md)
