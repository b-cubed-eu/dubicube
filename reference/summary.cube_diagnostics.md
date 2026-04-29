# Summarise cube diagnostics

Provides a summary of diagnostic results returned by
[`diagnose_cube()`](https://b-cubed-eu.github.io/dubicube/reference/diagnose_cube.md).
The summary reports the number of evaluated rules, counts per severity
level, and the number of diagnostics per cube dimension.

## Usage

``` r
# S3 method for class 'cube_diagnostics'
summary(object, ...)
```

## Arguments

- object:

  A `cube_diagnostics` object returned by
  [`diagnose_cube()`](https://b-cubed-eu.github.io/dubicube/reference/diagnose_cube.md).

- ...:

  Additional arguments passed to other methods (currently unused).

## Value

An object of class `summary_cube_diagnostics`, containing aggregated
diagnostic information.

## See also

Other diagnostic_methods:
[`plot.cube_diagnostics()`](https://b-cubed-eu.github.io/dubicube/reference/plot.cube_diagnostics.md),
[`print.cube_diagnostics()`](https://b-cubed-eu.github.io/dubicube/reference/print.cube_diagnostics.md)
