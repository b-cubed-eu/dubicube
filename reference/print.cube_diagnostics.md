# Print cube diagnostics

Displays a human-readable summary of data cube diagnostics produced by
[`diagnose_cube()`](https://b-cubed-eu.github.io/dubicube/reference/diagnose_cube.md).
Each diagnostic metric is shown with a severity flag, the metric name,
and a short explanatory message.

## Usage

``` r
# S3 method for class 'cube_diagnostics'
print(x, filter_summary = "ok", sort_summary = NA, ...)
```

## Arguments

- x:

  A `cube_diagnostics` object returned by
  [`diagnose_cube()`](https://b-cubed-eu.github.io/dubicube/reference/diagnose_cube.md).

- filter_summary:

  Filter the summary output based on a minimum severity level. Default,
  all levels are shown: `filter_summary = "ok"`.

- sort_summary:

  Sort the summary output based on severity level. Options are
  descending (`"desc"`), ascending (`"asc"`) or no sorting (`NA`,
  default).

- ...:

  Additional arguments passed to other methods (currently unused).

## Value

The input object `x`, returned invisibly.

## Details

Severity levels are indicated using coloured symbols:

- green ball: ok

- yellow ball: note

- orange ball: important

- red ball: very important

## See also

Other diagnostic_methods:
[`plot.cube_diagnostics()`](https://b-cubed-eu.github.io/dubicube/reference/plot.cube_diagnostics.md),
[`summary.cube_diagnostics()`](https://b-cubed-eu.github.io/dubicube/reference/summary.cube_diagnostics.md)
