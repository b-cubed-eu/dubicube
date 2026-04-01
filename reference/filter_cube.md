# Filter a processed data cube using diagnostic rules

Filters observations from a `processed_cube` based on rule definitions.
Filtering reuses the rule infrastructure used by
[`diagnose_cube()`](https://b-cubed-eu.github.io/dubicube/reference/diagnose_cube.md),
but applies row-level filtering logic through rule-specific
`filter_fn()` functions.

## Usage

``` r
filter_cube(data_cube, rules = NULL, diagnostics = NULL, ...)
```

## Arguments

- data_cube:

  A `processed_cube` object as returned by
  [`b3gbi::process_cube()`](https://b-cubed-eu.github.io/b3gbi/reference/process_cube.html).

- rules:

  Character vector or list of cube rule objects. Ignored if
  `diagnostics` is supplied.

- diagnostics:

  Optional `cube_diagnostics` object returned by
  [`diagnose_cube()`](https://b-cubed-eu.github.io/dubicube/reference/diagnose_cube.md).
  If provided, rules are extracted from this object.

- ...:

  Additional arguments passed to rule-specific `filter_fn()` functions.

## Value

A filtered `processed_cube`.

## Details

The function evaluates rule-specific `filter_fn()` functions that return
a logical vector indicating which rows should be removed. Only rules
that implement a `filter_fn()` are applied. Rules without a filtering
function are ignored.

Filtering rules operate independently from diagnostic severity levels.
For example, a cube may have acceptable overall diagnostics while still
containing individual observations that fail filtering criteria.

After filtering, the function attempts to rebuild the cube using
[`b3gbi::process_cube()`](https://b-cubed-eu.github.io/b3gbi/reference/process_cube.html)
to ensure cube metadata remains consistent. If this function is
unavailable or fails, the filtered data replaces `data_cube$data`
directly and the original cube metadata is retained. In that case a
warning is issued.

## See also

Other data_exploration:
[`diagnose_cube()`](https://b-cubed-eu.github.io/dubicube/reference/diagnose_cube.md)

## Examples

``` r
# Example cube
# ! Real cubes should be processed with b3gbi::process_cube()
processed_cube <- list(
  data = data.frame(
    obs = c(5, 2, 10, 1),
    year = c(2001, 2001, 2002, 2003),
    minCoordinateUncertaintyInMeters = c(50, 2000, NA, 10)
  ),
  resolutions = "10km"
)
class(processed_cube) <- "processed_cube"

# Filter cube based on rule
filtered_cube1 <- filter_cube(
  processed_cube,
  rules = list(rule_spatial_miss_uncertainty())
)
#> Warning: Filtered data replaced the cube data but metadata was not recomputed.
#> Install and use `b3gbi::process_cube()` to rebuild cube metadata.

# Filter cube based cube diagnostics
diag <- diagnose_cube(
  processed_cube,
  rules = list(
    rule_spatial_miss_uncertainty(),
    rule_temporal_missing_years()
  )
)
#> 
#> Data cube diagnostics
#> ----------------------
#> 🟡 NOTE - spatial_miss_uncertainty 
#>    Cube contains 1 records with missing coordinate uncertainty. 
#> 
#> 🟢 OK - temporal_missing_years 
#>    Cube contains 0 missing years. 
#> 

filtered_cube2 <- filter_cube(
  processed_cube,
  diagnostics = diag
)
#> Warning: Filtered data replaced the cube data but metadata was not recomputed.
#> Install and use `b3gbi::process_cube()` to rebuild cube metadata.

# The results are identical
identical(filtered_cube1$data, filtered_cube2$data)
#> [1] TRUE
```
