# Diagnose data quality of a processed data cube

Evaluates a set of diagnostic rules describing the data quality of a
biodiversity occurrence cube. Each rule computes a metric on the cube
and assigns a severity level indicating potential limitations of the
data for exploratory analysis or indicator calculation.

## Usage

``` r
diagnose_cube(data_cube, rules = "basic", verbose = TRUE, ...)
```

## Arguments

- data_cube:

  A `processed_cube` object as returned by
  [`b3gbi::process_cube()`](https://b-cubed-eu.github.io/b3gbi/reference/process_cube.html).

- rules:

  Diagnostic rules to evaluate. Can be:

  - A character vector referring to built-in rule sets (e.g. `"basic"`,
    `"spatial"`).

  - A list of rule objects.

  - A combination of both.

- verbose:

  Logical indicating whether a diagnostic summary should be printed.

- ...:

  Additional arguments passed to
  [`print.cube_diagnostics()`](https://b-cubed-eu.github.io/dubicube/reference/print.cube_diagnostics.md)
  in case `verbose = TRUE`.

## Value

An object of class `cube_diagnostics`, containing one row per metric
with the following columns:

- `dimension`: Dimension of the cube being evaluated (e.g. `"spatial"`,
  `"temporal"`, `"taxonomical"`).

- `metric`: Name of the diagnostic metric.

- `value`: Computed metric value.

- `severity`: Severity level (`"ok"`, `"note"`, `"important"`,
  `"very_important"`).

- `message`: Human-readable description of the diagnostic result.

The rule objects are attached as an attribute of the diagnostics object.

## See also

Other data_exploration:
[`filter_cube()`](https://b-cubed-eu.github.io/dubicube/reference/filter_cube.md)

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

# Diagnose based on default rules
diag <- diagnose_cube(processed_cube)
#> 
#> Data cube diagnostics
#> ----------------------
#> 🟡 NOTE - temporal_min_points 
#>    Cube contains observations across 3 years. 
#> 
#> 🟢 OK - temporal_missing_years 
#>    Cube contains 0 missing years. 
#> 
#> 🟠 IMPORTANT - spatial_min_cells 
#>    Cube contains observations across 0 grid cells. 
#> 
#> 🟢 OK - spatial_max_uncertainty 
#>    Cube contains 0 records where the coordinate uncertainty is larger than the grid cell resolution. 
#> 
#> 🟡 NOTE - spatial_miss_uncertainty 
#>    Cube contains 1 records with missing coordinate uncertainty. 
#> 
#> 🟠 IMPORTANT - taxon_min_taxa 
#>    Cube contains observations across 0 taxon keys. 
#> 
#> 🔴 VERY_IMPORTANT - obs_min_records 
#>    Cube contains 4 observation records (rows). 
#> 
#> 🔴 VERY_IMPORTANT - obs_min_total 
#>    Cube contains a total of 18 observations. 
#> 

# Sort diagnoses
diag <- diagnose_cube(processed_cube, sort_summary = "asc")
#> 
#> Data cube diagnostics
#> ----------------------
#> 🟢 OK - temporal_missing_years 
#>    Cube contains 0 missing years. 
#> 
#> 🟢 OK - spatial_max_uncertainty 
#>    Cube contains 0 records where the coordinate uncertainty is larger than the grid cell resolution. 
#> 
#> 🟡 NOTE - temporal_min_points 
#>    Cube contains observations across 3 years. 
#> 
#> 🟡 NOTE - spatial_miss_uncertainty 
#>    Cube contains 1 records with missing coordinate uncertainty. 
#> 
#> 🟠 IMPORTANT - spatial_min_cells 
#>    Cube contains observations across 0 grid cells. 
#> 
#> 🟠 IMPORTANT - taxon_min_taxa 
#>    Cube contains observations across 0 taxon keys. 
#> 
#> 🔴 VERY_IMPORTANT - obs_min_records 
#>    Cube contains 4 observation records (rows). 
#> 
#> 🔴 VERY_IMPORTANT - obs_min_total 
#>    Cube contains a total of 18 observations. 
#> 

# Only show at least important diagnoses
diag <- diagnose_cube(processed_cube, filter_summary = "important")
#> 
#> Data cube diagnostics
#> ----------------------
#> 🟠 IMPORTANT - spatial_min_cells 
#>    Cube contains observations across 0 grid cells. 
#> 
#> 🟠 IMPORTANT - taxon_min_taxa 
#>    Cube contains observations across 0 taxon keys. 
#> 
#> 🔴 VERY_IMPORTANT - obs_min_records 
#>    Cube contains 4 observation records (rows). 
#> 
#> 🔴 VERY_IMPORTANT - obs_min_total 
#>    Cube contains a total of 18 observations. 
#> 
```
