# Basic diagnostic rules for data cubes

Returns basic diagnostic rules used by
[`diagnose_cube()`](https://b-cubed-eu.github.io/dubicube/reference/diagnose_cube.md).
Each rule defines how a specific data quality metric is computed and
evaluated.

## Usage

``` r
basic_cube_rules()
```

## Value

A list of diagnostic rule definitions.

## Details

Rules are implemented as lists containing:

- `id` – name of the diagnostic metric

- `dimension` – cube dimension being evaluated (e.g. temporal)

- `thresholds` – reference values used to determine severity

- `compute()` – function that calculates the metric

- `severity()` – function assigning a severity level

- [`message()`](https://rdrr.io/r/base/message.html) – function
  generating a human-readable message

Contains the following rules:

- [`rule_temporal_min_years()`](https://b-cubed-eu.github.io/dubicube/reference/rule_temporal_min_years.md):
  Number of years

- [`rule_temporal_missing_years()`](https://b-cubed-eu.github.io/dubicube/reference/rule_temporal_missing_years.md):
  Missing years

- [`rule_spatial_min_cells()`](https://b-cubed-eu.github.io/dubicube/reference/rule_spatial_min_cells.md):
  Number of grid cells

- [`rule_spatial_max_uncertainty()`](https://b-cubed-eu.github.io/dubicube/reference/rule_spatial_max_uncertainty.md):
  Number of records where coordinate uncertainty is larger than grid
  resolution

- `rule_spatial_miss_uncertainty`: Number of records with missing
  coordinate uncertainty

- [`rule_taxon_min_taxa()`](https://b-cubed-eu.github.io/dubicube/reference/rule_taxon_min_taxa.md):
  Number of taxa

- [`rule_obs_min_records()`](https://b-cubed-eu.github.io/dubicube/reference/rule_obs_min_records.md):
  Number of records (rows)

- [`rule_obs_min_total()`](https://b-cubed-eu.github.io/dubicube/reference/rule_obs_min_total.md):
  Total number of observations (sum)

Default thresholds are used.
