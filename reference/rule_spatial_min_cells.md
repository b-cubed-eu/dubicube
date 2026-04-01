# Spatial minimum grid cells diagnostic rule

Creates a diagnostic rule that evaluates whether a data cube contains a
sufficient number of spatial observations (grid cells). The rule counts
the number of unique grid cells present in the cube and compares it to a
threshold to determine the severity level.

## Usage

``` r
rule_spatial_min_cells(
  thresholds = c(ok = 5, note = 3, important = 0, very_important = NULL)
)
```

## Arguments

- thresholds:

  Named numeric vector with severity thresholds: ok, note, important,
  very_important. Defaults are used if not provided.

## Value

An object of class `cube_rule`.

## See also

Other diagnostic_rules:
[`rule_obs_min_records()`](https://b-cubed-eu.github.io/dubicube/reference/rule_obs_min_records.md),
[`rule_obs_min_total()`](https://b-cubed-eu.github.io/dubicube/reference/rule_obs_min_total.md),
[`rule_spatial_max_uncertainty()`](https://b-cubed-eu.github.io/dubicube/reference/rule_spatial_max_uncertainty.md),
[`rule_spatial_miss_uncertainty()`](https://b-cubed-eu.github.io/dubicube/reference/rule_spatial_miss_uncertainty.md),
[`rule_taxon_min_taxa()`](https://b-cubed-eu.github.io/dubicube/reference/rule_taxon_min_taxa.md),
[`rule_temporal_min_years()`](https://b-cubed-eu.github.io/dubicube/reference/rule_temporal_min_years.md),
[`rule_temporal_missing_years()`](https://b-cubed-eu.github.io/dubicube/reference/rule_temporal_missing_years.md)
