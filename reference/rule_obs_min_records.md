# Minimum number of records diagnostic rule

Creates a diagnostic rule that evaluates whether a data cube contains a
sufficient number of observation records (rows). The rule counts the
number of records present in the cube and compares it to a threshold to
determine the severity level.

## Usage

``` r
rule_obs_min_records(
  thresholds = c(ok = 40, note = 30, important = 20, very_important = 0)
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
[`rule_obs_min_total()`](https://b-cubed-eu.github.io/dubicube/reference/rule_obs_min_total.md),
[`rule_spatial_max_uncertainty()`](https://b-cubed-eu.github.io/dubicube/reference/rule_spatial_max_uncertainty.md),
[`rule_spatial_min_cells()`](https://b-cubed-eu.github.io/dubicube/reference/rule_spatial_min_cells.md),
[`rule_spatial_miss_uncertainty()`](https://b-cubed-eu.github.io/dubicube/reference/rule_spatial_miss_uncertainty.md),
[`rule_taxon_min_taxa()`](https://b-cubed-eu.github.io/dubicube/reference/rule_taxon_min_taxa.md),
[`rule_temporal_min_years()`](https://b-cubed-eu.github.io/dubicube/reference/rule_temporal_min_years.md),
[`rule_temporal_missing_years()`](https://b-cubed-eu.github.io/dubicube/reference/rule_temporal_missing_years.md)
