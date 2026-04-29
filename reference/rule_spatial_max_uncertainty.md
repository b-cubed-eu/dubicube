# Maximal coordinate uncertainty diagnostic rule

Creates a diagnostic rule that evaluates whether a data cube contains a
records with high coordinate uncertainty. The rule counts the number of
records (rows) in the cube where the minimal coordinate uncertainty is
larger than the resolution of the grid, and compares it to a threshold
to determine the severity level.

## Usage

``` r
rule_spatial_max_uncertainty(
  thresholds = c(ok = 0, note = 1, important = 3, very_important = 5)
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
[`rule_spatial_min_cells()`](https://b-cubed-eu.github.io/dubicube/reference/rule_spatial_min_cells.md),
[`rule_spatial_miss_uncertainty()`](https://b-cubed-eu.github.io/dubicube/reference/rule_spatial_miss_uncertainty.md),
[`rule_taxon_min_taxa()`](https://b-cubed-eu.github.io/dubicube/reference/rule_taxon_min_taxa.md),
[`rule_temporal_min_years()`](https://b-cubed-eu.github.io/dubicube/reference/rule_temporal_min_years.md),
[`rule_temporal_missing_years()`](https://b-cubed-eu.github.io/dubicube/reference/rule_temporal_missing_years.md)
