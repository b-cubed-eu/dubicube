# Package index

## 1 Data variability and exploration

##   Diagnostics and filtering

- [`diagnose_cube()`](https://b-cubed-eu.github.io/dubicube/reference/diagnose_cube.md)
  : Diagnose data quality of a processed data cube
- [`filter_cube()`](https://b-cubed-eu.github.io/dubicube/reference/filter_cube.md)
  : Filter a processed data cube using diagnostic rules

##   Sensitivity analysis

- [`cross_validate_cube()`](https://b-cubed-eu.github.io/dubicube/reference/cross_validate_cube.md)
  : Leave-one-out cross-validation for data cubes

## 2 Indicator uncertainty and interpretation

##   Bootstrapping and confidence interval calculation

- [`bootstrap_cube()`](https://b-cubed-eu.github.io/dubicube/reference/bootstrap_cube.md)
  : Perform bootstrapping over a data cube for a calculated statistic
- [`calculate_bootstrap_ci()`](https://b-cubed-eu.github.io/dubicube/reference/calculate_bootstrap_ci.md)
  : Calculate confidence intervals from bootstrap results

##   Effect classification

- [`add_effect_classification()`](https://b-cubed-eu.github.io/dubicube/reference/add_effect_classification.md)
  : Add effect classifications to a dataframe by comparing the
  confidence intervals with a reference and thresholds

## 3 Diagnostic utilities

##   Diagnostic rule groups

- [`basic_cube_rules()`](https://b-cubed-eu.github.io/dubicube/reference/basic_cube_rules.md)
  : Basic diagnostic rules for data cubes

##   Diagnostic rules

- [`rule_obs_min_records()`](https://b-cubed-eu.github.io/dubicube/reference/rule_obs_min_records.md)
  : Minimum number of records diagnostic rule
- [`rule_obs_min_total()`](https://b-cubed-eu.github.io/dubicube/reference/rule_obs_min_total.md)
  : Minimum total number of observations diagnostic rule
- [`rule_spatial_max_uncertainty()`](https://b-cubed-eu.github.io/dubicube/reference/rule_spatial_max_uncertainty.md)
  : Maximal coordinate uncertainty diagnostic rule
- [`rule_spatial_min_cells()`](https://b-cubed-eu.github.io/dubicube/reference/rule_spatial_min_cells.md)
  : Spatial minimum grid cells diagnostic rule
- [`rule_spatial_miss_uncertainty()`](https://b-cubed-eu.github.io/dubicube/reference/rule_spatial_miss_uncertainty.md)
  : Missing coordinate uncertainty diagnostic rule
- [`rule_taxon_min_taxa()`](https://b-cubed-eu.github.io/dubicube/reference/rule_taxon_min_taxa.md)
  : Taxonomic minimum taxa diagnostic rule
- [`rule_temporal_min_years()`](https://b-cubed-eu.github.io/dubicube/reference/rule_temporal_min_years.md)
  : Temporal minimum years diagnostic rule
- [`rule_temporal_missing_years()`](https://b-cubed-eu.github.io/dubicube/reference/rule_temporal_missing_years.md)
  : Temporal gaps diagnostic rule

##   Diagnostic methods

- [`plot(`*`<cube_diagnostics>`*`)`](https://b-cubed-eu.github.io/dubicube/reference/plot.cube_diagnostics.md)
  : Plot cube diagnostics
- [`print(`*`<cube_diagnostics>`*`)`](https://b-cubed-eu.github.io/dubicube/reference/print.cube_diagnostics.md)
  : Print cube diagnostics
- [`summary(`*`<cube_diagnostics>`*`)`](https://b-cubed-eu.github.io/dubicube/reference/summary.cube_diagnostics.md)
  : Summarise cube diagnostics

## 4 Bootstrap utilities

##   Bootstrap helpers

- [`boot_list_to_dataframe()`](https://b-cubed-eu.github.io/dubicube/reference/boot_list_to_dataframe.md)
  : Convert a list of 'boot' objects to a tidy dataframe
- [`bootstrap_cube_raw()`](https://b-cubed-eu.github.io/dubicube/reference/bootstrap_cube_raw.md)
  : Perform bootstrapping over a dataframe for a calculated statistic
- [`calculate_acceleration()`](https://b-cubed-eu.github.io/dubicube/reference/calculate_acceleration.md)
  : Calculate acceleration for a statistic in a dataframe
- [`calculate_boot_ci_from_boot()`](https://b-cubed-eu.github.io/dubicube/reference/calculate_boot_ci_from_boot.md)
  : Calculate confidence intervals from a 'boot' object
- [`resolve_bootstrap_method()`](https://b-cubed-eu.github.io/dubicube/reference/resolve_bootstrap_method.md)
  : Resolve bootstrap method including use of the boot package

##   Bootstrap interval types

- [`basic_ci()`](https://b-cubed-eu.github.io/dubicube/reference/basic_ci.md)
  : Calculate basic bootstrap confidence interval
- [`bca_ci()`](https://b-cubed-eu.github.io/dubicube/reference/bca_ci.md)
  : Calculate Bias-Corrected and Accelerated (BCa) bootstrap confidence
  interval
- [`norm_ci()`](https://b-cubed-eu.github.io/dubicube/reference/norm_ci.md)
  : Calculate normal bootstrap confidence interval
- [`perc_ci()`](https://b-cubed-eu.github.io/dubicube/reference/perc_ci.md)
  : Calculate percentile bootstrap confidence interval
