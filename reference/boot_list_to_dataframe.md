# Convert a list of `boot` objects to a tidy dataframe

This function converts a named list of `"boot"` objects (typically
produced by
[`bootstrap_cube()`](https://b-cubed-eu.github.io/dubicube/reference/bootstrap_cube.md)
into a single long-format dataframe. Each element of the list is assumed
to correspond to one group, with the list names defining the values of
the grouping variable.

## Usage

``` r
boot_list_to_dataframe(boot_list, grouping_var)
```

## Arguments

- boot_list:

  A named list of objects of class `"boot"`, as returned by
  [`boot::boot()`](https://rdrr.io/pkg/boot/man/boot.html). Each list
  element must correspond to exactly one group, and the list names are
  used as the values of the grouping variable.

- grouping_var:

  A character string giving the name of the grouping variable (e.g.
  `"year"`). This will be used as the column name in the returned
  dataframe.

## Value

A dataframe with the following columns:

- `sample`: Sample ID of the bootstrap replicate

- `est_original`: The statistic based on the full dataset per group

- `rep_boot`: The statistic based on a bootstrapped dataset (bootstrap
  replicate)

- `est_boot`: The bootstrap estimate (mean of bootstrap replicates per
  group)

- `se_boot`: The standard error of the bootstrap estimate (standard
  deviation of the bootstrap replicates per group)

- `bias_boot`: The bias of the bootstrap estimate per group

## Details

This function is primarily intended for use with bootstrapping using the
[`bootstrap_cube()`](https://b-cubed-eu.github.io/dubicube/reference/bootstrap_cube.md)
function generated with boot methods.

The function assumes that each `boot` object in `boot_list` contains a
single bootstrap statistic per replicate (i.e. `boot$t` is a vector or a
one-column matrix).

## See also

Other indicator_uncertainty_helper:
[`bootstrap_cube_raw()`](https://b-cubed-eu.github.io/dubicube/reference/bootstrap_cube_raw.md),
[`calculate_boot_ci_from_boot()`](https://b-cubed-eu.github.io/dubicube/reference/calculate_boot_ci_from_boot.md),
[`derive_bootstrap_method()`](https://b-cubed-eu.github.io/dubicube/reference/derive_bootstrap_method.md),
[`resolve_bootstrap_method()`](https://b-cubed-eu.github.io/dubicube/reference/resolve_bootstrap_method.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# After processing a data cube with b3gbi::process_cube()

# Function to calculate statistic of interest
# Mean observations per year
mean_obs <- function(x) {
  out_df <- aggregate(obs ~ year, x, mean) # Calculate mean obs per year
  names(out_df) <- c("year", "diversity_val") # Rename columns
  return(out_df)
}
mean_obs(processed_cube$data)

# Perform bootstrapping
bootstrap_mean_obs <- bootstrap_cube(
  data_cube = processed_cube,
  fun = mean_obs,
  grouping_var = "year",
  samples = 1000,
  method = "boot_group_specific",
  seed = 123
)

bootstrap_df <- boot_list_to_dataframe(
  boot_list = bootstrap_mean_obs,
  grouping_var = "year"
)

head(bootstrap_df)
} # }
```
