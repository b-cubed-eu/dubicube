#' Convert a list of `boot` objects to a tidy data frame
#'
#' This function converts a named list of `"boot"` objects
#' (typically produced by [bootstrap_cube()] into a single
#' long-format data frame. Each element of the list is assumed to correspond
#' to one group, with the list names defining the values of the grouping
#' variable.
#'
#' @param boot_list A named list of objects of class `"boot"`, as returned
#' by [boot::boot()]. Each list element must correspond to exactly
#' one group, and the list names are used as the values of the grouping
#' variable.
#' @param grouping_var A character string giving the name of the grouping
#' variable (e.g. `"year"`). This will be used as the column name in the
#' returned data frame.
#'
#' @returns A data frame with the following columns:
#'   - `sample`: Sample ID of the bootstrap replicate
#'   - `est_original`: The statistic based on the full dataset per group
#'   - `rep_boot`: The statistic based on a bootstrapped dataset (bootstrap
#'   replicate)
#'   - `est_boot`: The bootstrap estimate (mean of bootstrap replicates per
#'   group)
#'   - `se_boot`: The standard error of the bootstrap estimate (standard
#'   deviation of the bootstrap replicates per group)
#'   - `bias_boot`: The bias of the bootstrap estimate per group
#'
#' @details
#' This function is primarily intended for use with bootstrapping using the
#' [bootstrap_cube()] function generated with boot methods.
#'
#' The function assumes that each `boot` object in `boot_list`
#' contains a single bootstrap statistic per replicate (i.e. `boot$t`
#' is a vector or a one-column matrix).
#'
#' @export
#'
#' @family indicator_uncertainty_helper
#'
#' @import dplyr
#' @importFrom assertthat is.string
#' @importFrom stats sd
#'
#' @examples
#' \dontrun{
#' # After processing a data cube with b3gbi::process_cube()
#'
#' # Function to calculate statistic of interest
#' # Mean observations per year
#' mean_obs <- function(x) {
#'   out_df <- aggregate(obs ~ year, x, mean) # Calculate mean obs per year
#'   names(out_df) <- c("year", "diversity_val") # Rename columns
#'   return(out_df)
#' }
#' mean_obs(processed_cube$data)
#'
#' # Perform bootstrapping
#' bootstrap_mean_obs <- bootstrap_cube(
#'   data_cube = processed_cube,
#'   fun = mean_obs,
#'   grouping_var = "year",
#'   samples = 1000,
#'   method = "boot_group_specific",
#'   seed = 123
#' )
#'
#' bootstrap_df <- boot_list_to_dataframe(
#'   boot_list = bootstrap_mean_obs,
#'   grouping_var = "year"
#' )
#'
#' head(bootstrap_df)
#' }

boot_list_to_dataframe <- function(boot_list, grouping_var) {
  # Input checks
  stopifnot("`boot_list` must be a named list of `boot` objects." =
              is.list(boot_list))
  stopifnot("`boot_list` must be a named list of `boot` objects." =
              all(sapply(boot_list, inherits, "boot")))
  stopifnot(
    "`grouping_var` must be a scalar character vector." =
      assertthat::is.string(grouping_var)
  )
  # The names of the list define the grouping variable values (e.g. years)
  groups <- names(boot_list)
  if (is.null(groups)) {
    stop(
      "`boot_list` must be a named list; list names are used as values of `'",
      grouping_var,
      "'`."
    )
  }

  # Convert each boot object to a data frame
  out <- lapply(seq_along(boot_list), function(i) {

    # Extract the boot object and its corresponding group value
    boot <- boot_list[[i]]
    group_val <- groups[i]

    # Number of bootstrap replicates
    replicates <- boot$R

    # Bootstrap estimate: mean of bootstrap replications
    est_boot <- mean(boot$t)

    # Bootstrap standard error: sd of bootstrap replications
    se_boot <- stats::sd(boot$t)

    # Construct a long-format data frame:
    # one row per bootstrap replicate for this group
    data.frame(
      sample = seq_len(replicates),  # bootstrap replicate index
      group  = group_val,            # grouping variable value (from list name)
      est_original = boot$t0,        # statistic computed on original data
      rep_boot = as.vector(boot$t),  # bootstrap replicate values
      est_boot = est_boot,           # bootstrap estimate (repeated per row)
      se_boot = se_boot,             # bootstrap SE (repeated per row)
      bias_boot = est_boot - boot$t0 # bootstrap bias
    )
  })

  # Combine all groups into a single data frame
  out_df <- dplyr::bind_rows(out)

  # Rename the generic 'group' column to the user-specified grouping variable
  names(out_df)[names(out_df) == "group"] <- grouping_var

  return(out_df)
}
