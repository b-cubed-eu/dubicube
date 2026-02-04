#' Normalize and dispatch bootstrap input
#'
#' Internal helper that standardizes bootstrap input supplied to
#' [calculate_bootstrap_ci()] and handles early dispatch for `boot` objects.
#' It contains no statistical logic itself, but controls workflow routing.
#'
#' @param bootstrap_results A dataframe with bootstrap replicates, a single
#' `boot` object, or a list of `boot` objects.
#' @param grouping_var Optional character vector of grouping variables used
#' to identify indicators or strata.
#' @param type Character vector specifying the type(s) of confidence interval
#' to compute (e.g. `"perc"`, `"bca"`, `"norm"`, `"basic"`).
#' @param conf Numeric confidence level between 0 and 1.
#' @param h Optional transformation function applied before CI calculation.
#' @param hinv Optional inverse transformation function.
#' @param no_bias Logical scalar. If `TRUE`, bias correction is skipped and
#' `boot` objects are converted to a dataframe. If `FALSE`, confidence
#' intervals are computed directly from the `boot` objects.
#' @param boot_args Optional list of additional arguments passed to
#' `boot::boot.ci()`.
#'
#' @return A list with two elements:
#'   - `data`: A dataframe of bootstrap replicates, or `NULL` if confidence
#'   intervals were computed directly
#'   - `result`: A dataframe of confidence intervals, or `NULL` if the
#'   dataframe workflow should continue.
#'
#' @details
#' The function supports three input modes:
#' \describe{
#'   \item{Data frame input}{Returned unchanged and passed on to the
#'   dataframe-based CI workflow.}
#'   \item{`boot` input with `no_bias = TRUE`}{`boot` objects are converted
#'   to a dataframe of bootstrap replicates and passed on to the dataframe-based
#'   CI workflow.}
#'   \item{`boot` input with `no_bias = FALSE`}{Confidence intervals are
#'   computed directly using `boot::boot.ci()` logic and returned immediately.}
#' }
#'
#' @export
#'
#' @family indicator_uncertainty_helper
#'
#' @importFrom dplyr bind_rows
#' @importFrom assertthat is.flag

process_boot_input <- function(
  bootstrap_results,
  grouping_var,
  type,
  conf,
  h,
  hinv,
  no_bias,
  boot_args
) {
  # Validate bias-correction flag early to avoid ambiguous control flow
  stopifnot("`no_bias` must be a logical vector of length 1." =
              assertthat::is.flag(no_bias))

  # Normalize single `boot` object to a list for uniform downstream handling
  if (inherits(bootstrap_results, "boot")) {
    bootstrap_results <- list(bootstrap_results)
  }

  # Detect whether input consists exclusively of `boot` objects
  is_boot_input <- is.list(bootstrap_results) &&
    all(vapply(bootstrap_results, inherits, logical(1), "boot"))

  # Non-boot input: must be a dataframe and is passed through unchanged
  if (!is_boot_input) {
    stopifnot(
      "`bootstrap_results` must be a dataframe or a (list of) boot object(s)." =
        inherits(bootstrap_results, "data.frame")
    )

    return(list(
      data = bootstrap_results,
      result = NULL
    ))
  }

  # `boot` input with no bias correction:
  # convert to a dataframe of bootstrap replicates and continue
  if (no_bias) {
    return(list(
      data = boot_list_to_dataframe(
        boot_list = bootstrap_results,
        grouping_var = grouping_var
      ),
      result = NULL
    ))
  }

  # `boot` input with bias correction:
  # compute confidence intervals directly and stop further processing
  ci_list <- lapply(seq_along(bootstrap_results), function(i) {

    # Compute confidence intervals for a single `boot` object
    ci <- calculate_boot_ci_from_boot(
      boot_obj = bootstrap_results[[i]],
      type = type,
      conf = conf,
      h = h,
      hinv = hinv,
      boot_args = boot_args
    )

    # Ensure stat index and grouping variables are consistent
    ci <- assign_stat_index(
      df = ci,
      index = i,
      names = names(bootstrap_results),
      grouping_var = grouping_var
    )

    return(ci)
  })

  list(
    data = NULL,
    result = dplyr::bind_rows(ci_list)
  )
}
