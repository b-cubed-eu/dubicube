#' Calculate confidence intervals from a 'boot' object
#'
#' This function calculates multiple types of confidence intervals
#' (normal, basic, percentile, BCa) for a \pkg{boot} object using
#' `boot::boot.ci()`.
#'
#' @param boot_obj A `boot` object (from the \pkg{boot} package).
#' @param type A character vector specifying the type(s) of confidence intervals
#' to compute. Options include:
#'   - `"perc"`: Percentile interval
#'   - `"bca"`: Bias-corrected and accelerated interval
#'   - `"norm"`: Normal interval
#'   - `"basic"`: Basic interval
#'   - `"all"`: Compute all available interval types (default)
#' @param conf A numeric value specifying the confidence level of the intervals.
#' Default is `0.95` (95 % confidence level).
#' @param h A function defining a transformation. The intervals are calculated
#' on the scale of `h(t)` and the inverse function `hinv` applied to the
#' resulting intervals. It must be a function of one variable only. The default
#' is the identity function.
#' @param hinv A function, like `h`, which returns the inverse of `h`. It is
#' used to transform the intervals calculated on the scale of `h(t)` back to the
#' original scale. The default is the identity function. If `h` is supplied but
#' `hinv` is not, then the intervals returned will be on the transformed scale.
#' @param boot_args Named list of additional arguments to pass to
#' `boot::boot.ci()`.
#'
#' @returns A tidy dataframe with columns:
#'   - `stat_index`: Index of statistic in the boot object
#'   - `est_original`: Original estimate from full dataset
#'   - `int_type`: Interval type
#'   - `ll`: Lower confidence limit
#'   - `ul`: Upper confidence limit
#'   - `conf`: Confidence level
#'
#' @export
#'
#' @family indicator_uncertainty_helper
#'
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom boot boot.ci

calculate_boot_ci_from_boot <- function(
    boot_obj,
    type = c("norm", "basic", "perc", "bca"),
    conf = 0.95,
    h = function(t) t,
    hinv = function(t) t,
    boot_args = list()) {
  # Determine which CI types to calculate
  ci_types <- if (any(type == "all")) {
    c("norm", "basic", "perc", "bca")
  } else {
    type
  }

  n_stats <- length(boot_obj$t0)

  # Compute CIs for each statistic
  ci_out <- lapply(seq_len(n_stats), function(idx) {
    res <- do.call(
      boot::boot.ci,
      c(
        list(
          boot.out = boot_obj,
          conf = conf,
          type = ci_types,
          index = idx,
          t0 = boot_obj$t0[idx],
          t = boot_obj$t[, idx, drop = TRUE],
          h = h,
          hinv = hinv
        ),
        boot_args
      )
    )

    # Convert results to tidy dataframe
    data.frame(
      stat_index = idx,
      est_original = rep(boot_obj$t0[idx], length(ci_types)),
      int_type = names(res)[
        names(res) %in%c("normal", "basic", "percent", "bca")
      ],
      ll = sapply(res[names(res) %in% c("normal", "basic", "percent", "bca")],
                  function(x) x[length(x) - 1]),
      ul = sapply(res[names(res) %in% c("normal", "basic", "percent", "bca")],
                  function(x) x[length(x)]),
      conf = conf
    ) %>%
      dplyr::mutate(
        int_type = dplyr::case_when(
          .data$int_type == "percent" ~ "perc",
          .data$int_type == "normal"  ~ "norm",
          TRUE ~ .data$int_type
        )
      )
  }) %>%
    dplyr::bind_rows()

  rownames(ci_out) <- NULL
  return(ci_out)
}
