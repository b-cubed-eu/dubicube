#' Calculate confidence intervals for a dataframe with bootstrap replicates
#'
#' This function calculates confidence intervals for a dataframe containing
#' bootstrap replicates based on different methods, including percentile
#' (`perc`), bias-corrected and accelerated (`bca`), normal (`norm`), and basic
#' (`basic`).
#'
#' @param data_cube A data cube object (class 'processed_cube' or 'sim_cube',
#' see `b3gbi::process_cube()`) or a dataframe (from `$data` slot of
#' 'processed_cube' or 'sim_cube'). As used by `bootstrap_cube()`.
#' @param grouping_var A string specifying the grouping variable(s) used for the
#' bootstrap analysis.
#' This variable is used to split the dataset into groups for separate
#' confidence interval calculations.
#' @param fun A function which, when applied to
#' `data_cube` returns the statistic(s) of interest. This function must return a
#' dataframe with a column `diversity_val` containing the statistic of interest.
#'  As used by `bootstrap_cube()`.
#' @param ref_group A string indicating the
#' reference group to compare the statistic with. Default is `NA`, meaning no
#' reference group is used. As used by `bootstrap_cube()`.
#' @param progress Logical. Whether to show a progress bar for jackknifing. Set
#' to `TRUE` to display a progress bar, `FALSE` (default) to suppress it.
#'
#' @returns A dataframe containing the jackknife estimates.
#'
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom tidyr expand_grid
#' @importFrom purrr map
#'
#' @noRd

perform_jackknifing <- function(
    data_cube,
    fun,
    grouping_var,
    ref_group = NA,
    progress = FALSE) {
  # Perform jackknifing
  if (inherits(data_cube, "processed_cube")) {
    jackknife_estimates <- purrr::map(
      seq_len(nrow(data_cube$data)),
      function(i) {
        # Identify group
        group <- data_cube$data[[i, grouping_var]]

        # Remove i'th observation
        data <- data_cube$data[-i, ]
        data_cube_copy <- data_cube
        data_cube_copy$data <- data

        # Calculate indicator value without i'th observation
        fun(data_cube_copy)$data %>%
          dplyr::filter(!!rlang::sym(grouping_var) == group) %>%
          dplyr::pull(.data$diversity_val)
      },
      .progress = ifelse(progress, "Jackknife estimation", progress)) %>%
      unlist()

    jackknife_df <- data_cube$data %>%
      dplyr::mutate(jack_rep = jackknife_estimates) %>%
      dplyr::select(dplyr::all_of(c(grouping_var, "jack_rep")))
  } else {
    jackknife_estimates <- purrr::map(
      seq_len(nrow(data_cube)),
      function(i) {
        # Identify group
        group <- data_cube[[i, grouping_var]]

        # Calculate indicator value without i'th observation
        fun(data_cube[-i, ]) %>%
          dplyr::filter(!!sym(grouping_var) == group) %>%
          dplyr::pull(.data$diversity_val)
      },
      .progress = ifelse(progress, "Jackknife estimation", progress)) %>%
      unlist()

    jackknife_df <- data_cube %>%
      dplyr::mutate(jack_rep = jackknife_estimates) %>%
      dplyr::select(dplyr::all_of(c(grouping_var, "jack_rep")))
  }

  # Calculate differences in presence of reference group
  if (!is.na(ref_group)) {
    # Get group-specific estimates
    if (inherits(data_cube, "processed_cube")) {
      group_estimates <- fun(data_cube)$data
    } else {
      group_estimates <- fun(data_cube)
    }

    # Get estimate for reference group
    ref_estimate <- group_estimates %>%
      dplyr::filter(.data[[grouping_var]] == ref_group) %>%
      dplyr::pull(.data$diversity_val)

    # Calculate jackknife estimates for difference for non-reference groups
    thetai_nonref <- jackknife_df %>%
      dplyr::filter(.data[[grouping_var]] != ref_group) %>%
      dplyr::mutate(theta2 = ref_estimate) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(jack_rep = .data$jack_rep - .data$theta2) %>%
      dplyr::ungroup()

    # Calculate jackknife estimates for difference for reference group
    thetai_ref <- tidyr::expand_grid(
      group_estimates %>%
        dplyr::filter(.data[[grouping_var]] != ref_group),
      jack_rep = jackknife_df %>%
        dplyr::filter(.data[[grouping_var]] == ref_group) %>%
        dplyr::pull(.data$jack_rep)
    ) %>%
      dplyr::rename("theta1" = "diversity_val") %>%
      dplyr::rowwise() %>%
      dplyr::mutate(jack_rep = .data$theta1 - .data$jack_rep) %>%
      dplyr::ungroup()

    # Combine all jackknife estimates
    jackknife_df <- dplyr::bind_rows(thetai_nonref, thetai_ref) %>%
      dplyr::select(-dplyr::starts_with("theta"))
  }

  return(jackknife_df)
}
