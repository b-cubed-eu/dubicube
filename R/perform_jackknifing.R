#' Calculate jackknife estimates for a dataframe with bootstrap replicates
#'
#' This function jackknife estimates for a dataframe containing
#' bootstrap replicates per group. The output is used to calculate the
#' acceleration `calculate_acceleration()` which can be used for BCa interval
#' calculation `calculate_bootstrap_ci()`.
#'
#' @param data_cube A data cube object (class 'processed_cube' or 'sim_cube',
#' see `b3gbi::process_cube()`) or a dataframe (from `$data` slot of
#' 'processed_cube' or 'sim_cube'). As used by `bootstrap_cube()`.
#' @param fun A function which, when applied to
#' `data_cube` returns the statistic(s) of interest. This function must return a
#' dataframe with a column `diversity_val` containing the statistic of interest.
#'  As used by `bootstrap_cube()`.
#' @param ... Additional arguments passed on to `fun`.
#' @param grouping_var A character vector specifying the grouping variable(s)
#' for the bootstrap analysis. The function `fun(data_cube, ...)` should return
#' a row per group. The specified variables must not be redundant, meaning they
#' should not contain the same information (e.g., `"time_point"` (1, 2, 3) and
#' `"year"` (2000, 2001, 2002) should not be used together if `"time_point"` is
#' just an alternative encoding of `"year"`).
#' This variable is used to split the dataset into groups for separate
#' jackknife calculations.
#' @param ref_group A string indicating the
#' reference group to compare the statistic with. Default is `NA`, meaning no
#' reference group is used. As used by `bootstrap_cube()`.
#' @param progress Logical. Whether to show a progress bar for jackknifing. Set
#' to `TRUE` to display a progress bar, `FALSE` (default) to suppress it.
#'
#' @returns A dataframe containing the jackknife estimates.
#'
#' @import dplyr
#' @importFrom rlang .data inherits_any
#' @importFrom tidyr expand_grid
#' @importFrom purrr map
#'
#' @noRd

perform_jackknifing <- function(
    data_cube,
    fun,
    ...,
    grouping_var,
    ref_group = NA,
    progress = FALSE) {
  # Perform jackknifing
  if (rlang::inherits_any(data_cube, c("processed_cube", "sim_cube"))) {
    # Check if grouping_var column is present in data cube
    stopifnot("`data_cube` should contain column(s) `grouping_var`." =
                all(grouping_var %in% names(data_cube$data)))

    # Check if ref_group is present in grouping_var
    stopifnot(
      "`ref_group` is not present in `grouping_var` column of `data_cube`." =
        is.na(ref_group) |
        any(
          sapply(
            as.list(grouping_var), function(var) {
              ref_group %in% data_cube$data[[var]]
            })
        )
    )

    jackknife_estimates <- purrr::map(
      seq_len(nrow(data_cube$data)),
      function(i) {
        # Identify group
        group <- data_cube$data[i, ] %>%
          select(all_of(grouping_var))

        # Remove i'th observation
        data <- data_cube$data[-i, ]
        data_cube_copy <- data_cube
        data_cube_copy$data <- data

        # Calculate indicator value without i'th observation
        fun(data_cube_copy, ...)$data %>%
          dplyr::inner_join(group, by = grouping_var) %>%
          dplyr::pull(.data$diversity_val)
      },
      .progress = ifelse(progress, "Jackknife estimation", progress)) %>%
      unlist()

    jackknife_df <- data_cube$data %>%
      dplyr::mutate(jack_rep = jackknife_estimates) %>%
      dplyr::select(dplyr::all_of(grouping_var), "jack_rep")
  } else {
    # Check if ref_group is present in grouping_var
    stopifnot(
      "`ref_group` is not present in `grouping_var` column of `data_cube`." =
        is.na(ref_group) |
        any(
          sapply(
            as.list(grouping_var), function(var) {
              ref_group %in% data_cube[[var]]
            })
        )
    )

    jackknife_estimates <- purrr::map(
      seq_len(nrow(data_cube)),
      function(i) {
        # Identify group
        group <- data_cube[i, ] %>%
          select(all_of(grouping_var))

        # Calculate indicator value without i'th observation
        fun(data_cube[-i, ], ...) %>%
          dplyr::inner_join(group, by = grouping_var) %>%
          dplyr::pull(.data$diversity_val)
      },
      .progress = ifelse(progress, "Jackknife estimation", progress)) %>%
      unlist()

    jackknife_df <- data_cube %>%
      dplyr::mutate(jack_rep = jackknife_estimates) %>%
      dplyr::select(dplyr::all_of(grouping_var), "jack_rep")
  }

  # Calculate differences in presence of reference group
  if (!is.na(ref_group)) {
    # Get group-specific estimates
    if (inherits(data_cube, "processed_cube")) {
      # Check if ref_group is present in grouping_var
      matching_col <- grouping_var[
        sapply(data_cube$data %>% dplyr::select(all_of(grouping_var)),
               function(col) ref_group %in% col)]

      stopifnot(
        "`ref_group` is not present in `grouping_var` column of `data_cube`." =
          is.na(ref_group) | ref_group %in% data_cube$data[[matching_col]]
      )

      group_estimates <- fun(data_cube, ...)$data
    } else {
      # Check if ref_group is present in grouping_var
      matching_col <- grouping_var[
        sapply(data_cube %>% dplyr::select(all_of(grouping_var)),
               function(col) ref_group %in% col)]

      stopifnot(
        "`ref_group` is not present in `grouping_var` column of `data_cube`." =
          is.na(ref_group) | ref_group %in% data_cube[[matching_col]]
      )

      group_estimates <- fun(data_cube, ...)
    }

    # Get estimate for reference group
    ref_val <- group_estimates %>%
      dplyr::filter(.data[[matching_col]] == !!ref_group) %>%
      dplyr::rename("theta2" = "diversity_val") %>%
      dplyr::select(-matching_col)

    # Calculate jackknife estimates for difference for non-reference groups
    thetai_nonref <- jackknife_df %>%
      dplyr::filter(.data[[matching_col]] != ref_group) %>%
      dplyr::left_join(ref_val, by = setdiff(grouping_var, matching_col)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(jack_rep = .data$jack_rep - .data$theta2) %>%
      dplyr::ungroup()

    # Calculate jackknife estimates for difference for reference group
    non_ref_val <- group_estimates %>%
      dplyr::filter(.data[[matching_col]] != !!ref_group) %>%
      dplyr::rename("theta1" = "diversity_val")

    thetai_ref <- jackknife_df %>%
      dplyr::filter(.data[[matching_col]] == ref_group) %>%
      dplyr::select(-matching_col) %>%
      dplyr::right_join(non_ref_val,
                        by = setdiff(grouping_var, matching_col)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(jack_rep = .data$theta1 - .data$jack_rep) %>%
      dplyr::ungroup()

    # Combine all jackknife estimates
    jackknife_df <- dplyr::bind_rows(thetai_nonref, thetai_ref) %>%
      dplyr::select(-dplyr::starts_with("theta"))
  }

  return(jackknife_df)
}
