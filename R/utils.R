#' Check for redundant grouping variables
#'
#' This function checks whether any of the specified grouping variables contain
#' redundant information. Two grouping variables are considered redundant if
#' there is a one-to-one mapping between them.
#'
#' @param data A dataframe containing the grouping variables.
#' @param grouping_var A character vector specifying the names of the grouping
#' variables.
#'
#' @return Returns `TRUE` if no redundancy is found. Otherwise, it throws an
#' error.
#'
#' @noRd
#'
#' @import dplyr
#' @importFrom rlang .data inherits_any
#' @importFrom stats setNames
#'
#' @examples
#' years <- 2000:2002
#' df <- data.frame(year = rep(years, 2),
#'                  time_point = rep(seq_along(years), 2),
#'                  region = rep(c("A", "B", "C"), each = 2))
#' # Will throw an error
#' check_redundant_grouping_vars(df, c("year", "time_point"))
#' # No error
#' check_redundant_grouping_vars(df, c("year", "region"))

check_redundant_grouping_vars <- function(data, grouping_var) {
  if (rlang::inherits_any(data, c("processed_cube", "sim_cube"))) {
    data <- data$data
  }

  if (length(grouping_var) > 1) {
    # Subset the data to keep only the specified grouping variables
    grouping_data <- data[, grouping_var, drop = FALSE]

    # Loop over all pairs of grouping variables
    for (i in seq_along(grouping_var)) {
      for (j in seq_along(grouping_var)) {
        if (i != j) { # Avoid self-comparisons

          # Check for a one-to-one mapping between the two variables
          mapping_check <- grouping_data %>%
            dplyr::select(grouping_var[i], grouping_var[j]) %>%
            dplyr::distinct() %>%
            dplyr::count(.data[[grouping_var[i]]]) %>%
            dplyr::pull(n) %>%
            unique()

          # If there is only one unique count and it equals 1, then the
          # variables are redundant
          error_message <- paste0(
            "Grouping variables '", grouping_var[i], "' and '",
            grouping_var[j], "' contain redundant information. ",
            "Please use only one of them."
          )

          do.call(stopifnot,
                  stats::setNames(list(
                    !(length(mapping_check) == 1 && mapping_check == 1)),
                    error_message)
          )
        }
      }
    }
  }
}

#' Calculate Statistic Per Group
#'
#' This function calculates a specified statistic on a dataset, grouped by one
#' or more grouping variables. It applies the user-defined function to the data
#' and returns the statistic for each group.
#'
#' @param data_cube A data cube object (class
#' 'processed_cube' or 'sim_cube', see `b3gbi::process_cube()`) or a dataframe
#' (from `$data` slot of 'processed_cube' or 'sim_cube'). As used by
#' `bootstrap_cube()`.
#' @param fun A function which, when applied to
#' `data_cube` returns the statistic(s) of interest. This function must return a
#' dataframe with a column `diversity_val` containing the statistic of interest.
#' @param ... Additional arguments passed on to `fun`.
#' @param grouping_var A character vector specifying the grouping variable(s)
#' for the bootstrap analysis. The function `fun(data_cube, ...)` should return
#' a row per group. Used in combination with `ref_group`.
#' @param ref_group A string indicating the
#' reference group to compare the statistic with. Default is `NA`, meaning no
#' reference group is used.
#'
#' @return A dataframe containing the calculated statistic for each group.
#' The returned dataframe will include the grouping variable(s) and the
#' statistic(s) computed by the function.
#'
#' @noRd
#'
#' @import dplyr
#' @importFrom rlang .data inherits_any

calc_stat_by_group <- function(
    data_cube,
    fun,
    ...,
    grouping_var = NA,
    ref_group = NA) {
  if (!is.na(ref_group)) {
    if (rlang::inherits_any(data_cube, c("processed_cube", "sim_cube"))) {
      # Check if ref_group is present in grouping_var
      matching_col <- grouping_var[
        sapply(data_cube$data %>% dplyr::select(dplyr::all_of(grouping_var)),
               function(col) ref_group %in% col)]

      stopifnot(
        "`ref_group` is not present in `grouping_var` column of `data_cube`." =
          is.na(ref_group) | ref_group %in% data_cube$data[[matching_col]]
      )

      t0_full <- fun(data_cube, ...)$data
    } else {
      # Check if ref_group is present in grouping_var
      matching_col <- grouping_var[
        sapply(data_cube %>% dplyr::select(dplyr::all_of(grouping_var)),
               function(col) ref_group %in% col)]

      stopifnot(
        "`ref_group` is not present in `grouping_var` column of `data_cube`." =
          is.na(ref_group) | ref_group %in% data_cube[[matching_col]]
      )

      t0_full <- fun(data_cube, ...)
    }

    # Calculate reference value
    ref_val <- t0_full %>%
      dplyr::filter(.data[[matching_col]] == !!ref_group) %>%
      dplyr::rename("ref_val" = "diversity_val") %>%
      dplyr::select(-matching_col)

    t0 <- t0_full %>%
      dplyr::filter(.data[[matching_col]] != !!ref_group) %>%
      dplyr::left_join(ref_val, by = setdiff(grouping_var, matching_col)) %>%
      dplyr::mutate(diversity_val = .data$diversity_val - .data$ref_val) %>%
      dplyr::select(-"ref_val")
  } else {
    # Calculate true statistic
    if (rlang::inherits_any(data_cube, c("processed_cube", "sim_cube"))) {
      t0 <- fun(data_cube, ...)$data
    } else {
      t0 <- fun(data_cube, ...)
    }
  }

  return(t0)
}
