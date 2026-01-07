#' Detect redundant grouping variables
#'
#' This function checks whether any of the specified grouping variables contain
#' redundant information. Two grouping variables are considered redundant if
#' there is a one-to-one mapping between them.
#'
#' @param data A dataframe containing the grouping variables.
#' @param grouping_var A character vector specifying the names of the grouping
#' variables.
#'
#' @return Logical. Returns `TRUE` if redundant grouping variables are detected,
#' otherwise `FALSE`.
#'
#' @examples
#' years <- 2000:2002
#' df <- data.frame(
#'   year = rep(years, 2),
#'   time_point = rep(seq_along(years), 2),
#'   region = rep(c("A", "B", "C"), each = 2)
#' )
#'
#' # year and time_point encode the same information
#' has_redundant_grouping_vars(df, c("year", "time_point"))
#' #> TRUE
#'
#' # year and region are not redundant
#' has_redundant_grouping_vars(df, c("year", "region"))
#' #> FALSE
#'
#' @noRd
#'
#' @import dplyr
#' @importFrom rlang .data
has_redundant_grouping_vars <- function(data, grouping_var) {# nolint: cyclocomp_linter

  if (length(grouping_var) < 2) {
    return(FALSE)
  }

  grouping_data <- data[, grouping_var, drop = FALSE]

  for (i in seq_along(grouping_var)) {
    for (j in seq_along(grouping_var)) {
      if (i != j) {

        mapping_check <- grouping_data %>%
          dplyr::select(all_of(grouping_var[c(i, j)])) %>%
          dplyr::distinct() %>%
          dplyr::count(.data[[grouping_var[i]]]) %>%
          dplyr::pull(n) %>%
          unique()

        if (length(mapping_check) == 1 && mapping_check == 1) {
          return(TRUE)
        }
      }
    }
  }

  FALSE
}


#' Extract data from a processed data cube or from a dataframe
#'
#' Returns the underlying data from a processed or simulated biodiversity data
#' cube, or directly returns the data frame if `processed_cube = FALSE`.
#'
#' @param data_cube An object of class `processed_cube`, `sim_cube`, or a
#' `data.frame`. If `processed_cube = TRUE`, this must be a processed or
#' simulated data cube (i.e., an object with class `processed_cube` or
#' `sim_cube`) that contains a `$data` element.
#' @param processed_cube Logical. If `TRUE` (default), the function expects
#' `data_cube` to be a cube object and returns its `$data` slot. If `FALSE`, the
#' function expects a plain `data.frame` and returns it as-is.
#'
#' @return A `data.frame` containing the underlying data from the cube or the
#' provided dataframe itself.
#'
#' @noRd
#'
#' @importFrom stats setNames
#' @importFrom rlang inherits_any

get_cube_data <- function(data_cube, processed_cube = TRUE) {
  # Check if processed_cube is a logical vector of length 1
  stopifnot("`processed_cube` must be a logical vector of length 1." =
              assertthat::is.flag(processed_cube))

  if (processed_cube) {
    # Check data_cube input
    cube_message <- paste0(
      "`data_cube` must be a data cube object (class 'processed_cube' or ",
      "'sim_cube').\n",
      "Set `processed_cube = FALSE` if you want to provide a dataframe."
    )
    do.call(
      stopifnot,
      stats::setNames(
        list(
          rlang::inherits_any(
            data_cube,
            c("processed_cube", "sim_cube")
          )
        ),
        cube_message
      )
    )

    # Return cube data
    return(data_cube$data)
  }

  # Check dataframe input
  df_message <- paste0(
    "`df` must be a dataframe.\n",
    "Set `processed_cube = TRUE` if you want to provide a data cube object ",
    "(class 'processed_cube' or 'sim_cube')."
  )
  do.call(
    stopifnot,
    stats::setNames(
      list(inherits(data_cube, "data.frame")),
      df_message
    )
  )

  stopifnot("`df` must be a dataframe. " =
              inherits(data_cube, "data.frame"))

  # Return cube data
  return(data_cube)
}

#' Calculate Statistic Per Group
#'
#' This function calculates a specified statistic on a dataset, grouped by one
#' or more grouping variables. It applies the user-defined function to the data
#' and returns the statistic for each group.
#'
#' @param data_cube A dataframe returning the data cube data. As returned by
#' `get_cube_data()`.
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
#' @importFrom rlang .data

calc_stat_by_group <- function(
    data_cube,
    fun,
    ...,
    grouping_var = NA,
    ref_group = NA) {
  if (!is.na(ref_group)) {
    # Check if ref_group is present in grouping_var
    matching_col <- grouping_var[
      sapply(data_cube %>% dplyr::select(dplyr::all_of(grouping_var)),
             function(col) ref_group %in% col)
    ]

    stopifnot(
      "`ref_group` is not present in `grouping_var` column of `data_cube`." =
        is.na(ref_group) | ref_group %in% data_cube[[matching_col]]
    )

    t0_full <- fun(data_cube, ...)

    # Calculate reference value
    ref_val <- t0_full %>%
      dplyr::filter(.data[[matching_col]] == !!ref_group) %>%
      dplyr::rename("ref_val" = "diversity_val") %>%
      dplyr::select(-matching_col)

    # Calculate true statistic
    t0 <- t0_full %>%
      dplyr::filter(.data[[matching_col]] != !!ref_group) %>%
      dplyr::left_join(ref_val, by = setdiff(grouping_var, matching_col)) %>%
      dplyr::mutate(diversity_val = .data$diversity_val - .data$ref_val) %>%
      dplyr::select(-"ref_val")

    return(t0)
  }

  # Calculate true statistic
  return(fun(data_cube, ...))
}
