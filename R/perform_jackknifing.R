#' Calculate jackknife estimates for a dataframe with bootstrap replicates
#'
#' This function jackknife estimates for a dataframe containing
#' bootstrap replicates per group. The resulting estimates are used to compute
#' the acceleration factor for BCa confidence intervals
#' (`calculate_acceleration()`), which in turn is used in
#' `calculate_bootstrap_ci()`.
#'
#' @param df A dataframe.
#' @param fun A function which, when applied to
#' `df` returns the statistic(s) of interest. This function must return a
#' dataframe with a column `diversity_val` containing the statistic of interest.
#' @param ... Additional arguments passed on to `fun`.
#' @param grouping_var A character vector specifying the grouping variable(s)
#' for the bootstrap analysis. The function `fun(df, ...)` should return
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
#' @returns A dataframe with jackknife estimates for each group defined by
#' `grouping_var`.
#'
#' @import dplyr
#' @importFrom rlang .data inherits_any
#' @importFrom tidyr expand_grid
#' @importFrom purrr map
#'
#' @noRd

perform_jackknifing <- function(
    df,
    fun,
    ...,
    grouping_var,
    ref_group = NA,
    progress = FALSE) {
  ### Start checks
  # Check fun input
  stopifnot("`fun` must be a function." = is.function(fun))

  # Check if grouping_var is a character vector
  stopifnot("`grouping_var` must be a character vector." =
              is.character(grouping_var))

  # Check if grouping_var contains redundant variables
  check_redundant_grouping_vars(df, grouping_var)

  # Check if ref_group is NA or a number or a string
  stopifnot(
    "`ref_group` must be a numeric/character vector of length 1 or NA." =
      (assertthat::is.number(ref_group) | assertthat::is.string(ref_group) |
       is.na(ref_group)) &
      length(ref_group) == 1
  )

  # Check if ref_group is present in grouping_var
  stopifnot(
    "`ref_group` is not present in `grouping_var` column of `df`." =
      is.na(ref_group) |
      any(
        sapply(
          as.list(grouping_var), function(var) {
            ref_group %in% df[[var]]
          }
        )
      )
  )

  # Check if progress is a logical vector of length 1
  stopifnot("`progress` must be a logical vector of length 1." =
              assertthat::is.flag(progress))
  ### End checks

  # Perform jackknifing
  jackknife_estimates <- purrr::map(
    seq_len(nrow(df)),
    function(i) {
      # Identify group
      group <- df[i, ] %>%
        dplyr::select(dplyr::all_of(grouping_var))

      # Calculate indicator value without i'th observation
      fun(df[-i, ], ...) %>%
        dplyr::inner_join(group, by = grouping_var) %>%
        dplyr::pull(.data$diversity_val)
    },
    .progress = ifelse(progress, "Jackknife estimation", progress)
  ) %>%
    unlist()

  # Create dataframe
  jackknife_df <- df %>%
    dplyr::mutate(jack_rep = jackknife_estimates) %>%
    dplyr::select(dplyr::all_of(grouping_var), "jack_rep")

  # Calculate differences in presence of reference group
  if (!is.na(ref_group)) {
    # Check if ref_group is present in grouping_var
    matching_col <- grouping_var[
      sapply(df %>% dplyr::select(dplyr::all_of(grouping_var)),
             function(col) ref_group %in% col)
    ]

    stopifnot(
      "`ref_group` is not present in `grouping_var` column of `df`." =
        is.na(ref_group) | ref_group %in% df[[matching_col]]
    )

    # Get group-specific estimates
    group_estimates <- fun(df, ...)

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
