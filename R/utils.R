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
