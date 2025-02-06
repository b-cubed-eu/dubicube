#' Add effect classifications to a dataframe by comparing the confidence
#' intervals with a reference and thresholds
#'
#' This function adds classified effects to a dataframe as ordered factor
#' variables by comparing the confidence intervals with a reference and
#' thresholds.
#'
#' @param df A dataframe containing summary data of confidence limits.
#' @param cl_columns A vector of 2 column names in `df` indicating respectively
#' the lower and upper confidence limits.
#' @param threshold A vector of either 1 or 2 thresholds. A single threshold
#' will be transformed into `reference + c(-abs(threshold), abs(threshold))`.
#' See `effectclass::classify()`.
#' @param reference The null hypothesis. Defaults to 0.
#' See `effectclass::classify()`.
#' @param coarse Logical, defaults to `TRUE`. If `TRUE`, add a coarse
#' classification to the dataframe.
#' See `effectclass::coarse_classification()`.
#'
#' @returns The returned value is the original dataframe with added columns
#' `effect_code` and `effect` containing respectively the effect symbols and
#' descriptions as ordered factor variables. In case or `coarse = TRUE` (by
#' default) also `effect_code_coarse` and `effect_coarse` containing the coarse
#' classification effects.
#'
#' @details
#' This function is a wrapper around `effectclass::classify()` and
#' `effectclass::coarse_classification()` from the \pkg{effectclass} package
#' (Onkelinx, 2023).
#'
#' @references
#' Onkelinx, T. (2023). effectclass: Classification and visualisation of effects
#' \[Computer software\]. \url{https://effectclass.netlify.com}
#'
#' @export
#'
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom effectclass coarse_classification classify
#'
#' @examples
#' # example code
#'

add_classification_as_factor <- function(
    df,
    cl_columns,
    threshold,
    reference = 0,
    coarse = TRUE) {
  require("dplyr")
  require("rlang")

  # Classify effects with effectclass
  classified_df <- df %>%
    mutate(effect_code = effectclass::classification(
      lcl = !!sym(cl_columns[1]),
      ucl = !!sym(cl_columns[2]),
      threshold = threshold,
      reference = reference)
    )

  # Add coarse classification if specified
  if (coarse) {
    classified_df$effect_code_coarse <- effectclass::coarse_classification(
      classified_df$effect_code)
  }

  # Create ordered factors of effects
  out_df <- classified_df %>%
    mutate(
      effect_code = factor(.data$effect_code,
                           levels = c(
                             "++",
                             "+",
                             "+~",
                             "~",
                             "-~",
                             "-",
                             "--",
                             "?+",
                             "?-",
                             "?"),
                           ordered = TRUE),
      effect = case_when(
        effect_code == "++" ~ "strong increase",
        effect_code == "+"  ~ "increase",
        effect_code == "+~" ~ "moderate increase",
        effect_code == "~"  ~ "stable",
        effect_code == "-~" ~ "moderate decrease",
        effect_code == "-"  ~ "decrease",
        effect_code == "--" ~ "strong decrease",
        effect_code == "?+" ~ "potential increase",
        effect_code == "?-" ~ "potential decrease",
        effect_code == "?"  ~ "unknown"
      ),
      effect = factor(.data$effect,
                      levels = c(
                        "strong increase",
                        "increase",
                        "moderate increase",
                        "stable",
                        "moderate decrease",
                        "decrease",
                        "strong decrease",
                        "potential increase",
                        "potential decrease",
                        "unknown"),
                      ordered = TRUE)
    )

  if (coarse) {
    out_df <- out_df %>%
      mutate(
        effect_code_coarse = factor(.data$effect_code_coarse,
                                    levels = c(
                                      "+",
                                      "~",
                                      "-",
                                      "?"),
                                    ordered = TRUE),
        effect_coarse = case_when(
          effect_code_coarse == "+" ~ "increase",
          effect_code_coarse == "-" ~ "decrease",
          effect_code_coarse == "~" ~ "stable",
          effect_code_coarse == "?" ~ "unknown"
        ),
        effect_coarse = factor(.data$effect_coarse,
                               levels = c(
                                 "increase",
                                 "stable",
                                 "decrease",
                                 "unknown"),
                               ordered = TRUE)
      )
  }

  return(out_df)
}
