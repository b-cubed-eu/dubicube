#' Add effect classifications to a dataframe by comparing the confidence
#' intervals with a reference and thresholds
#'
#' This function adds classified effects to a dataframe as ordered factor
#' variables by comparing the confidence intervals with a reference and
#' thresholds.
#'
#' @param df A dataframe containing summary data of confidence limits. Two
#' columns are required containing lower and upper limits indicated by the
#' `cl_columns` argument. Any other columns are optional.
#' @param cl_columns A vector of 2 column names in `df` indicating respectively
#' the lower and upper confidence limits (e.g. `c("lcl", "ucl")`).
#' @param threshold A vector of either 1 or 2 thresholds. A single threshold
#' will be transformed into `reference + c(-abs(threshold), abs(threshold))`.
#' @param reference The null hypothesis value to compare confidence intervals
#' against. Defaults to 0.
#' @param coarse Logical, defaults to `TRUE`. If `TRUE`, add a coarse
#' classification to the dataframe.
#'
#' @returns The returned value is a modified version of the original input
#' dataframe `df` with additional columns `effect_code` and `effect` containing
#' respectively the effect symbols and descriptions as ordered factor variables.
#' In case of `coarse = TRUE` (by default) also `effect_code_coarse` and
#' `effect_coarse` containing the coarse classification effects.
#'
#' @details
#' This function is a wrapper around `effectclass::classify()` and
#' `effectclass::coarse_classification()` from the \pkg{effectclass} package
#' (Onkelinx, 2023). They classify effects in a stable and transparent manner.
#'
#' # nolint start: line_length_linter.
#' | Symbol | Fine effect / trend | Coarse effect / trend | Rule |
#' | :---: | --- | --- | --- |
#' | `++` | strong positive effect / strong increase | positive effect / increase | confidence interval above the upper threshold |
#' | `+`  | positive effect / increase | positive effect / increase | confidence interval above reference and contains the upper threshold |
#' | `+~` | moderate positive effect / moderate increase | positive effect / increase | confidence interval between reference and the upper threshold |
#' | `~`  | no effect / stable | no effect / stable | confidence interval between thresholds and contains reference |
#' | `-~` | moderate negative effect / moderate decrease | negative effect / decrease | confidence interval between reference and the lower threshold |
#' | `-`  | negative effect / decrease | negative effect / decrease | confidence interval below reference and contains the lower threshold |
#' | `--` | strong negative effect / strong decrease | negative effect / decrease | confidence interval below the lower threshold |
#' | `?+` | potential positive effect / potential increase | unknown effect / unknown | confidence interval contains reference and the upper threshold |
#' | `?-` | potential negative effect / potential decrease | unknown effect / unknown | confidence interval contains reference and the lower threshold |
#' | `?`  | unknown effect / unknown | unknown effect / unknown | confidence interval contains the lower and upper threshold |
#' # nolint end
#'
#' @references
#' Onkelinx, T. (2023). effectclass: Classification and visualisation of effects
#' \[Computer software\]. \url{https://inbo.github.io/effectclass/}
#'
#' @export
#'
#' @family uncertainty
#'
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom effectclass coarse_classification classification
#'
#' @examples
#' # example code
#'

add_effect_classification <- function(
    df,
    cl_columns,
    threshold,
    reference = 0,
    coarse = TRUE) {
  ### Start checks
  # Check dataframe input
  stopifnot("`df` must be a dataframe." =
              inherits(df, "data.frame"))

  # Check if cl_columns is a character vector
  stopifnot("`cl_columns` must be a character vector of length 2." =
              is.character(cl_columns) & length(cl_columns) == 2)

  # Check if reference is a numeric vector
  stopifnot("`threshold` must be a numeric vector of length 1 or 2." =
              is.numeric(threshold) &
              (length(threshold) == 1 | length(threshold) == 2))

  # Check if reference is a number
  stopifnot("`reference` must be a numeric vector of length 1." =
              assertthat::is.number(reference))

  # Check if coarse is a logical vector of length 1
  stopifnot("`coarse` must be a logical vector of length 1." =
              assertthat::is.flag(coarse))
  ### End checks

  # Classify effects with effectclass
  classified_df <- df %>%
    dplyr::mutate(effect_code = effectclass::classification(
      lcl = !!dplyr::sym(cl_columns[1]),
      ucl = !!dplyr::sym(cl_columns[2]),
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
    dplyr::mutate(
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
      effect = dplyr::case_when(
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

  # Create ordered factors of effects if coarse is specified
  if (coarse) {
    out_df <- out_df %>%
      dplyr::mutate(
        effect_code_coarse = factor(.data$effect_code_coarse,
                                    levels = c(
                                      "+",
                                      "~",
                                      "-",
                                      "?"),
                                    ordered = TRUE),
        effect_coarse = dplyr::case_when(
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
