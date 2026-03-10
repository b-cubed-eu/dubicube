#' Create a cube_rule object
#'
#' Internal constructor for objects of class `cube_rule`. This function attaches
#' the `cube_rule` class to a rule definition and validates that the required
#' components are present.
#'
#' @param x A list describing a diagnostic rule.
#'
#' @return An object of class `cube_rule`.
#'
#' @details
#' A rule object must contain the following elements:
#'
#' \describe{
#'   \item{id}{Character. Unique identifier of the diagnostic metric.}
#'   \item{dimension}{Character. Cube dimension being evaluated
#'   (e.g. `"spatial"`, `"temporal"`, `"taxon"`).}
#'   \item{threshold}{Numeric. Reference threshold used for severity
#'   evaluation.}
#'   \item{compute}{Function computing the diagnostic metric.}
#'   \item{severity}{Function assigning a severity level.}
#'   \item{message}{Function generating a human-readable message.}
#' }
#'
#' @noRd

new_cube_rule <- function(x) {
  # Validate rule structure
  x <- validate_cube_rule(x)

  # Attach S3 class
  structure(
    x,
    class = c("cube_rule", class(x))
  )
}


#' Validate cube_rule structure
#'
#' Internal validation function for `cube_rule` objects. Ensures that the rule
#' definition contains the required elements.
#'
#' @param x A list describing a diagnostic rule.
#'
#' @return The validated rule object `x`.
#'
#' @noRd

validate_cube_rule <- function(x) {
  # Required columns describing the rule
  required <- c(
    "id",
    "dimension",
    "thresholds",
    "compute",
    "severity",
    "message"
  )

  # Identify missing columns
  missing <- setdiff(required, names(x))
  if (length(missing) > 0) {
    stop(
      "Missing rule elements: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  return(x)
}
