#' Create a cube_diagnostics object
#'
#' Internal constructor for objects of class `cube_diagnostics`. This function
#' attaches the `cube_diagnostics` class to a data frame containing diagnostic
#' results and performs basic structural validation.
#'
#' @param x A data frame containing diagnostic results.
#'
#' @return An object of class `cube_diagnostics`.
#'
#' @details
#' The object is expected to contain one row per diagnostic metric with the
#' following columns:
#'
#' \describe{
#'   \item{dimension}{Character. Dimension of the cube that is evaluated
#'   (e.g. `"spatial"`, `"temporal"`, `"taxonomical"`).}
#'   \item{metric}{Character. Name of the diagnostic metric.}
#'   \item{value}{Numeric. Calculated value of the diagnostic metric.}
#'   \item{severity}{Character. Diagnostic severity level
#'   (e.g. `"ok"`, `"note"`, `"important"`, `"very_important"`).}
#'   \item{message}{Character. Human-readable explanation of the diagnostic
#'   result.}
#' }
#'
#' This function is intended for internal use in \code{diagnose_cube()}.

new_cube_diagnostics <- function(x) {
  # Validate structure of diagnostics table
  x <- validate_cube_diagnostics(x)

  # Attach S3 class
  structure(
    x,
    class = c("cube_diagnostics", class(x))
  )
}


#' Validate cube_diagnostics structure
#'
#' Internal validation function for `cube_diagnostics` objects. Ensures that the
#' required columns are present in the diagnostics table.
#'
#' @param x A data frame containing diagnostic results.
#'
#' @return The validated object `x`. An error is thrown if required columns are
#' missing.

validate_cube_diagnostics <- function(x) {
  # Required columns describing each diagnostic metric
  required <- c(
    "dimension",
    "metric",
    "value",
    "severity",
    "message"
  )

  # Identify missing columns
  missing <- setdiff(required, names(x))
  if (length(missing) > 0) {
    stop(
      "Missing diagnostic columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  return(x)
}
