#' Diagnose data quality of a processed_cube
#'
#' Evaluates a set of diagnostic rules describing the data quality of a
#' biodiversity occurrence cube. Each rule computes a metric
#' on the cube and assigns a severity level indicating potential limitations
#' of the data for exploratory analysis or indicator calculation.
#'
#' @param cube A `processed_cube` object as returned by
#'   `b3gbi::process_cube()`.
#' @param rules A list of diagnostic rules. Each rule must contain elements
#'   `id`, `dimension`, `threshold`, `compute`, `severity`, and `message`.
#'   Defaults to `default_cube_rules()`.
#' @param verbose Logical indicating whether a diagnostic summary should be
#'   printed.
#'
#' @return An object of class `cube_diagnostics`, containing a data frame with
#'   the following columns:
#'   \describe{
#'   \item{dimension}{Dimension of the cube being evaluated
#'   (e.g. `"spatial"`, `"temporal"`, `"taxonomical"`).}
#'   \item{metric}{Name of the diagnostic metric.}
#'   \item{value}{Computed metric value.}
#'   \item{threshold}{Reference threshold used to determine severity.}
#'   \item{severity}{Severity level (`"ok"`, `"note"`, `"important"`,
#'   `"very_important"`).}
#'   \item{message}{Human-readable description of the diagnostic result.}
#'   }
#'
#' @details
#' Diagnostics are returned as a `cube_diagnostics` object containing one row
#' per metric with the evaluated value, threshold, severity level and an
#' explanatory message.
#'
#' @export
#'
#' @family data_exploration
#'
#' @examples
#' \dontrun{
#' # After processing a data cube with b3gbi::process_cube()
#' diag <- diagnose_cube(processed_cube)
#' diag
#' }
diagnose_cube <- function(
    cube,
    rules = default_cube_rules(),
    verbose = TRUE) {
  # Check input
  stopifnot("`cube` must be of class 'processed_cube'" =
              inherits(cube, "processed_cube"))

  results <- lapply(rules, function(rule) {
    # Validate rule
    # validate_cube_rule(rule)

    # Compute diagnostic values
    value <- rule$compute(cube)
    severity <- rule$severity(value, rule$threshold)
    message <- rule$message(value, rule$threshold)

    # Return diagnostic row
    data.frame(
      dimension = rule$dimension,
      metric = rule$id,
      value = value,
      threshold = rule$threshold,
      severity = severity,
      message = message,
      stringsAsFactors = FALSE
    )

  })

  # Combine rule results
  diagnostics <- do.call(rbind, results)

  # Create cube_diagnostics object
  diagnostics <- new_cube_diagnostics(diagnostics)

  # Print summary if requested
  if (verbose) {
    print(diagnostics)
  }

  return(diagnostics)
}
