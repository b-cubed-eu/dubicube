#' Diagnose data quality of a processed_cube
#'
#' Evaluates a set of diagnostic rules describing the data quality of a
#' biodiversity occurrence cube. Each rule computes a metric
#' on the cube and assigns a severity level indicating potential limitations
#' of the data for exploratory analysis or indicator calculation.
#'
#' @param cube A `processed_cube` object as returned by
#'   `b3gbi::process_cube()`.
#' @param rules Diagnostic rules to evaluate. Can be:
#' \itemize{
#'   \item A character vector referring to built-in rule sets
#'   (e.g. `"basic"`, `"spatial"`).
#'   \item A list of rule objects.
#'   \item A combination of both.
#' }
#' @param verbose Logical indicating whether a diagnostic summary should be
#'   printed.
#' @param ... Additional arguments passed to `print.cube_diagnostics()` in case
#'   `verbose = TRUE`.
#'
#' @return An object of class `cube_diagnostics`, containing one row
#' per metric with the following columns:
#'   \describe{
#'   \item{dimension}{Dimension of the cube being evaluated
#'   (e.g. `"spatial"`, `"temporal"`, `"taxonomical"`).}
#'   \item{metric}{Name of the diagnostic metric.}
#'   \item{value}{Computed metric value.}
#'   \item{severity}{Severity level (`"ok"`, `"note"`, `"important"`,
#'   `"very_important"`).}
#'   \item{message}{Human-readable description of the diagnostic result.}
#' }
#'
#' The rule objects are attached as an attribute of the diagnostics object.
#'
#' @export
#'
#' @family data_exploration
#'
#' @examples
#' \dontrun{
#' # After processing a data cube with b3gbi::process_cube()
#'
#' # Diagnose based on default rules
#' diag <- diagnose_cube(processed_cube)
#'
#' # Only show at least important diagnoses
#' diag <- diagnose_cube(processed_cube, filter_summary = "important")
#' }

diagnose_cube <- function(
    cube,
    rules = "basic",
    verbose = TRUE,
    ...) {
  # Check input
  stopifnot("`cube` must be of class 'processed_cube'" =
              inherits(cube, "processed_cube"))

  rules <- resolve_cube_rules(rules)

  results <- lapply(rules, function(rule) {
    # Compute diagnostic values
    value <- rule$compute(cube)
    severity <- rule$severity(value, rule$thresholds)
    message <- rule$message(value)

    # Return diagnostic row
    data.frame(
      dimension = rule$dimension,
      metric = rule$id,
      value = value,
      severity = severity,
      message = message,
      stringsAsFactors = FALSE
    )
  })

  # Combine rule results
  diagnostics <- do.call(rbind, results)

  # Create cube_diagnostics object
  diagnostics <- new_cube_diagnostics(diagnostics)
  # Attach rule definitions
  attr(diagnostics, "rules") <- rules

  # Print summary if requested
  if (verbose) {
    print(
      diagnostics,
      ...
    )
  }

  invisible(diagnostics)
}
