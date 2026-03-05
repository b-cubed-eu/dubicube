#' Default diagnostic rules for diagnose_cube
#'
#' Returns the default set of diagnostic rules used by `diagnose_cube()`.
#' Each rule defines how a specific data quality metric is computed and
#' evaluated.
#'
#' Rules are implemented as lists containing:
#' \itemize{
#'   \item `id` – name of the diagnostic metric
#'   \item `dimension` – cube dimension being evaluated (e.g. temporal)
#'   \item `threshold` – reference value used to determine severity
#'   \item `compute()` – function that calculates the metric
#'   \item `severity()` – function assigning a severity level
#'   \item `message()` – function generating a human-readable message
#' }
#'
#' @return A list of diagnostic rule definitions.

default_cube_rules <- function() {
  list(
    temporal_coverage_rule()
  )
}


#' Temporal coverage diagnostic rule
#'
#' Creates a diagnostic rule that evaluates the temporal coverage of a
#' `processed_cube`. The rule calculates the number of unique years present
#' in the cube and compares it to a threshold to determine the severity
#' level.
#'
#' @return A list describing the temporal coverage diagnostic rule.
#'

temporal_coverage_rule <- function() {

  list(
    # Unique identifier for the diagnostic metric
    id = "temporal_coverage",

    # Dimension of the data cube that is evaluated
    dimension = "temporal",

    # Minimum recommended number of years
    threshold = 5,

    # Function computing the diagnostic metric
    compute = function(cube) {
      # Get data
      data <- get_cube_data(
        data_cube = cube,
        processed_cube = TRUE
      )

      # Count number of unique years in the dataset
      length(unique(data$year))
    },

    # Function determining the severity level
    severity = function(value, threshold) {
      if (value >= threshold) "ok"
      else if (value >= 3) "note"
      else "important"
    },

    # Function generating a descriptive message
    message = function(value, threshold) {
      paste("Cube contains observations across", value, "years.")
    }
  )

}
