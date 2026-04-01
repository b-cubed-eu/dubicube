#' Convert cube resolution to meters
#'
#' Internal helper that converts spatial grid resolution values stored in a
#' `processed_cube` object to meters. This allows spatial diagnostics to compare
#' coordinate uncertainty with the spatial resolution of the cube.
#'
#' Supported resolution formats include:
#' \itemize{
#'   \item `"1km"`, `"10km"` – converted directly to meters
#'   \item `"0.25degrees"`, `"1degree"` – converted using an approximate
#'   conversion of 1 degree ≈ 111,320 meters
#' }
#'
#' @param res Character string describing the spatial resolution of the cube
#' (e.g. `"10km"`, `"1km"`, `"0.25degrees"`).
#'
#' @return Numeric value giving the resolution in meters.
#'
#' @details
#' The conversion from degrees to meters assumes an approximate value at the
#' equator (1 degree ≈ 111,320 meters). This approximation is sufficient for
#' diagnostic purposes but may vary with latitude.
#'
#' @noRd

resolution_to_meters <- function(res) {
  # km resolution
  if (grepl("km$", res)) {
    value <- as.numeric(sub("km", "", res))
    return(value * 1000)
  }

  # degree resolution (approx at equator)
  if (grepl("degrees?$", res)) {
    value <- as.numeric(sub("degrees?", "", res))
    return(value * 111320)
  }

  stop("Unsupported resolution format: ", res, call. = FALSE)
}


#' Resolve diagnostic rules for cube diagnostics
#'
#' Internal helper that resolves the `rules` argument supplied to
#' [diagnose_cube()]. Character rule set names are expanded into their
#' corresponding predefined rule lists, while user-supplied rule objects
#' are passed through unchanged.
#'
#' @param rules Diagnostic rules supplied to [diagnose_cube()]. Can be:
#' \itemize{
#'   \item A character vector referring to built-in rule sets
#'   (e.g. `"basic"`, `"spatial"`).
#'   \item A list of rule objects.
#'   \item A combination of both.
#' }
#'
#' @return A flat list of rule objects ready for evaluation.
#'
#' @noRd

resolve_cube_rules <- function(rules) {
  # If rules are provided as character names of rule sets,
  # expand them to their corresponding predefined rule lists
  if (is.character(rules)) {
    rules <- unlist(lapply(rules, get_cube_rule_set), recursive = FALSE)
  }

  # Ensure the result is a list of rule objects
  if (!is.list(rules)) {
    stop(
      "`rules` must be a character vector or list of rule objects.",
      call. = FALSE
    )
  }

  return(rules)
}


#' Retrieve a predefined cube diagnostic rule set
#'
#' Internal helper that maps rule set names used in [diagnose_cube()]
#' to their corresponding collections of diagnostic rules.
#'
#' Supported rule sets include:
#' \itemize{
#'   \item `"basic"`: General cube diagnostics.
#'   \item `"spatial"`: Spatial coverage and resolution diagnostics.
#'   \item `"temporal"`: Temporal coverage diagnostics.
#'   \item `"taxonomical"`: Taxonomic coverage diagnostics.
#'   \item `"all"`: Combination of all available rule sets.
#' }
#'
#' @param name Character string identifying the rule set.
#'
#' @return A list of diagnostic rule objects.
#'
#' @noRd

get_cube_rule_set <- function(name) {
  switch(
    # Basic diagnostic rules
    name,
    basic = basic_cube_rules(),

    # Error if an unknown rule set is requested
    stop("Unknown rule set: ", name, call. = FALSE)
  )
}
