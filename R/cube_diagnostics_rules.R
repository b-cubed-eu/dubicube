################################################################################
## Group rules
################################################################################

#' Basic diagnostic rules for diagnose_cube
#'
#' Returns basic diagnostic rules used by `diagnose_cube()`.
#' Each rule defines how a specific data quality metric is computed and
#' evaluated.
#'
#' Rules are implemented as lists containing:
#' \itemize{
#'   \item `id` – name of the diagnostic metric
#'   \item `dimension` – cube dimension being evaluated (e.g. temporal)
#'   \item `thresholds` – reference values used to determine severity
#'   \item `compute()` – function that calculates the metric
#'   \item `severity()` – function assigning a severity level
#'   \item `message()` – function generating a human-readable message
#' }
#'
#' @return A list of diagnostic rule definitions.
#'
#' @details
#' Contains the following rules:
#' - `rule_temporal_min_years()`: Number of years
#' - `rule_temporal_missing_years()`: Missing years
#' - `rule_spatial_min_cells()`: Number of grid cells
#' - `rule_spatial_max_uncertainty()`: Number of records where coordinate
#'    uncertainty is larger than grid resolution
#' - `rule_spatial_miss_uncertainty`: Number of records with missing coordinate
#'    uncertainty
#' - `rule_taxon_min_taxa()`: Number of taxa
#' - `rule_obs_min_records()`: Number of records (rows)
#' - `rule_obs_min_total()`: Total number of observations (sum)
#'
#' Default thresholds are used.
#'
#' @export
#' @keywords internal

basic_cube_rules <- function() {
  list(
    rule_temporal_min_years(), # Number of years
    rule_temporal_missing_years(), # Missing years
    rule_spatial_min_cells(), # Number of grid cells
    # Number of records where coord. uncertainty is larger than grid resolution
    rule_spatial_max_uncertainty(),
    # Number of records with missing coord. uncertainty
    rule_spatial_miss_uncertainty(),
    rule_taxon_min_taxa(), # Number of taxa
    rule_obs_min_records(), # Number of records (rows)
    rule_obs_min_total() # Total number of observations (sum)
  )
}


################################################################################
## Basic rules
################################################################################

#' Temporal minimum years diagnostic rule
#'
#' Creates a diagnostic rule that evaluates whether a data cube contains a
#' sufficient number of temporal observations (years). The rule counts
#' the number of unique years present in the cube and compares it to a
#' threshold to determine the severity level.
#'
#' @param thresholds Named numeric vector with severity thresholds:
#' ok, note, important, very_important. Defaults are used if not provided.
#'
#' @return An object of class `cube_rule`.
#'
#' @export
#' @keywords internal

rule_temporal_min_years <- function(
  thresholds = c(ok = 5, note = 3, important = 0, very_important = NULL)
) {
  rule <- list(
    # Unique identifier for the diagnostic metric
    id = "temporal_min_points",

    # Dimension of the cube being evaluated
    dimension = "temporal",

    # Minimum recommended number of years
    thresholds = thresholds,

    # Function computing the diagnostic metric
    compute = function(cube) {
      # Extract cube data
      data <- get_cube_data(
        data_cube = cube,
        processed_cube = TRUE
      )
      # Count number of unique years
      length(unique(data$year))
    },

    # Function assigning severity based on named thresholds
    severity = function(value, thresholds) {
      if (value >= thresholds["ok"]) "ok"
      else if (value >= thresholds["note"]) "note"
      else "important"
    },

    # Function generating a descriptive message
    message = function(value) {
      paste(
        "Cube contains observations across",
        value,
        "years."
      )
    }
  )

  # Return a cube_rule object
  return(new_cube_rule(rule))
}

#' Temporal gaps diagnostic rule
#'
#' Creates a diagnostic rule that evaluates whether a data cube contains
#' missing years. The rule counts the number of missing years present in the
#' cube and compares it to a threshold to determine the severity level.
#'
#' @param thresholds Named numeric vector with severity thresholds:
#' ok, note, important, very_important. Defaults are used if not provided.
#'
#' @return An object of class `cube_rule`.
#'
#' @export
#' @keywords internal

rule_temporal_missing_years <- function(
  thresholds = c(ok = 0, note = 1, important = 3, very_important = NULL)
) {
  rule <- list(
    # Unique identifier for the diagnostic metric
    id = "temporal_missing_years",

    # Dimension of the cube being evaluated
    dimension = "temporal",

    # Threshold number of missing years
    thresholds = thresholds,

    # Function computing the diagnostic metric
    compute = function(cube) {
      # Extract cube data
      data <- get_cube_data(
        data_cube = cube,
        processed_cube = TRUE
      )
      # Count number of missing years
      control_years <- seq(min(data$year), max(data$year))
      length(control_years) - length(unique(data$year))
    },

    # Function assigning severity based on named thresholds
    severity = function(value, thresholds) {
      if (value >= thresholds["important"]) "important"
      else if (value >= thresholds["note"]) "note"
      else "ok"
    },

    # Function generating a descriptive message
    message = function(value) {
      paste(
        "Cube contains",
        value,
        "missing years."
      )
    }
  )

  # Return a cube_rule object
  return(new_cube_rule(rule))
}


#' Spatial minimum grid cells diagnostic rule
#'
#' Creates a diagnostic rule that evaluates whether a data cube contains a
#' sufficient number of spatial observations (grid cells). The rule counts
#' the number of unique grid cells present in the cube and compares it to a
#' threshold to determine the severity level.
#'
#' @param thresholds Named numeric vector with severity thresholds:
#' ok, note, important, very_important. Defaults are used if not provided.
#'
#' @return An object of class `cube_rule`.
#'
#' @export
#' @keywords internal

rule_spatial_min_cells <- function(
  thresholds = c(ok = 5, note = 3, important = 0, very_important = NULL)
) {
  rule <- list(
    # Unique identifier for the diagnostic metric
    id = "spatial_min_cells",

    # Dimension of the cube being evaluated
    dimension = "spatial",

    # Minimum recommended number of grid cells
    thresholds = thresholds,

    # Function computing the diagnostic metric
    compute = function(cube) {
      # Extract cube data
      data <- get_cube_data(
        data_cube = cube,
        processed_cube = TRUE
      )
      # Count number of unique grid cells
      length(unique(data$cellCode))
    },

    # Function assigning severity based on named thresholds
    severity = function(value, thresholds) {
      if (value >= thresholds["ok"]) "ok"
      else if (value >= thresholds["note"]) "note"
      else "important"
    },

    # Function generating a descriptive message
    message = function(value) {
      paste(
        "Cube contains observations across",
        value,
        "grid cells."
      )
    }
  )

  # Return a cube_rule object
  return(new_cube_rule(rule))
}

#' Maximal coordinate uncertainty diagnostic rule
#'
#' Creates a diagnostic rule that evaluates whether a data cube contains a
#' records with high coordinate uncertainty. The rule counts the number of
#' records (rows) in the cube where the minimal coordinate uncertainty is larger
#' than the resolution of the grid, and compares it to a threshold to determine
#' the severity level.
#'
#' @param thresholds Named numeric vector with severity thresholds:
#' ok, note, important, very_important. Defaults are used if not provided.
#'
#' @return An object of class `cube_rule`.
#'
#' @export
#' @keywords internal

rule_spatial_max_uncertainty <- function(
  thresholds = c(ok = 0, note = 1, important = 3, very_important = 5)
) {
  rule <- list(
    # Unique identifier for the diagnostic metric
    id = "spatial_max_uncertainty",

    # Dimension of the cube being evaluated
    dimension = "spatial",

    # Threshold number of records with high coordinate uncertainty
    thresholds = thresholds,

    # Function computing the diagnostic metric
    compute = function(cube) {
      # Extract cube data
      data <- get_cube_data(
        data_cube = cube,
        processed_cube = TRUE
      )
      # Get spatial resolution
      res <- cube$resolutions
      # Convert resolution to meters
      res_m <- resolution_to_meters(res)
      # Count records where uncertainty exceeds grid resolution
      sum(
        data$minCoordinateUncertaintyInMeters > res_m,
        na.rm = TRUE
      )
    },

    # Function assigning severity based on named thresholds
    severity = function(value, thresholds) {
      if (value >= thresholds["very_important"]) "very_important"
      else if (value >= thresholds["important"]) "important"
      else if (value >= thresholds["note"]) "note"
      else "ok"
    },

    # Function generating a descriptive message
    message = function(value) {
      paste(
        "Cube contains",
        value,
        "records where the coordinate uncertainty is larger than the",
        "grid cell resolution."
      )
    },

    # Returns TRUE for rows to drop
    filter_fn = function(cube) {
      data <- get_cube_data(cube, processed_cube = TRUE)
      res_m <- resolution_to_meters(cube$resolutions)
      data$minCoordinateUncertaintyInMeters > res_m
    }
  )

  # Return a cube_rule object
  return(new_cube_rule(rule))
}


#' Missing coordinate uncertainty diagnostic rule
#'
#' Creates a diagnostic rule that evaluates whether a data cube contains a
#' records with missing coordinate uncertainty. The rule counts the number of
#' records (rows) with missing coordinate uncertainty and compares it to a
#' threshold to determine the severity level.
#'
#' @param thresholds Named numeric vector with severity thresholds:
#' ok, note, important, very_important. Defaults are used if not provided.
#'
#' @return An object of class `cube_rule`.
#'
#' @export
#' @keywords internal

rule_spatial_miss_uncertainty <- function(
  thresholds = c(ok = 0, note = 1, important = 3, very_important = 5)
) {
  rule <- list(
    # Unique identifier for the diagnostic metric
    id = "spatial_miss_uncertainty",

    # Dimension of the cube being evaluated
    dimension = "spatial",

    # Threshold number of records with missing coordinate uncertainty
    thresholds = thresholds,

    # Function computing the diagnostic metric
    compute = function(cube) {
      # Extract cube data
      data <- get_cube_data(
        data_cube = cube,
        processed_cube = TRUE
      )
      # Count number of records with missing coord. uncertainty
      sum(is.na(data$minCoordinateUncertaintyInMeters))
    },

    # Function assigning severity based on named thresholds
    severity = function(value, thresholds) {
      if (value >= thresholds["very_important"]) "very_important"
      else if (value >= thresholds["important"]) "important"
      else if (value >= thresholds["note"]) "note"
      else "ok"
    },

    # Function generating a descriptive message
    message = function(value) {
      paste(
        "Cube contains",
        value,
        "records with missing coordinate uncertainty."
      )
    },

    # Returns TRUE for rows to drop
    filter_fn = function(cube) {
      data <- get_cube_data(cube, processed_cube = TRUE)
      is.na(data$minCoordinateUncertaintyInMeters)
    }
  )

  # Return a cube_rule object
  return(new_cube_rule(rule))
}


#' Taxonomic minimum taxa diagnostic rule
#'
#' Creates a diagnostic rule that evaluates whether a data cube contains a
#' sufficient number of taxonomical observations (taxa). The rule counts
#' the number of unique taxa present in the cube and compares it to a
#' threshold to determine the severity level.
#'
#' @param thresholds Named numeric vector with severity thresholds:
#' ok, note, important, very_important. Defaults are used if not provided.
#'
#' @return An object of class `cube_rule`.
#'
#' @export
#' @keywords internal

rule_taxon_min_taxa <- function(
  thresholds = c(ok = 5, note = 3, important = 0, very_important = NULL)
) {
  rule <- list(
    # Unique identifier for the diagnostic metric
    id = "taxon_min_taxa",

    # Dimension of the cube being evaluated
    dimension = "taxonomic",

    # Minimum recommended number of taxa
    thresholds = thresholds,

    # Function computing the diagnostic metric
    compute = function(cube) {
      # Extract cube data
      data <- get_cube_data(
        data_cube = cube,
        processed_cube = TRUE
      )
      # Count number of unique taxa
      length(unique(data$taxonKey))
    },

    # Function assigning severity based on named thresholds
    severity = function(value, thresholds) {
      if (value >= thresholds["ok"]) "ok"
      else if (value >= thresholds["note"]) "note"
      else "important"
    },

    # Function generating a descriptive message
    message = function(value) {
      paste(
        "Cube contains observations across",
        value,
        "taxon keys."
      )
    }
  )

  # Return a cube_rule object
  return(new_cube_rule(rule))
}


#' Minimum number of records diagnostic rule
#'
#' Creates a diagnostic rule that evaluates whether a data cube contains a
#' sufficient number of observation records (rows). The rule counts
#' the number of records present in the cube and compares it to a
#' threshold to determine the severity level.
#'
#' @param thresholds Named numeric vector with severity thresholds:
#' ok, note, important, very_important. Defaults are used if not provided.
#'
#' @return An object of class `cube_rule`.
#'
#' @export
#' @keywords internal

rule_obs_min_records <- function(
  thresholds = c(ok = 40, note = 30, important = 20, very_important = 0)
) {
  rule <- list(
    # Unique identifier for the diagnostic metric
    id = "obs_min_records",

    # Dimension of the cube being evaluated
    dimension = "observation",

    # Minimum recommended number of observation records
    thresholds = thresholds,

    # Function computing the diagnostic metric
    compute = function(cube) {
      # Extract cube data
      data <- get_cube_data(
        data_cube = cube,
        processed_cube = TRUE
      )
      # Count number of observation records
      nrow(data)
    },

    # Function assigning severity based on named thresholds
    severity = function(value, thresholds) {
      if (value >= thresholds["ok"]) "ok"
      else if (value >= thresholds["note"]) "note"
      else if (value >= thresholds["important"]) "important"
      else "very_important"
    },

    # Function generating a descriptive message
    message = function(value) {
      paste(
        "Cube contains",
        value,
        "observation records (rows)."
      )
    }
  )

  # Return a cube_rule object
  return(new_cube_rule(rule))
}


#' Minimum total number of observations diagnostic rule (multi-threshold)
#'
#' Creates a diagnostic rule that evaluates whether a data cube contains a
#' sufficient number of total observations, using a named vector of thresholds
#' for severity classification.
#'
#' @param thresholds Named numeric vector with severity thresholds:
#' ok, note, important, very_important. Defaults are used if not provided.
#'
#' @return An object of class `cube_rule`.
#'
#' @export
#' @keywords internal

rule_obs_min_total <- function(
  thresholds = c(ok = 40, note = 30, important = 20, very_important = 0)
) {
  rule <- list(
    # Unique identifier for the diagnostic metric
    id = "obs_min_total",

    # Dimension of the cube being evaluated
    dimension = "observation",

    # Minimum recommended number of observations
    thresholds = thresholds,

    # Function computing the diagnostic metric
    compute = function(cube) {
      # Extract cube data
      data <- get_cube_data(
        data_cube = cube,
        processed_cube = TRUE
      )
      # Count total number of observations
      sum(data$obs, na.rm = TRUE)
    },

    # Function assigning severity based on named thresholds
    severity = function(value, thresholds) {
      if (value >= thresholds["ok"]) "ok"
      else if (value >= thresholds["note"]) "note"
      else if (value >= thresholds["important"]) "important"
      else "very_important"
    },

    # Function generating a descriptive message
    message = function(value) {
      paste(
        "Cube contains a total of",
        value,
        "observations."
      )
    }
  )

  # Return a cube_rule object
  return(new_cube_rule(rule))
}
