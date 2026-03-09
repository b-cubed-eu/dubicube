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
#'   \item `threshold` – reference value used to determine severity
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
#' @return An object of class `cube_rule`.

rule_temporal_min_years <- function() {
  rule <- list(
    # Unique identifier for the diagnostic metric
    id = "temporal_min_points",

    # Dimension of the cube being evaluated
    dimension = "temporal",

    # Minimum recommended number of years
    threshold = 5,

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

    # Function assigning a severity level
    severity = function(value, threshold) {
      if (value >= threshold) "ok"
      else if (value >= 3) "note"
      else "important"
    },

    # Function generating a descriptive message
    message = function(value, threshold) {
      paste(
        "Cube contains observations across",
        value,
        "years."
      )
    }
  )

  return(new_cube_rule(rule))
}

#' Temporal gaps diagnostic rule
#'
#' Creates a diagnostic rule that evaluates whether a data cube contains
#' missing years. The rule counts the number of missing years present in the
#' cube and compares it to a threshold to determine the severity level.
#'
#' @return An object of class `cube_rule`.

rule_temporal_missing_years <- function() {
  rule <- list(
    # Unique identifier for the diagnostic metric
    id = "temporal_missing_years",

    # Dimension of the cube being evaluated
    dimension = "temporal",

    # Threshold number of missing years
    threshold = 3,

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

    # Function assigning a severity level
    severity = function(value, threshold) {
      if (value >= threshold) "important"
      else if (value >= 1) "note"
      else "ok"
    },

    # Function generating a descriptive message
    message = function(value, threshold) {
      paste(
        "Cube contains",
        value,
        "missing years."
      )
    }
  )

  return(new_cube_rule(rule))
}


#' Spatial minimum grid cells diagnostic rule
#'
#' Creates a diagnostic rule that evaluates whether a data cube contains a
#' sufficient number of spatial observations (grid cells). The rule counts
#' the number of unique grid cells present in the cube and compares it to a
#' threshold to determine the severity level.
#'
#' @return An object of class `cube_rule`.

rule_spatial_min_cells <- function() {
  rule <- list(
    # Unique identifier for the diagnostic metric
    id = "spatial_min_cells",

    # Dimension of the cube being evaluated
    dimension = "spatial",

    # Minimum recommended number of grid cells
    threshold = 5,

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

    # Function assigning a severity level
    severity = function(value, threshold) {
      if (value >= threshold) "ok"
      else if (value >= 3) "note"
      else "important"
    },

    # Function generating a descriptive message
    message = function(value, threshold) {
      paste(
        "Cube contains observations across",
        value,
        "grid cells."
      )
    }
  )

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
#' @return An object of class `cube_rule`.

rule_spatial_max_uncertainty <- function() {
  rule <- list(
    # Unique identifier for the diagnostic metric
    id = "spatial_max_uncertainty",

    # Dimension of the cube being evaluated
    dimension = "spatial",

    # Threshold number of records with high coordinate uncertainty
    threshold = 5,

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

    # Function assigning a severity level
    severity = function(value, threshold) {
      if (value >= threshold) "very_important"
      else if (value >= 3) "important"
      else if (value >= 1) "note"
      else "ok"
    },

    # Function generating a descriptive message
    message = function(value, threshold) {
      paste(
        "Cube contains",
        value,
        "records where the coordinate uncertainty is larger than the",
        "grid cell resolution."
      )
    }
  )

  return(new_cube_rule(rule))
}


#' Missing coordinate uncertainty diagnostic rule
#'
#' Creates a diagnostic rule that evaluates whether a data cube contains a
#' records with missing coordinate uncertainty. The rule counts the number of
#' records (rows) with missing coordinate uncertainty and compares it to a
#' threshold to determine the severity level.
#'
#' @return An object of class `cube_rule`.

rule_spatial_miss_uncertainty <- function() {
  rule <- list(
    # Unique identifier for the diagnostic metric
    id = "spatial_miss_uncertainty",

    # Dimension of the cube being evaluated
    dimension = "spatial",

    # Threshold number of records with missing coordinate uncertainty
    threshold = 5,

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

    # Function assigning a severity level
    severity = function(value, threshold) {
      if (value >= threshold) "very_important"
      else if (value >= 3) "important"
      else if (value >= 1) "note"
      else "ok"
    },

    # Function generating a descriptive message
    message = function(value, threshold) {
      paste(
        "Cube contains",
        value,
        "records with missing coordinate uncertainty."
      )
    }
  )

  return(new_cube_rule(rule))
}


#' Taxonomic minimum taxa diagnostic rule
#'
#' Creates a diagnostic rule that evaluates whether a data cube contains a
#' sufficient number of taxonomical observations (taxa). The rule counts
#' the number of unique taxa present in the cube and compares it to a
#' threshold to determine the severity level.
#'
#' @return An object of class `cube_rule`.

rule_taxon_min_taxa <- function() {
  rule <- list(
    # Unique identifier for the diagnostic metric
    id = "taxon_min_taxa",

    # Dimension of the cube being evaluated
    dimension = "taxonomic",

    # Minimum recommended number of taxa
    threshold = 5,

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

    # Function assigning a severity level
    severity = function(value, threshold) {
      if (value >= threshold) "ok"
      else if (value >= 3) "note"
      else "important"
    },

    # Function generating a descriptive message
    message = function(value, threshold) {
      paste(
        "Cube contains observations across",
        value,
        "taxon keys."
      )
    }
  )

  return(new_cube_rule(rule))
}


#' Minimum number of records diagnostic rule
#'
#' Creates a diagnostic rule that evaluates whether a data cube contains a
#' sufficient number of observation records (rows). The rule counts
#' the number of records present in the cube and compares it to a
#' threshold to determine the severity level.
#'
#' @return An object of class `cube_rule`.

rule_obs_min_records <- function() {
  rule <- list(
    # Unique identifier for the diagnostic metric
    id = "obs_min_records",

    # Dimension of the cube being evaluated
    dimension = "observation",

    # Minimum recommended number of observation records
    threshold = 40,

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

    # Function assigning a severity level
    severity = function(value, threshold) {
      if (value >= threshold) "ok"
      else if (value >= 30) "note"
      else if (value >= 20) "important"
      else "very_important"
    },

    # Function generating a descriptive message
    message = function(value, threshold) {
      paste(
        "Cube contains",
        value,
        "observation records (rows)."
      )
    }
  )

  return(new_cube_rule(rule))
}


#' Minimum total number of observations diagnostic rule
#'
#' Creates a diagnostic rule that evaluates whether a data cube contains a
#' sufficient number of total observations. The rule sums
#' the number of observations present in the cube and compares it to a
#' threshold to determine the severity level.
#'
#' @return An object of class `cube_rule`.

rule_obs_min_total <- function() {
  rule <- list(
    # Unique identifier for the diagnostic metric
    id = "obs_min_total",

    # Dimension of the cube being evaluated
    dimension = "observation",

    # Minimum recommended number of observations
    threshold = 40,

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

    # Function assigning a severity level
    severity = function(value, threshold) {
      if (value >= threshold) "ok"
      else if (value >= 30) "note"
      else if (value >= 20) "important"
      else "very_important"
    },

    # Function generating a descriptive message
    message = function(value, threshold) {
      paste(
        "Cube contains a total of",
        value,
        "observations."
      )
    }
  )

  return(new_cube_rule(rule))
}
