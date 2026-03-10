#' Filter a processed data cube using diagnostic rules
#'
#' Filters observations from a `processed_cube` based on rule definitions.
#' Filtering reuses the rule infrastructure used by [diagnose_cube()], but
#' applies row-level filtering logic through rule-specific `filter_fn()`
#' functions.
#'
#' @param data_cube A `processed_cube` object.
#' @param rules Character vector or list of cube rule objects.
#' Ignored if `diagnostics` is supplied.
#' @param diagnostics Optional `cube_diagnostics` object returned by
#' [diagnose_cube()]. If provided, rules are extracted from this object.
#' @param ... Additional arguments passed to rule-specific `filter_fn()`
#' functions.
#'
#' @return A filtered `processed_cube`.
#'
#' @details
#' The function evaluates rule-specific `filter_fn()` functions that return
#' a logical vector indicating which rows should be removed. Only rules that
#' implement a `filter_fn()` are applied. Rules without a filtering function
#' are ignored.
#'
#' Filtering rules operate independently from diagnostic severity levels.
#' For example, a cube may have acceptable overall diagnostics while still
#' containing individual observations that fail filtering criteria.
#'
#' After filtering, the function attempts to rebuild the cube using
#' `b3gbi::process_cube()` to ensure cube metadata remains consistent.
#' If this function is unavailable or fails, the filtered data replaces
#' `data_cube$data` directly and the original cube metadata is retained.
#' In that case a warning is issued.
#'
#' @export
#'
#' @family data_exploration
#'
#' @examples
#' \dontrun{
#' # After processing a data cube with b3gbi::process_cube()
#'
#' # Filter cube based on rule
#' filtered_cube1 <- filter_cube(
#'   processed_cube,
#'   rules = list(rule_spatial_miss_uncertainty())
#' )
#'
#' # Filter cube based cube diagnostics
#' diag <- diagnose_cube(
#'   processed_cube,
#'   rules = list(
#'     rule_spatial_miss_uncertainty(),
#'     rule_temporal_min_years()
#'   )
#' )
#'
#' filtered_cube2 <- filter_cube(
#'   processed_cube,
#'   diagnostics = diag
#' )
#'
#' # The results are identical
#' identical(filtered_cube1$data, filtered_cube2$data)
#' }

filter_cube <- function(
    data_cube,
    rules = NULL,
    diagnostics = NULL,
    ...) {
  stopifnot(
    "`data_cube` must be of class 'processed_cube'" =
      inherits(data_cube, "processed_cube")
  )

  # Extract rules
  if (!is.null(diagnostics)) {
    stopifnot(
      "`diagnostics` must be of class 'cube_diagnostics'" =
        inherits(diagnostics, "cube_diagnostics")
    )

    rules <- attr(diagnostics, "rules")
  } else {
    rules <- resolve_cube_rules(rules)
  }

  # Extract cube data
  data <- get_cube_data(
    data_cube = data_cube,
    processed_cube = TRUE
  )

  # Initialize drop vector
  drop_rows <- rep(FALSE, nrow(data))

  # Apply filtering rules
  for (rule in rules) {
    if (!is.null(rule$filter_fn)) {
      flags <- rule$filter_fn(
        data_cube,
        ...
      )

      if (!is.logical(flags) || length(flags) != nrow(data)) {
        stop(
          paste("`filter_fn()` must return a logical vector with length",
                "equal to the number of rows in the cube."),
          call. = FALSE
        )
      }

      drop_rows <- drop_rows | flags
    }
  }

  # Filter observations
  data_filtered <- data[!drop_rows, , drop = FALSE]

  # Try rebuilding cube using b3gbi
  cube_new <- tryCatch(
    b3gbi::process_cube(data_filtered),
    error = function(e) NULL,
    warning = function(w) NULL
  )

  # Return cubes
  if (!is.null(cube_new)) return(cube_new)

  data_cube$data <- data_filtered

  warning(
    paste(
      "Filtered data replaced the cube data but metadata was not recomputed.",
      "Install and use `b3gbi::process_cube()` to rebuild cube metadata.",
      sep = "\n"
    ),
    call. = FALSE
  )

  return(data_cube)
}
