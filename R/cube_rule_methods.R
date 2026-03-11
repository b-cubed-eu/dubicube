#' Print a cube_rule object
#'
#' Prints a concise summary of a diagnostic rule used by
#' [diagnose_cube()]. The output shows the rule identifier,
#' cube dimension, and threshold used to determine severity.
#'
#' @param x An object of class `cube_rule`.
#' @param ... Additional arguments passed to other methods (currently unused).
#'
#' @return The input object, invisibly.
#'
#' @export
#' @keywords internal

print.cube_rule <- function(x, ...) {
  cat("<cube_rule>\n")

  if (!is.null(x$id)) {
    cat("  id:        ", x$id, "\n", sep = "")
  }

  if (!is.null(x$dimension)) {
    cat("  dimension: ", x$dimension, "\n", sep = "")
  }

  if (!is.null(x$thresholds)) {
    thr <- paste(names(x$thresholds), x$thresholds, sep = " = ")
    cat("  thresholds: ", paste(thr, collapse = ", "), "\n", sep = "")
  }

  cat("\n")
  cat("Functions:\n")
  cat("  compute()     metric calculation\n")
  cat("  severity()    severity assignment\n")
  cat("  message()     diagnostic message\n")

  if (!is.null(x$filter_fn)) {
    cat("  filter_fn()   filter rows\n")
  }

  invisible(x)
}
