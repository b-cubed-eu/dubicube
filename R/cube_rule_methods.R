#' Print a cube_rule object
#'
#' Prints a concise summary of a diagnostic rule used by
#' [diagnose_cube()]. The output shows the rule identifier,
#' cube dimension, and threshold used to determine severity.
#'
#' @param x An object of class `cube_rule`.
#' @param ... Additional arguments passed to other methods (currently unused).
#'
#' @return The input object `x`, returned invisibly.
#'
#' @method print cube_rule
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


#' Print a list of cube diagnostic rules
#'
#' Displays a compact overview of a collection of diagnostic rules used by
#' [diagnose_cube()]. Each rule is shown with its identifier and the cube
#' dimension it evaluates.
#'
#' @param x An object of class `cube_rule_list`.
#' @param ... Additional arguments passed to other methods (currently unused).
#'
#' @return The input object `x`, returned invisibly.
#'
#' @method print cube_rule_list
#' @export
#' @keywords internal

print.cube_rule_list <- function(x, ...) {
  cat("<cube_rule_list>\n")
  cat("  rules:", length(x), "\n\n")

  for (i in seq_along(x)) {
    rule <- x[[i]]

    id <- if (!is.null(rule$id)) rule$id else "<unknown>"
    dim <- if (!is.null(rule$dimension)) rule$dimension else "<unknown>"

    cat("  -", id, "(", dim, ")\n")
  }

  invisible(x)
}
