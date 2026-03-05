#' Print method for cube_diagnostics objects
#'
#' Displays a human-readable summary of data cube diagnostics produced by
#' `diagnose_cube()`. Each diagnostic metric is shown with a severity flag,
#' the metric name, and a short explanatory message.
#'
#' Severity levels are indicated using coloured symbols:
#' \itemize{
#'   \item 🟢 ok
#'   \item 🟡 note
#'   \item 🟠 important
#'   \item 🔴 very important
#' }
#'
#' @param x A `cube_diagnostics` object returned by `diagnose_cube()`.
#' @param ... Additional arguments passed to other methods (currently unused).
#'
#' @return The input object `x`, returned invisibly.
#'
#' @method print cube_diagnostics
#' @export

print.cube_diagnostics <- function(x, ...) {

  # Mapping between severity levels and visual symbols
  sev_symbols <- c(
    ok = "🟢",
    note = "🟡",
    important = "🟠",
    very_important = "🔴"
  )

  # Header
  cat("\nData cube diagnostics\n")
  cat("----------------------\n")

  # Print each diagnostic metric
  for (i in seq_len(nrow(x))) {

    sym <- sev_symbols[[x$severity[i]]]

    cat(
      sym,
      toupper(x$severity[i]),
      "-",
      x$metric[i],
      "\n  ",
      x$message[i],
      "\n\n"
    )

  }

  invisible(x)
}
