#' Print method for cube_diagnostics objects
#'
#' Displays a human-readable summary of data cube diagnostics produced by
#' `diagnose_cube()`. Each diagnostic metric is shown with a severity flag,
#' the metric name, and a short explanatory message.
#'
#' @param x A `cube_diagnostics` object returned by `diagnose_cube()`.
#' @param filter_summary Filter the summary output based on a minimum severity
#' level. Default, all levels are shown: `filter_summary = "ok"`.
#' @param sort_summary Sort the summary output based on severity level. Options
#' are descending (`"desc"`), ascending (`"asc"`) or no sorting (`NA`, default).
#' Default, all levels are shown: `filter_summary = "ok"`.
#' @param ... Additional arguments passed to other methods (currently unused).
#'
#' @return The input object `x`, returned invisibly.
#'
#' @details
#' Severity levels are indicated using coloured symbols:
#' \itemize{
#'   \item 🟢 ok
#'   \item 🟡 note
#'   \item 🟠 important
#'   \item 🔴 very important
#' }
#'
#' @method print cube_diagnostics
#' @export

print.cube_diagnostics <- function(
    x,
    filter_summary = "ok",
    sort_summary = NA,
    ...) {
  # Mapping between severity levels and visual symbols
  sev_symbols <- c(
    ok = "🟢",
    note = "🟡",
    important = "🟠",
    very_important = "🔴"
  )

  # Filter output
  x$severity <- factor(
    x$severity,
    levels = names(sev_symbols),
    ordered = TRUE
  )
  x <- x[x$severity >= filter_summary, ]

  # Sort output
  if (!is.na(sort_summary)) {
    if (sort_summary == "desc") {
      x <- x[order(x$severity, decreasing = TRUE), ]
    } else if (sort_summary == "asc") {
      x <- x[order(x$severity, decreasing = FALSE), ]
    } else {
      stop("`sort_summary` must be one of NA, 'desc' or 'asc'.", call. = FALSE)
    }
  }

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
