#' Print cube diagnostics
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


#' Summarise cube diagnostics
#'
#' Provides a summary of diagnostic results returned by
#' [diagnose_cube()]. The summary reports the number of evaluated
#' rules, counts per severity level, and the number of diagnostics
#' per cube dimension.
#'
#' @param x A `cube_diagnostics` object returned by `diagnose_cube()`.
#' @param ... Additional arguments passed to other methods (currently unused).
#'
#' @return An object of class `summary_cube_diagnostics`, containing
#' aggregated diagnostic information.
#'
#' @method summary cube_diagnostics
#' @export

summary.cube_diagnostics <- function(x, ...) {
  # Count diagnostics by severity
  severity_counts <- table(x$severity)

  # Count diagnostics by dimension
  dimension_counts <- table(x$dimension)

  # Diagnostics that are not ok
  flagged <- x[x$severity != "ok", , drop = FALSE]

  out <- list(
    n_rules = nrow(x),
    severity = severity_counts,
    dimensions = dimension_counts,
    flagged = flagged
  )
  class(out) <- "summary_cube_diagnostics"

  out
}


#' Print summary diagnostics
#'
#' Displays a human-readable summary of data cube diagnostics.
#'
#' @export

print.summary_cube_diagnostics <- function(x, ...) {
  cat("<cube_diagnostics_summary>\n\n")
  cat("Rules evaluated:", x$n_rules, "\n\n")
  cat("Severity levels:\n")
  print(x$severity)

  cat("\nDimensions:\n")
  print(x$dimensions)

  if (nrow(x$flagged) > 0) {
    cat("\nFlagged diagnostics:\n")

    flagged <- x$flagged[, c("metric", "dimension", "severity")]
    rownames(flagged) <- NULL

    print(flagged, sort_summary = "asc")
  } else {
    cat("\nAll diagnostics are OK.\n")
  }

  invisible(x)
}

#' Plot cube diagnostics
#'
#' Visualises diagnostic results returned by [diagnose_cube()]. The plot
#' summarises the number of diagnostics per severity level and cube dimension.
#'
#' @param x A `cube_diagnostics` object returned by `diagnose_cube()`.
#' @param type Type of plot. Options are `"severity"` (default),
#' `"dimension"`, or `"heatmap"`.
#' @param ... Additional arguments passed to other methods (currently unused).
#'
#' @return A `ggplot` object.
#'
#' @details
#' Three visualisations are supported:
#'
#' \itemize{
#' \item `"severity"`: Number of diagnostics per severity level.
#' \item `"dimension"`: Diagnostics grouped by cube dimension.
#' \item `"heatmap"`: Severity levels per diagnostic rule and dimension.
#' }
#'
#' @method plot cube_diagnostics
#' @export
#' @import ggplot2
#' @importFrom rlang .data

plot.cube_diagnostics <- function(x, type = "severity", ...) {
  # Sort severity levels
  severity_levels <- c("ok", "note", "important", "very_important")
  x$severity <- factor(
    x$severity,
    levels = severity_levels,
    ordered = TRUE
  )

  # Create plot by severity level
  if (type == "severity") {
    # Count by severity
    df <- as.data.frame(table(x$severity))
    colnames(df) <- c("severity", "count")

    # Create plot
    p <- ggplot2::ggplot(
      df,
      ggplot2::aes(.data$severity, .data$count, fill = .data$severity)
    ) +
      ggplot2::geom_col(show.legend = TRUE) +
      ggplot2::labs(
        title = "Cube diagnostics by severity",
        x = "Severity level",
        y = "Number of diagnostics"
      ) +
      ggplot2::theme_minimal()

    # Create plot by dimension
  } else if (type == "dimension") {
    # Count by dimension
    df <- as.data.frame(table(x$dimension, x$severity))
    colnames(df) <- c("dimension", "severity", "count")

    # Create plot
    p <- ggplot2::ggplot(
      df,
      ggplot2::aes(.data$dimension, .data$count, fill = .data$severity)
    ) +
      ggplot2::geom_col(position = "stack", show.legend = TRUE) +
      ggplot2::labs(
        title = "Cube diagnostics by dimension",
        x = "Cube dimension",
        y = "Number of diagnostics"
      ) +
      ggplot2::theme_minimal()

    # Create plot by rule
  } else if (type == "heatmap") {
    p <- ggplot2::ggplot(
      x,
      ggplot2::aes(.data$metric, .data$dimension, fill = .data$severity)
    ) +
      ggplot2::geom_tile(color = "white", show.legend = TRUE) +
      ggplot2::labs(
        title = "Cube diagnostics heatmap",
        x = "Diagnostic rule",
        y = "Cube dimension"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )
  } else {
    stop(
      "`type` must be one of 'severity', 'dimension', or 'heatmap'.",
      call. = FALSE
    )
  }

  # Add manual colours
  p <- p +
    ggplot2::scale_fill_manual(
      values = c(
        ok = "#4CAF50",
        note = "#FFC107",
        important = "#FF9800",
        very_important = "#F44336"
      ),
      labels = c(
        ok = "ok",
        note = "note",
        important = "important",
        very_important = "very important"
      ),
      drop = FALSE
    )

  return(p)
}
