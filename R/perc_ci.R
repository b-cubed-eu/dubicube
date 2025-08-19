#' Percentile confidence interval (helper)
#'
#' @param replicates Numeric vector of bootstrap replicates.
#' @param conf Confidence level (default = 0.95).
#' @param h Transformation function (default = identity).
#' @param hinv Inverse transformation function (default = identity).
#'
#' @return Named numeric vector with lower (`ll`) and upper (`ul`) limits.
#'
#' @details
#'
#' @references
#' Canty, A., & Ripley, B. (1999). boot: Bootstrap Functions (Originally by
#' Angelo Canty for S) \[Computer software\].
#' \url{https://CRAN.R-project.org/package=boot}
#'
#' Davison, A. C., & Hinkley, D. V. (1997). Bootstrap Methods and their
#' Application (1st ed.). Cambridge University Press.
#' \doi{10.1017/CBO9780511802843}
#'
#' @export
#'
#' @family interval_calculation
#'
#' @import
#'
#' @examples
#'

perc_ci <- function(replicates, conf = 0.95,
                    h = identity, hinv = identity) {
  # Transform replicates
  t <- h(replicates)

  # Compute percentile bounds
  alpha <- (1 - conf) / 2
  probs <- c(alpha, 1 - alpha)

  ci <- stats::quantile(t, probs, na.rm = TRUE, names = FALSE)

  # Transform back
  ci <- hinv(ci)

  stats::setNames(ci, c("ll", "ul"))
}
