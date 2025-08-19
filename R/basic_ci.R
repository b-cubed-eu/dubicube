#' Basic confidence interval (helper)
#'
#' @param t0 Original statistic.
#' @param replicates Numeric vector of bootstrap replicates.
#' @param conf Confidence level.
#' @param h Transformation function.
#' @param hinv Inverse transformation function.
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

basic_ci <- function(t0, replicates, conf = 0.95,
                     h = identity, hinv = identity) {
  t <- h(replicates)
  t0 <- h(t0)

  alpha <- (1 - conf) / 2
  probs <- c(alpha, 1 - alpha)

  q <- stats::quantile(t, probs, na.rm = TRUE, names = FALSE)

  # Basic CI: 2*t0 - quantiles
  ci <- 2 * t0 - q
  ci <- hinv(ci)

  stats::setNames(ci, c("ll", "ul"))
}
