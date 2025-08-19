#' Normal confidence interval (helper)
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

norm_ci <- function(
    t0,
    replicates,
    conf = 0.95,
    h = function(t) t,
    hinv = function(t) t) {
  t <- h(replicates)
  t0 <- h(t0)

  # Estimate mean, SE, and bias
  t_bar <- mean(t, na.rm = TRUE)
  se <- stats::sd(t, na.rm = TRUE)
  bias <- t_bar - t0

  z <- stats::qnorm((1 + conf) / 2)

  # Normal CI formula
  ci <- c(
    t0 - bias - z * se,
    t0 - bias + z * se
  )
  ci <- hinv(ci)

  stats::setNames(ci, c("ll", "ul"))
}
