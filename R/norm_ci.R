#' Normal confidence interval (helper)
#'
#' @param t0 Original statistic.
#' @param t Numeric vector of bootstrap replicates.
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
    t,
    conf = 0.95,
    h = function(t) t,
    hinv = function(t) t) {

  fins <- seq_along(t)[is.finite(t)]
  t <- h(t[fins])

  var_t0 <- var(t)

  t0 <- h(t0)
  bias <- mean(t) - t0

  merr <- sqrt(var_t0) * qnorm((1 + conf) / 2)
  cbind(conf, hinv(t0 - bias - merr), hinv(t0 - bias + merr))
}
