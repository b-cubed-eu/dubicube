#' Bias-Corrected and Accelerated (BCa) confidence interval (helper)
#'
#' @param t0 Original statistic.
#' @param replicates Numeric vector of bootstrap replicates.
#' @param a Acceleration constant.
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

bca_ci <- function(
    t0,
    replicates,
    a,
    conf = 0.95,
    h = function(t) t,
    hinv = function(t) t) {
  t <- h(replicates)
  t0 <- h(t0)
  R <- length(t)

  # Bias correction
  z0 <- stats::qnorm(sum(t < t0) / R)

  # Critical values
  alpha <- (1 + c(-conf, conf)) / 2
  z_alpha <- stats::qnorm(alpha)

  # Adjusted alpha
  adj_alpha <- stats::pnorm(
    z0 + (z0 + z_alpha) / (1 - a * (z0 + z_alpha))
  )

  # Interpolation on normal quantile scale
  qq <- norm_inter(t, adj_alpha) # <- your own version, not boot:::
  ci <- hinv(h(qq[, 2]))

  stats::setNames(ci, c("ll", "ul"))
}
