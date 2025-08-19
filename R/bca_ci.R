#' Bias-Corrected and Accelerated (BCa) confidence interval (helper)
#'
#' @param t0 Original statistic.
#' @param t Numeric vector of bootstrap replicates.
#' @param a Acceleration constant.
#' @param conf Confidence level.
#' @param h Transformation function.
#' @param hinv Inverse transformation function.
#'
#' @return A matrix with four columns:
#'   \describe{
#'     \item{conf}{confidence level}
#'     \item{rk_lower}{rank of lower endpoint (interpolated)}
#'     \item{rk_upper}{rank of upper endpoint (interpolated)}
#'     \item{ll}{lower confidence limit}
#'     \item{ul}{upper confidence limit}
#'   }
#'
#' @note
#' This function is adapted from the internal function `bca.ci()`
#' in the \pkg{boot} package (Davison & Ripley, 1999).
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
#' @importFrom stats pnorm qnorm
#'

bca_ci <- function(
    t0,
    t,
    a,
    conf = 0.95,
    h = function(t) t,
    hinv = function(t) t) {
  # Two-sided alpha levels for the CI
  alpha <- (1 + c(-conf, conf)) / 2
  zalpha <- stats::qnorm(alpha)

  # Calculate bias correction factor z0
  z0 <- stats::qnorm(sum(t < t0) / length(t))
  if (!is.finite(z0)) {
    warning("Estimated adjustment 'z0' is infinite.")
    return(cbind(conf, ll = NA, ul = NA))
  }

  # Adjust alpha values for acceleration
  adj_alpha <- stats::pnorm(z0 + (z0 + zalpha) /
                              (1 - a * (z0 + zalpha)))

  # Interpolate on the normal quantile scale
  qq <- norm_inter(h(t), adj_alpha)

  # build output in the same style as boot:::bca.ci:
  #   [conf, rk, CI values] for lower and upper limits
  out <- matrix(c(conf, qq[1, 1L], qq[2, 1L], hinv(qq[1, 2]), hinv(qq[2, 2])),
                nrow = 1)
  colnames(out) <- c("conf", "rk_lower", "rk_upper", "ll", "ul")

  return(out)
}
