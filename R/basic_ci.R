#' Basic confidence interval (helper)
#'
#' @param t0 Original statistic.
#' @param t Numeric vector of bootstrap replicates.
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
#' @details
#'
#' @note
#' This function is adapted from the internal function `basic.ci()`
#' in the \pkg{boot} package (Davison & Ripley, R Core Team).
#' Credit: \pkg{boot} authors (A. C. Davison, B. D. Ripley, and R Core Team).
#' Licensed under the same terms as R itself.
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

basic_ci <- function(
    t0,
    t,
    conf = 0.95,
    h = function(t) t,
    hinv = function(t) t) {
  # Two-sided alpha levels for the CI
  alpha <- (1 + c(-conf, conf)) / 2

  # Interpolate on the normal quantile scale
  qq <- norm_inter(h(t), alpha)

  # build output in the same style as boot:::basic.ci:
  #   [conf, rk, CI values] for lower and upper limits
  out <- matrix(c(conf, qq[1, 1L], qq[2, 1L], hinv(2 * h(t0) - qq[2, 2]),
                  hinv(2 * h(t0) - qq[1, 2])),
                nrow = 1L)
  colnames(out) <- c("conf", "rk_lower", "rk_upper", "ll", "ul")

  return(out)
}
