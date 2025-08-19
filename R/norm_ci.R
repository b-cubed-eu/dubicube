#' Normal confidence interval (helper)
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
#' @note
#' This function is adapted from the function `norm.ci()`
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
#' @importFrom stats qnorm sd
#'

norm_ci <- function(
    t0,
    t,
    conf = 0.95,
    h = function(t) t,
    hinv = function(t) t) {
  # Keep only finite bootstrap replicates
  fins <- seq_along(t)[is.finite(t)]
  t_h <- h(t[fins])

  # Transform original statistic
  t0_h <- h(t0)

  # Estimated bias
  bias <- mean(t_h) - t0_h

  # Margin of error (sd * z)
  merr <- stats::sd(t_h) * stats::qnorm((1 + conf) / 2)

  # normal-based CI, back-transform with hinv
  ll <- hinv(t0_h - bias - merr)
  ul <- hinv(t0_h - bias + merr)

  out <- matrix(c(conf, ll, ul), nrow = 1)
  colnames(out) <- c("conf", "ll", "ul")

  return(out)
}
