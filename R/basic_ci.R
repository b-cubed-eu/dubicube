# nolint start: line_length_linter.
#' Calculate basic bootstrap confidence interval
#'
#' This function calculates a basic confidence interval from a bootstrap sample.
#' It is used by `calculate_bootstrap_ci()`.
#'
#' @param t0 Original statistic.
#' @param t Numeric vector of bootstrap replicates.
#' @param conf A numeric value specifying the confidence level of the interval.
#' Default is `0.95` (95 % confidence level).
#' @param h A function defining a transformation. The intervals are calculated
#' on the scale of `h(t)` and the inverse function `hinv` applied to the
#' resulting intervals. It must be a function of one variable only. The default
#' is the identity function.
#' @param hinv A function, like `h`, which returns the inverse of `h`. It is
#' used to transform the intervals calculated on the scale of `h(t)` back to the
#' original scale. The default is the identity function. If `h` is supplied but
#' `hinv` is not, then the intervals returned will be on the transformed scale.
#'
#' @return A matrix with four columns:
#'   - `conf`: confidence level
#'   - `rk_lower`: rank of lower endpoint (interpolated)
#'   - `rk_upper`: rank of upper endpoint (interpolated)
#'   - `ll`: lower confidence limit
#'   - `ul`: lower confidence limit
#'
#' @details
#' \deqn{CI_{basic} = \left[ 2\hat{\theta} - \hat{\theta}^*_{(1-\alpha/2)},
#' 2\hat{\theta} - \hat{\theta}^*_{(\alpha/2)} \right]}
#'
#' where \eqn{\hat{\theta}^*_{(\alpha/2)}} and
#' \eqn{\hat{\theta}^*_{(1-\alpha/2)}} are the \eqn{\alpha/2} and
#' \eqn{1-\alpha/2} percentiles of the bootstrap distribution, respectively.
#'
#' @note
#' This function is adapted from the internal function `basic.ci()`
#' in the \pkg{boot} package (Canty & Ripley, 1999).
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
#' @examples
#' set.seed(123)
#' boot_reps <- rnorm(1000)
#' t0 <- mean(boot_reps)
#'
#' # Basic bootstrap CI
#' basic_ci(t0, boot_reps, conf = 0.95)
# nolint end

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
