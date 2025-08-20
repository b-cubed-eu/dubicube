# nolint start: line_length_linter.
#' Calculate Bias-Corrected and Accelerated (BCa) bootstrap confidence interval
#'
#' This function calculates a Bias-Corrected and Accelerated (BCa) confidence
#' interval from a bootstrap sample. It is used by `calculate_bootstrap_ci()`.
#'
#' @param t0 Original statistic.
#' @param t Numeric vector of bootstrap replicates.
#' @param a Acceleration constant. See also `calculate_acceleration()`.
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
#' Adjusts for bias and acceleration.
#' Bias refers to the systematic difference between the observed statistic
#' from the original dataset and the center of the bootstrap distribution of
#' the statistic. The bias correction term is calculated as follows:
#'
#' \deqn{\hat{z}_0 = \Phi^{-1}\left(\frac{\#(\hat{\theta}^*_b < \hat{\theta})}{B}\right)}
#'
#' where \eqn{\#} is the counting operator, counting the number of times
#' \eqn{\hat{\theta}^*_b} is smaller than \eqn{\hat{\theta}}, and
#' \eqn{\Phi^{-1}} the inverse cumulative density function of the standard
#' normal distribution.\eqn{B} is the number of bootstrap samples.
#'
#' Acceleration quantifies how sensitive the variability of the statistic is
#' to changes in the data.
#' See `calculate_acceleration()` on how this is calculated.
#'
#' - \eqn{a=0}: The statistic's variability does not depend on the data
#' (e.g., symmetric distribution)
#' - \eqn{a>0}: Small changes in the data have a large effect on the
#' statistic's variability (e.g., positive skew)
#' - \eqn{a<0}: Small changes in the data have a smaller effect on the
#' statistic's variability (e.g., negative skew).
#'
#' The bias and acceleration estimates are then used to calculate adjusted
#' percentiles.
#'
#' \eqn{\alpha_1 = \Phi\left( \hat{z}_0 + \frac{\hat{z}_0 + z_{\alpha/2}}{1 - \hat{a}(\hat{z}_0 + z_{\alpha/2})} \right)},
#' \eqn{\alpha_2 = \Phi\left( \hat{z}_0 + \frac{\hat{z}_0 + z_{1 - \alpha/2}}{1 - \hat{a}(\hat{z}_0 + z_{1 - \alpha/2})} \right)}
#'
#' So, we get
#'
#' \deqn{CI_{bca} = \left[ \hat{\theta}^*_{(\alpha_1)}, \hat{\theta}^*_{(\alpha_2)} \right]}
#'
#' @note
#' This function is adapted from the internal function `bca.ci()`
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
#' @importFrom stats pnorm qnorm
#'
#' @examples
#' set.seed(123)
#' boot_reps <- rnorm(1000)
#' t0 <- mean(boot_reps)
#'
#' # Example acceleration value (normally estimated via jackknife)
#' a <- 0.01
#'
#' # BCa bootstrap CI
#' bca_ci(t0, boot_reps, a, conf = 0.95)
# nolint end

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
