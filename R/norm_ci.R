# nolint start: line_length_linter.
#' Calculate normal bootstrap confidence interval
#'
#' This function calculates a normal confidence interval from a bootstrap
#' sample. It is used by `calculate_bootstrap_ci()`.
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
#' @param no_bias Logical. If `TRUE` intervals are centered around the original
#' estimates (bias is ignored). Default is `FALSE`.
#'
#' @return A matrix with four columns:
#'   - `conf`: confidence level
#'   - `ll`: lower confidence limit
#'   - `ul`: lower confidence limit
#'
#' @details
#' \deqn{CI_{norm} = \left[\hat{\theta} - \text{Bias}_{\text{boot}} - \text{SE}_{\text{boot}} \times z_{1-\alpha/2},
#' \hat{\theta} - \text{Bias}_{\text{boot}} + \text{SE}_{\text{boot}} \times z_{1-\alpha/2} \right]}
#'
#' where \eqn{z_{1-\alpha/2}} is the \eqn{1-\alpha/2} quantile of the
#' standard normal distribution.
#'
#' @note
#' This function is adapted from the function `norm.ci()`
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
#' @importFrom stats qnorm sd
# nolint end

norm_ci <- function(
    t0,
    t,
    conf = 0.95,
    h = function(t) t,
    hinv = function(t) t,
    no_bias = FALSE) {
  # Keep only finite bootstrap replicates
  fins <- seq_along(t)[is.finite(t)]
  t_h <- h(t[fins])

  # Transform original statistic
  t0_h <- h(t0)

  # Estimated bias
  bias <- ifelse(no_bias, 0, mean(t_h) - t0_h)

  # Margin of error (sd * z)
  merr <- stats::sd(t_h) * stats::qnorm((1 + conf) / 2)

  # normal-based CI, back-transform with hinv
  ll <- hinv(t0_h - bias - merr)
  ul <- hinv(t0_h - bias + merr)

  out <- matrix(c(conf, ll, ul), nrow = 1)
  colnames(out) <- c("conf", "ll", "ul")

  return(out)
}
