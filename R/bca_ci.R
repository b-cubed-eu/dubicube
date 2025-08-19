#' Bias-Corrected and Accelerated (BCa) confidence interval (helper)
#'
#' @param t0 Original statistic.
#' @param t Numeric vector of bootstrap replicates.
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
  # Calculate the BCa critical values
  alpha <- (1 + c(-conf, conf)) / 2
  zalpha <- stats::qnorm(alpha)

  z0 <- stats::qnorm(sum(t < t0) / length(t))
  if (!is.finite(z0)) {
    warning("Estimated adjustment 'z0' is infinite.")
    return(cbind(conf, ll = NA, ul = NA))
  }

  # Adjust for acceleration
  adj_alpha <- stats::pnorm(z0 + (z0 + zalpha) /
                              (1 - a * (z0 + zalpha)))
  qq <- norm_inter(t, adj_alpha)
  qq_matrix <- matrix(hinv(h(qq[, 2L])), ncol = 2L)
  colnames(qq_matrix) <- c("ll", "ul")

  return(cbind(conf, qq_matrix))
}
