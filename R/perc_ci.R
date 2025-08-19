#' Percentile confidence interval (helper)
#'
#' @param t Numeric vector of bootstrap replicates.
#' @param conf Confidence level (default = 0.95).
#' @param h Transformation function (default = identity).
#' @param hinv Inverse transformation function (default = identity).
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

perc_ci <- function(
    t,
    conf = 0.95,
    h = function(t) t,
    hinv = function(t) t) {
  alpha <- (1 + c(-conf, conf)) / 2
  qq <- norm_inter(h(t), alpha)
  cbind(conf, matrix(qq[, 1L], ncol = 2L), matrix(hinv(qq[, 2]), ncol = 2L))
}
