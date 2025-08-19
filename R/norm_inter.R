#' Interpolation on the Normal Quantile Scale
#'
#' This function performs interpolation between order statistics
#' on the normal quantile scale, as described in Davison and Hinkley (1997,
#' eq. 5.8). It is used internally by the \pkg{boot} package (Canty & Ripley,
#' 1999) for bootstrap confidence interval calculations (normal-based
#' interpolation).
#'
#' @param t A numeric vector of bootstrap replicates (finite values only are
#' used).
#' @param alpha A numeric vector of probabilities (between 0 and 1) at which to
#' interpolate.
#'
#' @return A two-column matrix with:
#'   \itemize{
#'     \item First column: the fractional order statistic position
#'     \eqn{(B + 1) \alpha}, rounded to two decimals. \eqn{B} is the number of
#'     bootstrap samples.
#'     \item Second column: the interpolated value corresponding to that
#'     quantile.
#'   }
#'
#' @details
#' For non-integer order statistics, interpolation is carried out between
#' the two surrounding order statistics on the scale of normal quantiles.
#' If \eqn{(B + 1)\alpha} is exactly an integer, the corresponding order
#' statistic is returned. Extreme values (below the first or above the last
#' order statistic) are truncated and a warning is issued.
#'
#' @note
#' This function is adapted from the internal function `norm.inter()`
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
#' @importFrom stats qnorm
#'
#' @noRd

norm_inter <- function(t, alpha) {
  # Keep only finite values
  t <- t[is.finite(t)]
  n_rep <- length(t)  # number of replicates

  # Rank position for the given alpha
  rk <- (n_rep + 1) * alpha

  # Warn if interpolation would use extreme endpoints
  if (!all(rk > 1 & rk < n_rep))
    warning("Extreme order statistics used as endpoints.")

  # Floor of rk gives index of lower order statistic
  k <- trunc(rk)
  inds <- seq_along(k)
  out <- inds

  # Indices that are within bounds
  kvs <- k[k > 0 & k < n_rep]

  # Efficient partial sorting (only needed order statistics)
  tstar <- sort(t, partial = sort(union(c(1, n_rep), c(kvs, kvs + 1))))

  # Case 1: exact integer order statistic
  ints <- (k == rk)
  if (any(ints))
    out[inds[ints]] <- tstar[k[inds[ints]]]

  # Case 2: below or above the range → clamp to extremes
  out[k == 0] <- tstar[1L]
  out[k == n_rep] <- tstar[n_rep]

  # Case 3: interpolation on normal quantile scale
  temp <- inds[!ints & k != 0 & k != n_rep]
  if (length(temp) > 0) {
    temp1 <- stats::qnorm(alpha[temp])  # target quantile (on normal scale)
    temp2 <- stats::qnorm(k[temp] / (n_rep + 1))       # lower bounding quantile
    temp3 <- stats::qnorm((k[temp] + 1) / (n_rep + 1)) # upper bounding quantile
    tk <- tstar[k[temp]]                # lower order statistic
    tk1 <- tstar[k[temp] + 1L]          # upper order statistic

    # Linear interpolation on normal scale
    out[temp] <- tk + (temp1 - temp2) / (temp3 - temp2) * (tk1 - tk)
  }

  # Return matrix: rank positions and interpolated values
  cbind(round(rk, 2), out)
}
