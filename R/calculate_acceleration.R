# nolint start: line_length_linter.
#' Calculate acceleration for a statistic in a dataframe
#'
#' This function calculates acceleration values, which quantify the sensitivity
#' of a statistic’s variability to changes in the dataset. Acceleration is used
#' for bias-corrected and accelerated (BCa) confidence intervals in
#' `calculate_bootstrap_ci()`.
#'
#' @param data_cube A data cube object (class
#' 'processed_cube' or 'sim_cube', see `b3gbi::process_cube()`) or a dataframe
#' (from `$data` slot of 'processed_cube' or 'sim_cube'). As used by
#' `bootstrap_cube()`. To limit runtime, we recommend using a
#' dataframe with custom function as `fun`.
#' @param fun A function which, when applied to
#' `data_cube` returns the statistic(s) of interest. This function must return a
#' dataframe with a column `diversity_val` containing the statistic of interest.
#'  As used by `bootstrap_cube()`.
#' @param ... Additional arguments passed on to `fun`.
#' @param grouping_var A character vector specifying the grouping variable(s)
#' for the bootstrap analysis. The function `fun(data_cube, ...)` should return
#' a row per group. The specified variables must not be redundant, meaning they
#' should not contain the same information (e.g., `"time_point"` (1, 2, 3) and
#' `"year"` (2000, 2001, 2002) should not be used together if `"time_point"` is
#' just an alternative encoding of `"year"`).
#' This variable is used to split the dataset into groups for separate
#' acceleration calculations.
#' @param ref_group A string indicating the
#' reference group to compare the statistic with. Default is `NA`, meaning no
#' reference group is used.
#' As used by `bootstrap_cube()`.
#' @param influence_method A string specifying the method used for calculating
#' the influence values.
#'   - `"usual"`: Negative jackknife (default if BCa is selected).
#'   - `"pos"`: Positive jackknife
#' @param progress Logical. Whether to show a progress bar for jackknifing. Set
#' to `TRUE` to display a progress bar, `FALSE` (default) to suppress it.
#'
#' @returns A dataframe containing the acceleration values per `grouping_var`.
#'
#' @details
#' Acceleration quantifies how sensitive the variability of a statistic
#' \eqn{\theta} is to changes in the data.
#'
#' - \eqn{a=0}: The statistic's variability does not depend on the data
#' (e.g., symmetric distribution)
#' - \eqn{a>0}: Small changes in the data have a large effect on the
#' statistic's variability (e.g., positive skew)
#' - \eqn{a<0}: Small changes in the data have a smaller effect on the
#' statistic's variability (e.g., negative skew).
#'
#' It is used for BCa confidence interval calculation, which adjust for
#' bias and skewness in bootstrapped distributions (Davison & Hinkley, 1997,
#' Chapter 5). See also the `empinf()` function of the \pkg{boot} package in R
#' (Canty & Ripley, 1999)). The acceleration is calculated as follows:
#'
#' \deqn{\hat{a} = \frac{1}{6} \frac{\sum_{i = 1}^{n}(I_i^3)}{\left( \sum_{i = 1}^{n}(I_i^2) \right)^{3/2}}}
#'
#' where \eqn{I_i} denotes the influence of data point \eqn{x_i} on the
#' estimation of \eqn{\theta}. \eqn{I_i} can be estimated using jackknifing.
#' Examples are (1) the negative jackknife:
#' \eqn{I_i = (n-1)(\hat{\theta} - \hat{\theta}_{-i})}, and (2) the positive
#' jackknife \eqn{I_i = (n+1)(\hat{\theta}_{-i} - \hat{\theta})}
#' (Frangos & Schucany, 1990). Here, \eqn{\hat{\theta}_{-i}} is the estimated
#' value leaving out the \eqn{i}’th data point \eqn{x_i}. The \pkg{boot}
#' package also offers infinitesimal jackknife and regression estimation.
#' Implementation of these jackknife algorithms can be explored in the
#' future.
#'
#' If a reference group is used, jackknifing is implemented in a different way.
#' Consider \eqn{\hat{\theta} = \hat{\theta}_1 - \hat{\theta}_2} where
#' \eqn{\hat{\theta}_1} is the estimate for the indicator value of a
#' non-reference period (sample size \eqn{n_1}) and \eqn{\hat{\theta}_2} is the
#' estimate for the indicator value of a reference period (sample size
#' \eqn{n_2}). The acceleration is now calculated as follows:
#'
#' \deqn{\hat{a} = \frac{1}{6} \frac{\sum_{i = 1}^{n_1 + n_2}(I_i^3)}{\left( \sum_{i = 1}^{n_1 + n_2}(I_i^2) \right)^{3/2}}}
#'
#' \eqn{I_i} can be calculated using the negative or positive jackknife. Such
#' that
#'
#' \eqn{\hat{\theta}_{-i} = \hat{\theta}_{1,-i} - \hat{\theta}_2 \text{ for } i = 1, \ldots, n_1}, and
#'
#' \eqn{\hat{\theta}_{-i} = \hat{\theta}_{1} - \hat{\theta}_{2,-i} \text{ for } i = n_1 + 1, \ldots, n_1 + n_2}
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
#' Frangos, C. C., & Schucany, W. R. (1990). Jackknife estimation of the
#' bootstrap acceleration constant. Computational Statistics & Data Analysis,
#' 9(3), 271–281. \doi{10.1016/0167-9473(90)90109-U}
#'
#' @export
#'
#' @family indicator_uncertainty
#'
#' @import dplyr
#' @import assertthat
#' @importFrom rlang .data inherits_any
#' @importFrom stats setNames
#'
#' @examples
#' # Get example data
#' # install.packages("b3gbi", repos = "https://b-cubed-eu.r-universe.dev")
#' library(b3gbi)
#' cube_path <- system.file(
#'   "extdata", "denmark_mammals_cube_eqdgc.csv",
#'   package = "b3gbi")
#' denmark_cube <- process_cube(
#'   cube_path,
#'   first_year = 2014,
#'   last_year = 2020)
#'
#' # Function to calculate statistic of interest
#' # Mean observations per year
#' mean_obs <- function(data) {
#'   out_df <- aggregate(obs ~ year, data, mean) # Calculate mean obs per year
#'   names(out_df) <- c("year", "diversity_val") # Rename columns
#'   return(out_df)
#' }
#' mean_obs(denmark_cube$data)
#'
#' # Perform bootstrapping
#' \donttest{
#' bootstrap_mean_obs <- bootstrap_cube(
#'   data_cube = denmark_cube$data,
#'   fun = mean_obs,
#'   grouping_var = "year",
#'   samples = 1000,
#'   seed = 123,
#'   progress = FALSE)
#' head(bootstrap_mean_obs)
#'
#' # Calculate acceleration
#' acceleration_df <- calculate_acceleration(
#'   data_cube = denmark_cube$data,
#'   fun = mean_obs,
#'   grouping_var = "year",
#'   progress = FALSE)
#' acceleration_df
#' }
# nolint end

calculate_acceleration <- function(
    data_cube,
    fun,
    ...,
    grouping_var,
    ref_group = NA,
    influence_method = "usual",
    progress = FALSE) {
  ### Start checks
  # Check if grouping_var is a character vector
  stopifnot("`grouping_var` must be a character vector." =
              is.character(grouping_var))

  # Check if grouping_var contains redundant variables
  check_redundant_grouping_vars(data_cube, grouping_var)

  # Check data_cube input
  cube_message <- paste("`data_cube` must be a data cube object (class",
                        "'processed_cube' or 'sim_cube') or a dataframe.")
  do.call(stopifnot,
          stats::setNames(list(
            rlang::inherits_any(data_cube,
                                c("processed_cube", "sim_cube", "data.frame"))),
            cube_message)
  )

  # Check fun input
  stopifnot("`fun` must be a function." = is.function(fun))

  # Check if ref_group is NA or a number or a string
  stopifnot(
    "`ref_group` must be a numeric/character vector of length 1 or NA." =
      (assertthat::is.number(ref_group) | assertthat::is.string(ref_group) |
         is.na(ref_group)) &
      length(ref_group) == 1)

  # Check if influence_method is 'usual' or 'pos'
  influence_method <- tryCatch({
    match.arg(influence_method, c("usual", "pos"))
  }, error = function(e) {
    stop("`influence_method` must be one of 'usual', 'pos'.",
         call. = FALSE)
  })

  # Check if progress is a logical vector of length 1
  stopifnot("`progress` must be a logical vector of length 1." =
              assertthat::is.flag(progress))
  ### End checks

  # Perform jackknifing
  jackknife_df <- perform_jackknifing(
    data_cube = data_cube,
    fun = fun,
    ...,
    grouping_var = grouping_var,
    ref_group = ref_group,
    progress = progress)

  # Calculate original estimates
  t0 <- calc_stat_by_group(
    data_cube = data_cube,
    fun = fun,
    ...,
    grouping_var = grouping_var,
    ref_group = ref_group)

  # Calculate influence values
  influence_df <- jackknife_df %>%
    dplyr::left_join(t0, by = grouping_var) %>%
    dplyr::mutate(n = dplyr::n(),
                  .by = dplyr::all_of(grouping_var)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(influence = ifelse(
        influence_method == "usual",
        (.data$n - 1) * (.data$diversity_val - .data$jack_rep),
        (.data$n + 1) * (.data$jack_rep - .data$diversity_val)
      )
    ) %>%
    dplyr::ungroup()

  # Calculate acceleration
  out_df <- influence_df %>%
    dplyr::summarise(
      numerator = sum(.data$influence^3),
      denominator = 6 * sum(.data$influence^2)^1.5,
      acceleration = .data$numerator / .data$denominator,
      .by = dplyr::all_of(grouping_var)
    ) %>%
    dplyr::select(-c("numerator", "denominator")) %>%
    as.data.frame()

  return(out_df)
}
