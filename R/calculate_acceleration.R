# nolint start: line_length_linter.
#' Calculate acceleration
#'
#' @details
#' Acceleration quantifies how sensitive the variability of the statistic is
#' to changes in the data.
#'
#' - \eqn{a=0}: The statistic's variability does not depend on the data
#' (e.g., symmetric distribution)
#' - \eqn{a>0}: Small changes in the data have a large effect on the
#' statistic's variability (e.g., positive skew)
#' - \eqn{a<0}: Small changes in the data have a smaller effect on the
#' statistic's variability (e.g., negative skew).
#'
#' The acceleration term is calculated as follows:
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

calculate_acceleration <- function(
    bootstrap_samples_df,
    data_cube,
    fun,
    ...,
    grouping_var,
    ref_group = NA,
    jackknife = "usual",
    progress = FALSE) {
  ### Start checks
  # Check dataframe input
  stopifnot("`bootstrap_samples_df` must be a dataframe." =
              inherits(bootstrap_samples_df, "data.frame"))

  # Check if grouping_var is a character vector
  stopifnot("`grouping_var` must be a character vector." =
              is.character(grouping_var))

  # Check if "est_original" and grouping_var columns are present
  colname_message <- paste(
    "`bootstrap_samples_df` should contain columns: 'est_original'",
    "and `grouping_var`.")
  do.call(stopifnot,
          stats::setNames(list(
            all(c(grouping_var, "est_original") %in%
                  names(bootstrap_samples_df))),
            colname_message)
  )

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

  # Check if jackknife is 'usual' or 'pos'
  jackknife <- tryCatch({
    match.arg(jackknife, c("usual", "pos"))
  }, error = function(e) {
    stop("`jackknife` must be one of 'usual', 'pos'.",
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

  # Calculate influence values
  influence_df <- jackknife_df %>%
    dplyr::left_join(bootstrap_samples_df %>%
                       dplyr::distinct(!!!dplyr::syms(grouping_var),
                                       .data$est_original),
                     by = grouping_var) %>%
    dplyr::mutate(n = dplyr::n(),
                  .by = dplyr::all_of(grouping_var)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(influence = ifelse(
      jackknife == "usual",
      (.data$n - 1) * (.data$est_original - .data$jack_rep),
      (.data$n + 1) * (.data$jack_rep - .data$est_original)
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
    )

  return(out_df)
}
