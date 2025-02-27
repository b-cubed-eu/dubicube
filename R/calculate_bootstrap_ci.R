# nolint start: line_length_linter.
#' Calculate confidence intervals for a dataframe with bootstrap replicates
#'
#' This function calculates confidence intervals for a dataframe containing
#' bootstrap replicates based on different methods, including percentile
#' (`perc`), bias-corrected and accelerated (`bca`), normal (`norm`), and basic
#' (`basic`).
#'
#' @param bootstrap_samples_df A dataframe containing the bootstrap replicates,
#' where each row represents a bootstrap sample. As returned by
#' `bootstrap_cube()`. Apart from the `grouping_var` column, the following
#' columns should be present:
#'   - `est_original`: The statistic based on the full dataset per group
#'   - `rep_boot`: The statistic based on a bootstrapped dataset (bootstrap
#'   replicate)
#' @param grouping_var A string specifying the grouping variable(s) used for the
#' bootstrap analysis.
#' This variable is used to split the dataset into groups for separate
#' confidence interval calculations.
#' @param type A character vector specifying the type(s) of confidence intervals
#' to compute. Options include:
#'   - `"perc"`: Percentile interval
#'   - `"bca"`: Bias-corrected and accelerated interval
#'   - `"norm"`: Normal interval
#'   - `"basic"`: Basic interval
#'   - `"all"`: Compute all available interval types (default)
#' @param conf A numeric value specifying the confidence level of the intervals.
#' Default is `0.95` (95 % confidence level).
#' @param aggregate Logical. If `TRUE` (default), the function returns distinct
#' confidence limits per group. If `FALSE`, the confidence limits are added to
#' the original bootstrap dataframe `bootstrap_samples_df`.
#' @param data_cube Only used when `type = "bca"`. A data cube object (class
#' 'processed_cube' or 'sim_cube', see `b3gbi::process_cube()`) or a dataframe
#' (from `$data` slot of 'processed_cube' or 'sim_cube'). As used by
#' `bootstrap_cube()`. To limit runtime, we recommend using a
#' dataframe with custom function as `fun`.
#' @param fun Only used when `type = "bca"`. A function which, when applied to
#' `data_cube` returns the statistic(s) of interest. This function must return a
#' dataframe with a column `diversity_val` containing the statistic of interest.
#'  As used by `bootstrap_cube()`.
#' @param ... Additional arguments passed on to `fun`.
#' @param ref_group Only used when `type = "bca"`. A string indicating the
#' reference group to compare the statistic with. Default is `NA`, meaning no
#' reference group is used.
#' As used by `bootstrap_cube()`.
#' @param jackknife Only used when `type = "bca"`. A string specifying the
#' jackknife resampling method for BCa intervals.
#'   - `"usual"`: Negative jackknife (default if BCa is selected).
#'   - `"pos"`: Positive jackknife
#' @param progress Logical. Whether to show a progress bar for jackknifing. Set
#' to `TRUE` to display a progress bar, `FALSE` (default) to suppress it.
#'
#' @returns A dataframe containing the bootstrap results with the following
#' columns:
#'   - `est_original`: The statistic based on the full dataset per group
#'   - rep_boo
#'   - `est_boot`: The bootstrap estimate (mean of bootstrap replicates per
#'   group)
#'   - `se_boot`: The standard error of the bootstrap estimate (standard
#'   deviation of the bootstrap replicates per group)
#'   - `bias_boot`: The bias of the bootstrap estimate per group
#'   - `int_type`: The interval type
#'   - `ll`: The lower limit of the confidence interval
#'   - `ul`: The upper limit of the confidence interval
#'   - `conf`: The confidence level of the interval
#' When `aggregate = FALSE`, the dataframe contains the columns from
#' `bootstrap_samples_df` with one row per bootstrap replicate.
#'
#' @details
#' We consider four different types of intervals (with confidence level
#' \eqn{\alpha}). The choice for confidence interval types and their calculation
#' is in line with the \pkg{boot} package in R (Canty & Ripley, 1999) to ensure
#' ease of implementation. They are based on the definitions provided by
#' Davison & Hinkley (1997, Chapter 5)
#' (see also DiCiccio & Efron, 1996; Efron, 1987).
#'
#' 1. **Percentile**: Uses the percentiles of the bootstrap distribution.
#'
#'    \deqn{CI_{perc} = \left[ \hat{\theta}^*_{(\alpha/2)}, \hat{\theta}^*_{(1-\alpha/2)} \right]}
#'
#'    where \eqn{\hat{\theta}^*_{(\alpha/2)}} and
#'    \eqn{\hat{\theta}^*_{(1-\alpha/2)}} are the \eqn{\alpha/2} and
#'    \eqn{1-\alpha/2} percentiles of the bootstrap distribution, respectively.
#'
#' 2. **Bias-Corrected and Accelerated (BCa)**: Adjusts for bias and
#' acceleration
#'
#'    Bias refers to the systematic difference between the observed statistic
#'    from the original dataset and the center of the bootstrap distribution of
#'    the statistic. The bias correction term is calculated as follows:
#'
#'    \deqn{\hat{z}_0 = \Phi^{-1}\left(\frac{\#(\hat{\theta}^*_b < \hat{\theta})}{B}\right)}
#'
#'    where \eqn{\#} is the counting operator and \eqn{\Phi^{-1}} the inverse
#'    cumulative density function of the standard normal distribution.
#'
#'    Acceleration quantifies how sensitive the variability of the statistic is
#'    to changes in the data.
#'
#'    - \eqn{a=0}: The statistic's variability does not depend on the data
#'    (e.g., symmetric distribution)
#'    - \eqn{a>0}: Small changes in the data have a large effect on the
#'    statistic's variability (e.g., positive skew)
#'    - \eqn{a<0}: Small changes in the data have a smaller effect on the
#'    statistic's variability (e.g., negative skew).
#'
#'    The acceleration term is calculated as follows:
#'
#'    \deqn{\hat{a} = \frac{1}{6} \frac{\sum_{i = 1}^{n}(I_i^3)}{\left( \sum_{i = 1}^{n}(I_i^2) \right)^{3/2}}}
#'
#'    where \eqn{I_i} denotes the influence of data point \eqn{x_i} on the
#'    estimation of \eqn{\theta}. \eqn{I_i} can be estimated using jackknifing.
#'    Examples are (1) the negative jackknife:
#'    \eqn{I_i = (n-1)(\hat{\theta} - \hat{\theta}_{-i})}, and (2) the positive
#'    jackknife \eqn{I_i = (n+1)(\hat{\theta}_{-i} - \hat{\theta})}
#'    (Frangos & Schucany, 1990). Here, \eqn{\hat{\theta}_{-i}} is the estimated
#'    value leaving out the \eqn{i}’th data point \eqn{x_i}. The \pkg{boot}
#'    package also offers infinitesimal jackknife and regression estimation.
#'    Implementation of these jackknife algorithms can be explored in the
#'    future.
#'
#'    The bias and acceleration estimates are then used to calculate adjusted
#'    percentiles.
#'
#'    \eqn{\alpha_1 = \Phi\left( \hat{z}_0 + \frac{\hat{z}_0 + z_{\alpha/2}}{1 - \hat{a}(\hat{z}_0 + z_{\alpha/2})} \right)},
#'    \eqn{\alpha_2 = \Phi\left( \hat{z}_0 + \frac{\hat{z}_0 + z_{1 - \alpha/2}}{1 - \hat{a}(\hat{z}_0 + z_{1 - \alpha/2})} \right)}
#'
#'    So, we get
#'
#'    \deqn{CI_{bca} = \left[ \hat{\theta}^*_{(\alpha_1)}, \hat{\theta}^*_{(\alpha_2)} \right]}
#'
#' 3. **Normal**: Assumes the bootstrap distribution of the statistic is
#' approximately normal
#'
#'    \deqn{CI_{norm} = \left[\hat{\theta} - \text{Bias}_{\text{boot}} - \text{SE}_{\text{boot}} \times z_{1-\alpha/2},
#'    \hat{\theta} - \text{Bias}_{\text{boot}} + \text{SE}_{\text{boot}} \times z_{1-\alpha/2} \right]}
#'
#'    where \eqn{z_{1-\alpha/2}} is the \eqn{1-\alpha/2} quantile of the
#'    standard normal distribution.
#'
#' 4. **Basic**: Centers the interval using percentiles
#'
#'    \deqn{CI_{basic} = \left[ 2\hat{\theta} - \hat{\theta}^*_{(1-\alpha/2)},
#'    2\hat{\theta} - \hat{\theta}^*_{(\alpha/2)} \right]}
#'
#'    where \eqn{\hat{\theta}^*_{(\alpha/2)}} and
#'    \eqn{\hat{\theta}^*_{(1-\alpha/2)}} are the \eqn{\alpha/2} and
#'    \eqn{1-\alpha/2} percentiles of the bootstrap distribution, respectively.
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
#' DiCiccio, T. J., & Efron, B. (1996). Bootstrap confidence intervals.
#' Statistical Science, 11(3). \doi{10.1214/ss/1032280214}
#'
#' Efron, B. (1987). Better Bootstrap Confidence Intervals. Journal of the
#' American Statistical Association, 82(397), 171–185.
#' \doi{10.1080/01621459.1987.10478410}
#'
#' Efron, B., & Tibshirani, R. J. (1994). An Introduction to the Bootstrap
#' (1st ed.). Chapman and Hall/CRC. \doi{10.1201/9780429246593}
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
#' @import boot
#' @import assertthat
#' @importFrom rlang .data
#' @importFrom stats pnorm qnorm
#'
#' @examples
#' # Get example data
#' # install.packages("remotes")
#' # remotes::install_github("b-cubed-eu/b3gbi")
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
#' # Calculate confidence limits
#' # Percentile interval
#' ci_mean_obs1 <- calculate_bootstrap_ci(
#'   bootstrap_samples_df = bootstrap_mean_obs,
#'   grouping_var = "year",
#'   type = "perc",
#'   conf = 0.95,
#'   aggregate = TRUE)
#' ci_mean_obs1
#'
#' # All intervals
#' ci_mean_obs2 <- calculate_bootstrap_ci(
#'   bootstrap_samples_df = bootstrap_mean_obs,
#'   grouping_var = "year",
#'   type = c("perc", "bca", "norm", "basic"),
#'   conf = 0.95,
#'   aggregate = TRUE,
#'   data_cube = denmark_cube$data, # Required for BCa
#'   fun = mean_obs,                # Required for BCa
#'   progress = FALSE)
#' ci_mean_obs2
#' }
# nolint end

calculate_bootstrap_ci <- function(
    bootstrap_samples_df,
    grouping_var,
    type = c("perc", "bca", "norm", "basic"),
    conf = 0.95,
    aggregate = TRUE,
    data_cube = NA,
    fun = NA,
    ...,
    ref_group = NA,
    jackknife = ifelse(is.element("bca", type), "usual", NA),
    progress = FALSE) {
  ### Start checks
  # Arguments data_cube, fun, ref_group, and jackknife arguments are checked
  # further on in code

  # Check dataframe input
  stopifnot("`bootstrap_samples_df` must be a dataframe." =
              inherits(bootstrap_samples_df, "data.frame"))

  # Check if grouping_var is a character vector of length 1
  stopifnot("`grouping_var` must be a character vector of length 1." =
              assertthat::is.string(grouping_var))

  # Check if "rep_boot", "est_original" and grouping_var columns are present
  colname_message <- paste(
    "`bootstrap_samples_df` should contain columns: 'rep_boot', 'est_original'",
    "and `grouping_var`.")
  do.call(stopifnot,
          stats::setNames(list(
            all(c(grouping_var, "rep_boot", "est_original") %in%
                  names(bootstrap_samples_df))),
            colname_message)
  )

  # Check if interval type is correct
  stopifnot("`type` must be one of 'perc', 'bca', 'norm', 'basic'." =
              all(is.element(type, c("perc", "bca", "norm", "basic", "all"))))

  # conf should be numeric between 0 and 1
  stopifnot("`conf` must be a numeric value between 0 and 1." =
              assertthat::is.number(conf) &
              (conf > 0 & conf < 1))

  # Check if aggregate is a logical vector of length 1
  stopifnot("`aggregate` must be a logical vector of length 1." =
              assertthat::is.flag(aggregate))

  # Check if progress is a logical vector of length 1
  stopifnot("`progress` must be a logical vector of length 1." =
              assertthat::is.flag(progress))
  ### End checks

  # Calculate intervals
  if (any(type == "all")) type <- c("perc", "bca", "norm", "basic")
  out_list <- vector(mode = "list", length = length(type))
  for (i in seq_along(type)) {
    t <- type[i]

    if (t == "perc") {
      # Calculate confidence limits per group
      intervals_list <- bootstrap_samples_df %>%
        split(bootstrap_samples_df[[grouping_var]]) %>%
        lapply(function(df) {
          replicates <- df$rep_boot
          boot:::perc.ci(t = replicates, conf = conf)
        })

      # Combine confidence levels in dataframe
      intervals_df <- do.call(rbind.data.frame, intervals_list) %>%
        dplyr::mutate(group = unique(bootstrap_samples_df[[grouping_var]])) %>%
        dplyr::rename("ll" = "V4", "ul" = "V5") %>%
        dplyr::select("group", "ll", "ul", "conf")

      # Join with input data
      conf_df <- bootstrap_samples_df %>%
        dplyr::mutate(int_type = t) %>%
        dplyr::left_join(intervals_df,
                         by = dplyr::join_by(!!grouping_var == "group"))
    }
    if (t == "bca") {
      # Check whether data_cube and fun are provided
      stopifnot(
        "`data_cube` and `fun` must be provided to calculate BCa interval." =
          assertthat::noNA(data_cube) &
          suppressWarnings(assertthat::noNA(fun)))

      # Check data_cube input
      cube_message <- paste("`data_cube` must be a data cube object (class",
                            "'processed_cube' or 'sim_cube') or a dataframe.")
      do.call(stopifnot,
              stats::setNames(list(
                rlang::inherits_any(
                  data_cube,
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

      jackknife_df <- perform_jackknifing(
        data_cube = data_cube,
        fun = fun,
        ...,
        grouping_var = grouping_var,
        ref_group = ref_group,
        progress = progress)

      acceleration_df <- jackknife_df %>%
        dplyr::left_join(bootstrap_samples_df %>%
                           dplyr::distinct(!!dplyr::sym(grouping_var),
                                           .data$est_original),
                         by = dplyr::join_by(!!grouping_var)) %>%
        dplyr::mutate(n = dplyr::n(),
                      .by = dplyr::all_of(grouping_var)) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(intensity = ifelse(
          jackknife == "usual",
          (.data$n - 1) * (.data$est_original - .data$jack_rep),
          (.data$n + 1) * (.data$jack_rep - .data$est_original)
        )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::summarise(
          numerator = sum(.data$intensity^3),
          denominator = 6 * sum(.data$intensity^2)^1.5,
          acceleration = .data$numerator / .data$denominator,
          .by = dplyr::all_of(grouping_var)
        )

      # Calculate confidence limits per group
      intervals_list <- bootstrap_samples_df %>%
        dplyr::left_join(acceleration_df,
                         by = dplyr::join_by(!!grouping_var)) %>%
        split(bootstrap_samples_df[[grouping_var]]) %>%
        lapply(function(df) {
          # Get the original statistic and bootstrap replicates
          t0 <- unique(df$est_original)
          t <- df$rep_boot

          # Get the acceleration
          a <- unique(df$acceleration)
          if (!is.finite(a)) {
            warning("Estimated adjustment 'a' is NA.")
            return(cbind(conf, NA, NA))
          }

          # Calculate the BCa critical values
          alpha <- (1 + c(-conf, conf)) / 2
          zalpha <- stats::qnorm(alpha)

          z0 <- stats::qnorm(sum(t < t0) / length(t))
          if (!is.finite(z0)) {
            warning("Estimated adjustment 'z0' is infinite.")
            return(cbind(conf, NA, NA))
          }

          # Adjust for acceleration
          adj_alpha <- stats::pnorm(z0 + (z0 + zalpha) /
                                      (1 - a * (z0 + zalpha)))
          qq <- boot:::norm.inter(t, adj_alpha)

          return(cbind(conf, matrix(qq[, 2L], ncol = 2L)))
        })

      # Combine confidence levels in dataframe
      intervals_df <- do.call(rbind.data.frame, intervals_list) %>%
        dplyr::mutate(group = unique(bootstrap_samples_df[[grouping_var]])) %>%
        dplyr::rename("ll" = "V2", "ul" = "V3") %>%
        dplyr::select("group", "ll", "ul", "conf")

      # Join with input data
      conf_df <- bootstrap_samples_df %>%
        dplyr::mutate(int_type = t) %>%
        dplyr::left_join(intervals_df,
                         by = dplyr::join_by(!!grouping_var == "group"))
    }
    if (t == "norm") {
      # Calculate confidence limits per group
      intervals_list <- bootstrap_samples_df %>%
        split(bootstrap_samples_df[[grouping_var]]) %>%
        lapply(function(df) {
          estimate <- unique(df$est_original)
          replicates <- df$rep_boot
          boot::norm.ci(t0 = estimate, t = replicates, conf = conf)
        })

      # Combine confidence levels in dataframe
      intervals_df <- do.call(rbind.data.frame, intervals_list) %>%
        dplyr::mutate(group = unique(bootstrap_samples_df[[grouping_var]])) %>%
        dplyr::rename("ll" = "V2", "ul" = "V3") %>%
        dplyr::select("group", "ll", "ul", "conf")

      # Join with input data
      conf_df <- bootstrap_samples_df %>%
        dplyr::mutate(int_type = t) %>%
        dplyr::left_join(intervals_df,
                         by = dplyr::join_by(!!grouping_var == "group"))
    }
    if (t == "basic") {
      # Calculate confidence limits per group
      intervals_list <- bootstrap_samples_df %>%
        split(bootstrap_samples_df[[grouping_var]]) %>%
        lapply(function(df) {
          estimate <- unique(df$est_original)
          replicates <- df$rep_boot
          boot:::basic.ci(t0 = estimate, t = replicates, conf = conf)
        })

      # Combine confidence levels in dataframe
      intervals_df <- do.call(rbind.data.frame, intervals_list) %>%
        dplyr::mutate(group = unique(bootstrap_samples_df[[grouping_var]])) %>%
        dplyr::rename("ll" = "V4", "ul" = "V5") %>%
        dplyr::select("group", "ll", "ul", "conf")

      # Join with input data
      conf_df <- bootstrap_samples_df %>%
        dplyr::mutate(int_type = t) %>%
        dplyr::left_join(intervals_df,
                         by = dplyr::join_by(!!grouping_var == "group"))
    }

    out_list[[i]] <- conf_df
  }

  # Combine dataframes from all interval types
  conf_df_full <- dplyr::bind_rows(out_list)

  # Aggregate if requested
  if (aggregate) {
    conf_df_out <- conf_df_full %>%
      dplyr::select(-c("sample", "rep_boot")) %>%
      dplyr::distinct()
  } else {
    conf_df_out <- conf_df_full
  }

  return(conf_df_out)
}
