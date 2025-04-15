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
#' @param grouping_var A character vector specifying the grouping variable(s)
#' for the bootstrap analysis. The function `fun(data_cube, ...)` should return
#' a row per group. The specified variables must not be redundant, meaning they
#' should not contain the same information (e.g., `"time_point"` (1, 2, 3) and
#' `"year"` (2000, 2001, 2002) should not be used together if `"time_point"` is
#' just an alternative encoding of `"year"`).
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
#' @param influence_method A string specifying the method used for calculating
#' the influence values.
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
#'    See `calculate_acceleration()` on how this is calculated.
#'
#'    - \eqn{a=0}: The statistic's variability does not depend on the data
#'    (e.g., symmetric distribution)
#'    - \eqn{a>0}: Small changes in the data have a large effect on the
#'    statistic's variability (e.g., positive skew)
#'    - \eqn{a<0}: Small changes in the data have a smaller effect on the
#'    statistic's variability (e.g., negative skew).
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
#' @export
#'
#' @family indicator_uncertainty
#'
#' @import dplyr
#' @import boot
#' @import assertthat
#' @importFrom rlang .data inherits_any
#' @importFrom stats pnorm qnorm setNames
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
    h = function(t) t,
    hinv = function(t) t,
    no_bias = FALSE,
    aggregate = TRUE,
    data_cube = NA,
    fun = NA,
    ...,
    ref_group = NA,
    influence_method = ifelse(is.element("bca", type), "usual", NA),
    progress = FALSE) {
  ### Start checks
  # Arguments data_cube, fun, ref_group, influence_method, and progress
  # arguments are checked in the calculate_acceleration() function

  # Check dataframe input
  stopifnot("`bootstrap_samples_df` must be a dataframe." =
              inherits(bootstrap_samples_df, "data.frame"))

  # Check if grouping_var is a character vector
  stopifnot("`grouping_var` must be a character vector." =
              is.character(grouping_var))

  # Check if "rep_boot", "est_original" and grouping_var columns are present
  colname_message <- paste(
    "`bootstrap_samples_df` should contain columns: 'rep_boot', 'est_original'",
    "and `grouping_var`."
  )
  do.call(
    stopifnot,
    stats::setNames(
      list(
        all(c(grouping_var, "rep_boot", "est_original") %in%
              names(bootstrap_samples_df))
      ),
      colname_message
    )
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
  ### End checks

  # Adjust bias if required
  bootstrap_samples_df <- bootstrap_samples_df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      rep_boot = ifelse(
        no_bias,
        .data$rep_boot - .data$bias_boot,
        .data$rep_boot
      )
    ) %>%
    dplyr::ungroup()

  # Calculate intervals
  if (any(type == "all")) type <- c("perc", "bca", "norm", "basic")
  out_list <- vector(mode = "list", length = length(type))
  for (i in seq_along(type)) {
    t <- type[i]

    if (t == "perc") {
      # Calculate confidence limits per group
      intervals_list <- bootstrap_samples_df %>%
        split(bootstrap_samples_df %>%
                dplyr::select(dplyr::all_of(grouping_var))) %>%
        lapply(function(df) {
          # Get group
          group <- df %>%
            dplyr::distinct(!!!dplyr::syms(grouping_var))

          # Calculate interval
          replicates <- df$rep_boot
          qq <- boot:::perc.ci(t = h(replicates), conf = conf, hinv = hinv)

          # Return interval limits
          qq_matrix <- matrix(qq[4:5], ncol = 2L)
          colnames(qq_matrix) <- c("ll", "ul")

          return(cbind(group, conf, qq_matrix))
        })

      # Combine confidence levels in dataframe
      intervals_df <- do.call(rbind.data.frame, intervals_list)

      # Join with input data
      conf_df <- bootstrap_samples_df %>%
        dplyr::mutate(int_type = t) %>%
        dplyr::left_join(intervals_df, by = grouping_var)
    }
    if (t == "bca") {
      # Check whether data_cube and fun are provided
      stopifnot(
        "`data_cube` and `fun` must be provided to calculate BCa interval." =
          rlang::inherits_any(
            data_cube,
            c("processed_cube", "sim_cube", "data.frame")
          ) &
          is.function(fun)
      )

      # Calculate acceleration values per grouping_var
      acceleration_df <- calculate_acceleration(
        data_cube = data_cube,
        fun = fun,
        ...,
        grouping_var = grouping_var,
        ref_group = ref_group,
        influence_method = influence_method,
        progress = progress
      )

      # Calculate confidence limits per group
      intervals_list <- bootstrap_samples_df %>%
        dplyr::left_join(acceleration_df,
                         by = grouping_var) %>%
        split(bootstrap_samples_df %>%
                dplyr::select(dplyr::all_of(grouping_var))) %>%
        lapply(function(df) {
          # Get group
          group <- df %>%
            dplyr::distinct(!!!dplyr::syms(grouping_var))

          # Get the original statistic and bootstrap replicates
          t0 <- unique(df$est_original)
          t <- df$rep_boot

          # Get the acceleration
          a <- unique(df$acceleration)
          if (!is.finite(a)) {
            warning("Estimated adjustment 'a' is NA.")
            return(cbind(group, conf, ll = NA, ul = NA))
          }

          # Calculate the BCa critical values
          alpha <- (1 + c(-conf, conf)) / 2
          zalpha <- stats::qnorm(alpha)

          z0 <- stats::qnorm(sum(t < t0) / length(t))
          if (!is.finite(z0)) {
            warning("Estimated adjustment 'z0' is infinite.")
            return(cbind(group, conf, ll = NA, ul = NA))
          }

          # Adjust for acceleration
          adj_alpha <- stats::pnorm(z0 + (z0 + zalpha) /
                                      (1 - a * (z0 + zalpha)))
          qq <- boot:::norm.inter(t, adj_alpha)
          qq_matrix <- matrix(hinv(h(qq[, 2L])), ncol = 2L)
          colnames(qq_matrix) <- c("ll", "ul")

          return(cbind(group, conf, qq_matrix))
        })

      # Combine confidence levels in dataframe
      intervals_df <- do.call(rbind.data.frame, intervals_list)

      # Join with input data
      conf_df <- bootstrap_samples_df %>%
        dplyr::mutate(int_type = t) %>%
        dplyr::left_join(intervals_df, by = grouping_var)
    }
    if (t == "norm") {
      # Calculate confidence limits per group
      intervals_list <- bootstrap_samples_df %>%
        split(bootstrap_samples_df %>%
                dplyr::select(dplyr::all_of(grouping_var))) %>%
        lapply(function(df) {
          # Get group
          group <- df %>%
            dplyr::distinct(!!!dplyr::syms(grouping_var))

          # Calculate interval
          estimate <- unique(df$est_original)
          replicates <- df$rep_boot
          qq <- boot::norm.ci(t0 = estimate, t = replicates, conf = conf,
                              h = h, hinv = hinv)

          # Return interval limits
          qq_matrix <- matrix(qq[2:3], ncol = 2L)
          colnames(qq_matrix) <- c("ll", "ul")

          return(cbind(group, conf, qq_matrix))
        })

      # Combine confidence levels in dataframe
      intervals_df <- do.call(rbind.data.frame, intervals_list)

      # Join with input data
      conf_df <- bootstrap_samples_df %>%
        dplyr::mutate(int_type = t) %>%
        dplyr::left_join(intervals_df, by = grouping_var)
    }
    if (t == "basic") {
      # Calculate confidence limits per group
      intervals_list <- bootstrap_samples_df %>%
        split(bootstrap_samples_df %>%
                dplyr::select(dplyr::all_of(grouping_var))) %>%
        lapply(function(df) {
          # Get group
          group <- df %>%
            dplyr::distinct(!!!dplyr::syms(grouping_var))

          # Calculate interval
          estimate <- unique(df$est_original)
          replicates <- df$rep_boot
          qq <- boot:::basic.ci(t0 = h(estimate), t = h(replicates),
                                conf = conf, hinv = hinv)

          # Return interval limits
          qq_matrix <- matrix(qq[4:5], ncol = 2L)
          colnames(qq_matrix) <- c("ll", "ul")

          return(cbind(group, conf, qq_matrix))
        })

      # Combine confidence levels in dataframe
      intervals_df <- do.call(rbind.data.frame, intervals_list)

      # Join with input data
      conf_df <- bootstrap_samples_df %>%
        dplyr::mutate(int_type = t) %>%
        dplyr::left_join(intervals_df, by = grouping_var)
    }

    out_list[[i]] <- conf_df
  }

  # Combine dataframes from all interval types
  conf_df_full <- dplyr::bind_rows(out_list) %>%
    # Revert bias if required
    dplyr::rowwise() %>%
    dplyr::mutate(
      rep_boot = ifelse(
        no_bias,
        .data$rep_boot + .data$bias_boot,
        .data$rep_boot
      )
    ) %>%
    dplyr::ungroup()

  # Aggregate if requested
  if (aggregate) {
    conf_df_out <- conf_df_full %>%
      dplyr::select(-c("sample", "rep_boot")) %>%
      dplyr::distinct() %>%
      as.data.frame()
  } else {
    conf_df_out <- conf_df_full %>%
      as.data.frame()
  }

  return(conf_df_out)
}
