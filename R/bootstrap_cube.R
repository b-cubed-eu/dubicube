# nolint start: line_length_linter.
#' Perform bootstrapping over a data cube for a calculated statistic
#'
#' This function generate `samples` bootstrap replicates of a statistic applied
#' to a data cube. It resamples the data cube and computes a statistic `fun` for
#' each bootstrap replicate, optionally comparing the results to a reference
#' group (`ref_group`).
#'
#' @param data_cube A data cube object (class 'processed_cube' or 'sim_cube',
#' see `b3gbi::process_cube()`) or a dataframe (cf. `$data` slot of
#' 'processed_cube' or 'sim_cube'). If `processed_cube = TRUE` (default), this
#' must be a processed or simulated data cube that contains a `$data` element.
#' @param fun A function which, when applied to `data_cube$data` returns the
#' statistic(s) of interest (or just `data_cube` in case of a dataframe).
#' This function must return a dataframe with a column `diversity_val`
#' containing the statistic of interest.
#' @param ... Additional arguments passed on to `fun`.
#' @param grouping_var A character vector specifying the grouping variable(s)
#' for the bootstrap analysis. The function `fun(data_cube$data, ...)` should
#' return a row per group. The specified variables must not be redundant,
#' meaning they should not contain the same information (e.g., `"time_point"`
#' (1, 2, 3) and `"year"` (2000, 2001, 2002) should not be used together if
#' `"time_point"` is just an alternative encoding of `"year"`).
#' @param samples The number of bootstrap replicates. A single positive integer.
#' Default is 1000.
#' @param ref_group A string indicating the reference group to compare the
#' statistic with. Default is `NA`, meaning no reference group is used.
#' @param seed A positive numeric value setting the seed for random number
#' generation to ensure reproducibility. If `NA` (default), then `set.seed()`
#' is not called at all. If not `NA`, then the random number generator state is
#' reset (to the state before calling this function) upon exiting this function.
#' @param processed_cube Logical. If `TRUE` (default), the function expects
#' `data_cube` to be a data cube object with a `$data` slot. If `FALSE`, the
#' function expects `data_cube` to be a dataframe.
#' @param method A character string specifying the bootstrap method.
#' Options include:
#'   - `"whole_cube"`: Perform whole-cube bootstrap
#'   - `"group_specific"`: Perform group-specific bootstrap
#'   - `"smart"`: Let dubicube derive the bootstrap method (default)
#' @param progress Logical. Whether to show a progress bar. Set to `TRUE` to
#' display a progress bar, `FALSE` (default) to suppress it.
#'
#' @returns A dataframe containing the bootstrap results with the following
#' columns:
#'   - `sample`: Sample ID of the bootstrap replicate
#'   - `est_original`: The statistic based on the full dataset per group
#'   - `rep_boot`: The statistic based on a bootstrapped dataset (bootstrap
#'   replicate)
#'   - `est_boot`: The bootstrap estimate (mean of bootstrap replicates per
#'   group)
#'   - `se_boot`: The standard error of the bootstrap estimate (standard
#'   deviation of the bootstrap replicates per group)
#'   - `bias_boot`: The bias of the bootstrap estimate per group
#'   - `method_boot`: The bootstrap method used
#'
#' @details
#' Bootstrapping is a statistical technique used to estimate the distribution of
#' a statistic by resampling with replacement from the original data
#' (Davison & Hinkley, 1997; Efron & Tibshirani, 1994).
#' In the case of data cubes, each row is sampled with replacement.
#' Below are the common notations used in bootstrapping:
#'
#' 1. **Original Sample Data**: \eqn{\mathbf{X} = \{X_1, X_2, \ldots, X_n\}}
#'    - The initial set of data points. Here, \eqn{n} is the sample
#'    size. This corresponds to the number of cells in a data cube or the number
#'    of rows in tabular format.
#'
#' 2. **Statistic of Interest**: \eqn{\theta}
#'    - The parameter or statistic being estimated, such as the mean
#'    \eqn{\bar{X}}, variance \eqn{\sigma^2}, or a biodiversity indicator. Let
#'    \eqn{\hat{\theta}} denote the estimated value of \eqn{\theta} calculated
#'    from the complete dataset \eqn{\mathbf{X}}.
#'
#' 3. **Bootstrap Sample**: \eqn{\mathbf{X}^* = \{X_1^*, X_2^*, \ldots, X_n^*\}}
#'    - A sample of size \eqn{n} drawn with replacement from the original sample
#'    \eqn{\mathbf{X}}. Each \eqn{X_i^*} is drawn independently from
#'    \eqn{\mathbf{X}}.
#'    - A total of \eqn{B} bootstrap samples are drawn from the original data.
#'    Common choices for \eqn{B} are 1000 or 10,000 to ensure a good
#'    approximation of the distribution of the bootstrap replications (see
#'    further).
#'
#' 4. **Bootstrap Replication**: \eqn{\hat{\theta}^*_b}
#'    - The value of the statistic of interest calculated from the \eqn{b}-th
#'    bootstrap sample \eqn{\mathbf{X}^*_b}. For example, if \eqn{\theta} is
#'    the sample mean, \eqn{\hat{\theta}^*_b = \bar{X}^*_b}.
#'
#' 5. **Bootstrap Statistics**:
#'
#' - **Bootstrap Estimate of the Statistic**: \eqn{\hat{\theta}_{\text{boot}}}
#'   - The average of the bootstrap replications:
#'
#' \deqn{\hat{\theta}_{\text{boot}} = \frac{1}{B} \sum_{b=1}^B \hat{\theta}^*_b}
#'
#' - **Bootstrap Bias**: \eqn{\text{Bias}_{\text{boot}}}
#'    - This bias indicates how much the bootstrap estimate deviates from the
#'    original sample estimate. It is calculated as the difference between the
#'    average bootstrap estimate and the original estimate:
#'
#' \deqn{\text{Bias}_{\text{boot}} = \frac{1}{B} \sum_{b=1}^B (\hat{\theta}^*_b - \hat{\theta}) = \hat{\theta}_{\text{boot}} - \hat{\theta}}
#'
#' - **Bootstrap Standard Error**: \eqn{\text{SE}_{\text{boot}}}
#'    - The standard deviation of the bootstrap replications, which estimates
#'    the variability of the statistic.
#'
#' There are two methods for bootstrapping:
#' - Whole-cube bootstrapping: resampling all rows in the cube, regardless of
#'   grouping. For indicators that are use data across groups.
#' - Group-specific bootstrapping: resampling rows only within a group of
#'   interest (e.g., a species, year, or habitat). For indicators that are
#'   calculated independently per group.
#'
#' The default smart option (`method = "smart"`) will decide on the bootstrap
#' method by calculating the statistic on a larger and smaller subset of the
#' data (containing respectively more and less groups in `grouping_var`). If
#' the indicator values for the common groups are identical,
#' `method = "group_specific"`, otherwise `method = "whole_cube"`.
#'
#' @references
#' Davison, A. C., & Hinkley, D. V. (1997). Bootstrap Methods and their
#' Application (1st ed.). Cambridge University Press.
#' \doi{10.1017/CBO9780511802843}
#'
#' Efron, B., & Tibshirani, R. J. (1994). An Introduction to the Bootstrap
#' (1st ed.). Chapman and Hall/CRC. \doi{10.1201/9780429246593}
#'
#' @export
#'
#' @family indicator_uncertainty
#'
#' @import dplyr
#' @import assertthat
#' @importFrom rlang .data
#' @importFrom modelr bootstrap
#' @importFrom purrr map
#' @importFrom stats sd
#'
#' @examples
#' \dontrun{
#' # After processing a data cube with b3gbi::process_cube()
#'
#' # Function to calculate statistic of interest
#' # Mean observations per year
#' mean_obs <- function(x) {
#'   out_df <- aggregate(obs ~ year, x, mean) # Calculate mean obs per year
#'   names(out_df) <- c("year", "diversity_val") # Rename columns
#'   return(out_df)
#' }
#' mean_obs(processed_cube$data)
#'
#' # Perform bootstrapping
#' bootstrap_mean_obs <- bootstrap_cube(
#'   data_cube = processed_cube,
#'   fun = mean_obs,
#'   grouping_var = "year",
#'   samples = 1000,
#'   seed = 123,
#'   progress = FALSE
#' )
#' head(bootstrap_mean_obs)
#' }
# nolint end

bootstrap_cube <- function(
    data_cube,
    fun,
    ...,
    grouping_var,
    samples = 1000,
    ref_group = NA,
    seed = NA,
    processed_cube = TRUE,
    method = "smart",
    progress = FALSE) {
  ### Start checks
  # Check data_cube input
  data_cube <- get_cube_data(
    data_cube = data_cube,
    processed_cube = processed_cube
  )

  # Check fun input
  stopifnot("`fun` must be a function." = is.function(fun))

  # Check if grouping_var is a character vector
  stopifnot("`grouping_var` must be a character vector." =
              is.character(grouping_var))

  # Check if method is specified correctly
  method <- tryCatch({
    match.arg(method, c("whole_cube", "group_specific", "smart"))
  }, error = function(e) {
    stop("`method` must be one of 'whole_cube', 'group_specific' or 'smart'.",
         call. = FALSE)
  })

  # Check if progress is a logical vector of length 1
  stopifnot("`progress` must be a logical vector of length 1." =
              assertthat::is.flag(progress))
  ### End checks

  # Get bootstrap method
  if (method == "smart") {
    method <- derive_bootstrap_method(
      df = data_cube,
      fun = fun,
      ...,
      cat_var = grouping_var
    )
  }

  # Perform bootstrapping
  if (method == "whole_cube") {
    print("Performing whole-cube bootstrap.")
    bootstrap_samples_df <- bootstrap_cube_raw(
      data_cube = data_cube,
      fun = fun,
      ...,
      grouping_var = grouping_var,
      samples = samples,
      ref_group = ref_group,
      seed = seed,
      processed_cube = processed_cube,
      progress = progress
    ) %>%
      mutate(method_boot = "whole_cube")
  } else if (method == "group_specific") {
    # Currenly not ok, see further for possible fix
    stopifnot(
      "`ref_group` cannot be used combined with group-specific bootstrapping." =
        is.na(ref_group)
    )

    # Make this in a for loop for progress = TRUE, combine dataframes with
    # refgroup if not NA
    print("Performing group-specific bootstrap.")
    bootstrap_samples_df <- bootstrap_samples_df %>%
      # will not work for multiple grouping var columns
      # bootstrap_samples_df %>% dplyr::select(dplyr::all_of(grouping_var))
      split(bootstrap_samples_df[[grouping_var]]) %>%
      lapply(function(cube) {
        bootstrap_cube_raw(
          data_cube = cube,
          fun = fun,
          ...,
          grouping_var = grouping_var,
          samples = samples,
          ref_group = ref_group,
          seed = seed,
          processed_cube = processed_cube,
          progress = progress
        )
      }) %>%
      bind_rows() %>%
      mutate(method_boot = "group_specific")
  } else {
    stop("Boot functionality not implemented yet.")
  }

  return(bootstrap_samples_df)
}
