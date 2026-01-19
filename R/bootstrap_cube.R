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
#'   - `"smart"`: Automatically select the appropriate bootstrap method
#'     based on indicator behaviour and the presence of a reference group
#'     (default).
#'   - `"boot_whole_cube"`: Perform whole-cube bootstrap using
#'     [boot::boot()]. Cannot be used with `ref_group`.
#'   - `"boot_group_specific"`: Perform group-specific bootstrap using
#'     [boot::boot()]. Cannot be used with `ref_group`.
#'   - `"whole_cube"`: Perform whole-cube bootstrap without using the
#'     \pkg{boot} package. Can be used with `ref_group`.
#'   - `"group_specific"`: Perform group-specific bootstrap without using the
#'     \pkg{boot} package. Can be used with `ref_group`.
#' @param progress Logical. Whether to show a progress bar. Set to `TRUE` to
#' display a progress bar, `FALSE` (default) to suppress it.
#' @param boot_args Named list of additional arguments passed to `boot::boot()`.
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
#' If `method` resolves to `"boot_whole_cube"` or `"boot_group_specific"`,
#' the returned value is an object of class `"boot"`, as produced by
#' [boot::boot()].
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
#' The default smart option (`method = "smart"`) determines both
#' (i) whether the indicator is group-specific or whole-cube, and
#' (ii) whether the \pkg{boot} package should be used.
#'
#' The decision is made by calculating the statistic on larger and smaller
#' subsets of the data (containing respectively more and fewer groups in
#' `grouping_var`). If indicator values for the common groups are identical,
#' the indicator is treated as group-specific; otherwise, it is treated as
#' whole-cube.
#'
#' If no reference group is used (`ref_group = NA`), `method = "smart"`
#' resolves to `"boot_group_specific"` or `"boot_whole_cube"`, both of which
#' use [boot::boot()]. If a reference group is specified, `method = "smart"`
#' resolves to `"group_specific"` or `"whole_cube"` and bootstrapping is
#' handled internally.
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
#' @importFrom boot boot
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
  progress = FALSE,
  boot_args = list()
) {
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
    match.arg(
      method,
      c("smart", "boot_whole_cube", "boot_group_specific", "whole_cube",
        "group_specific")
    )
  }, error = function(e) {
    stop(
      paste("`method` must be one of 'smart', 'boot_whole_cube',",
            "'boot_group_specific', 'whole_cube', or 'group_specific'."),
      call. = FALSE
    )
  })

  # Check if progress is a logical vector of length 1
  stopifnot("`progress` must be a logical vector of length 1." =
              assertthat::is.flag(progress))

  # Check if boot_args is correct
  stopifnot("`boot_args` must be a named list." = is.list(boot_args))
  ### End checks

  # Get bootstrap method
  method <- resolve_bootstrap_method(
    df = data_cube,
    fun = fun,
    ...,
    cat_var = grouping_var,
    ref_group = ref_group,
    method = method
  )

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
      progress = progress
    ) %>%
      mutate(method_boot = "whole_cube")

  } else if (method == "group_specific") {

    stopifnot(
      "Group-specific bootstrapping requires exactly one grouping variable." =
        length(grouping_var) == 1
    )
    print("Performing group-specific bootstrap.")

    # Identify groups
    cube_grouped <- data_cube %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(grouping_var))) %>%
      dplyr::mutate(.grp_id = dplyr::cur_group_id()) %>%
      dplyr::ungroup()

    if (!is.na(ref_group)) {
      # Calculate group_var columns for matching
      matching_col <- grouping_var[
        sapply(data_cube %>% dplyr::select(dplyr::all_of(grouping_var)),
               function(col) ref_group %in% col)
      ]
      # Reference group should not be included
      cube_grouped <- cube_grouped %>%
        dplyr::filter(.data[[matching_col]] != !!ref_group)
    }

    n_cat <- max(cube_grouped$.grp_id)

    # Bootstrap for every group
    bootstrap_samples_list <- vector(mode = "list", length = n_cat)

    for (group in seq_len(n_cat)) {
      if (progress) print(paste("Bootstrapping group", group, "of", n_cat, "."))

      # Select relevant data
      group_data <- cube_grouped %>%
        dplyr::filter(.data$.grp_id == group) %>%
        dplyr::select(-".grp_id")

      if (!is.na(ref_group)) {
        # Calculate group_var columns for matching
        matching_col <- grouping_var[
          sapply(data_cube %>% dplyr::select(dplyr::all_of(grouping_var)),
                 function(col) ref_group %in% col)
        ]
        # Bind group data with reference group data
        group_data <- dplyr::bind_rows(
          group_data,
          dplyr::filter(data_cube, .data[[matching_col]] == !!ref_group)
        )
      }

      # Perform bootstraping
      group_bootstrap_samples <- bootstrap_cube_raw(
        data_cube = group_data,
        fun = fun,
        ...,
        grouping_var = grouping_var,
        samples = samples,
        ref_group = ref_group,
        seed = seed,
        progress = progress
      ) %>%
        dplyr::mutate(method_boot = "group_specific")

      bootstrap_samples_list[[group]] <- group_bootstrap_samples
    }

    # Combine results
    bootstrap_samples_df <- dplyr::bind_rows(bootstrap_samples_list) %>%
      dplyr::mutate(method_boot = "group_specific")

  } else {
    # Wrapper for boot::boot() to match expected output
    boot_stat_wrapper <- function(data, indices) {
      sampled_data <- data[indices, , drop = FALSE]
      fun(sampled_data, ...) %>% dplyr::pull("diversity_val")
    }

    # Set seed if provided
    if (!is.na(seed)) {
      if (exists(".Random.seed", envir = .GlobalEnv)) {
        rng_state_old <- get(".Random.seed", envir = .GlobalEnv)
        on.exit(assign(".Random.seed", rng_state_old, envir = .GlobalEnv)) # nolint: object_name_linter
      }
      set.seed(seed)
    }

    if (method == "boot_whole_cube") {

      # Call boot::boot() with user-specified arguments
      boot_res <- do.call(
        boot::boot,
        c(
          list(
            data = data_cube,
            statistic = boot_stat_wrapper,
            R = samples
          ),
          boot_args
        )
      )

      # Return boot object
      return(boot_res)

    }

    if (method == "boot_group_specific") {

      stopifnot(
        "boot_group_specific requires exactly one grouping variable." =
          length(grouping_var) == 1
      )

      # Split data per group
      cube_split <- split(
        data_cube,
        data_cube[[grouping_var]]
      )

      # Call boot::boot() with user-specified arguments per group
      boot_list <- lapply(cube_split, function(df) {
        do.call(
          boot::boot,
          c(
            list(
              data = df,
              statistic = boot_stat_wrapper,
              R = samples
            ),
            boot_args
          )
        )
      })

      return(boot_list)
    }
  }

  return(bootstrap_samples_df)
}
