#' Perform bootstrapping over a data cube for a calculated statistic
#'
#' This function generate `samples` bootstrap replicates of a statistic applied
#' to a data cube. It resamples the data cube and computes a statistic `fun` for
#' each bootstrap replicate, optionally comparing the results to a reference
#' group (`ref_group`).
#'
#' @param data_cube A data cube object (class 'processed_cube' or 'sim_cube',
#' see `b3gbi::process_cube()`) or a dataframe (from `$data` slot of
#' 'processed_cube' or 'sim_cube').
#' @param fun A function which, when applied to `data_cube` returns the
#' statistic(s) of interest. This function must return a dataframe with a column
#' `diversity_val` containing the statistic of interest.
#' @param grouping_var A string specifying the grouping variable(s) for the
#' bootstrap analysis. The output of `fun(data_cube)` returns a row per group.
#' @param samples The number of bootstrap replicates. A single positive integer.
#' Default is 1000.
#' @param ref_group A string indicating the reference group to compare the
#' statistic with. Default is `NA`, meaning no reference group is used.
#' @param seed A positive numeric value setting the seed for random number
#' generation to ensure reproducibility. If `NA` (default), then `set.seed()`
#' is not called at all. If not `NA`, then the random number generator state is
#' reset (to the state before calling this function) upon exiting this function.
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
#'
#' @details
#' Bootstrapping is a statistical technique used to estimate the distribution of
#' a statistic by resampling with replacement from the original data
#' (Davison & Hinkley, 1997; Efron & Tibshirani, 1994).
#' In the case of data cubes, each row is sampled with replacement.
#' Below are the common notations used in bootstrapping:
#'
#' 1. **Original Sample Data**: \eqn{\mathbf{X} = \{X_1, X_2, \ldots, X_n\}}
#'    - The initial set of observed data points. Here, \eqn{n} is the sample
#'    size. This corresponds to the number of cells in a data cube or the number
#'    of rows in tabular format.
#'
#' 2. **Statistic of Interest**: \eqn{\theta}
#'    - The parameter or statistic being estimated, such as the mean
#'    \eqn{\bar{X}}, variance \eqn{\sigma^2}, or a biodiversity indicator. Let
#'    \eqn{\hat{\theta}}
#'    denote the estimated value of \eqn{\theta}
#'    calculated from the complete dataset \eqn{\mathbf{X}}.
#'
#' 3. **Bootstrap Sample**: \eqn{\mathbf{X}^* = \{X_1^*, X_2^*, \ldots, X_n^*\}}
#'    - A sample of size \eqn{n}
#'    drawn with replacement from the original sample
#'    \eqn{\mathbf{X}}. Each \eqn{X_i^*} is drawn independently from
#'    \eqn{\mathbf{X}}.
#'    - A total of \eqn{B}
#'    bootstrap samples are drawn from the original data.
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
#' \deqn{\text{Bias}_{\text{boot}} = \frac{1}{B} \sum_{b=1}^B (\hat{\theta}^*_b
#' - \hat{\theta}) = \hat{\theta}_{\text{boot}} - \hat{\theta}}
#'
#' - **Bootstrap Standard Error**: \eqn{\text{SE}_{\text{boot}}}
#'    - The standard deviation of the bootstrap replications, which estimates
#'    the variability of the statistic.
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
#' @import dplyr
#' @import assertthat
#' @importFrom rlang .data inherits_any
#' @importFrom modelr bootstrap
#' @importFrom purrr map
#' @importFrom stats sd setNames
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
#' }

bootstrap_cube <- function(
    data_cube,
    fun,
    grouping_var,
    samples = 1000,
    ref_group = NA,
    seed = NA,
    progress = FALSE) {
  ### Start checks
  # Check data_cube input
  cube_message <- paste("`data_cube` must be a data cube object (class",
                        "'processed_cube' or 'sim_cube') or a dataframe.")
  do.call(stopifnot,
          stats::setNames(list(
            rlang::inherits_any(data_cube,
                                c("processed_cube", "sim_cube", "data.frame"))),
            cube_message)
          )

  # Check if grouping_var is a character vector of length 1
  stopifnot("`grouping_var` must be a character vector of length 1." =
              assertthat::is.string(grouping_var))

  # Check if samples is a positive integer
  stopifnot(
    "`samples` must be a single positive integer." =
      assertthat::is.count(samples))

  # Check if ref_group is NA or a number or a string
  stopifnot(
    "`ref_group` must be a numeric/character vector of length 1 or NA." =
      (assertthat::is.number(ref_group) | assertthat::is.string(ref_group) |
         is.na(ref_group)) &
      length(ref_group) == 1)

  # Check if seed is NA or a number
  stopifnot("`seed` must be a numeric vector of length 1 or NA." =
              (is.numeric(seed) | is.na(seed)) & length(seed) == 1)

  # Check if progress is a logical vector of length 1
  stopifnot("`progress` must be a logical vector of length 1." =
              assertthat::is.flag(progress))
  ### End checks

  # Set seed if provided
  if (!is.na(seed)) {
    if (exists(".Random.seed", envir = .GlobalEnv)) {
      rng_state_old <- get(".Random.seed", envir = .GlobalEnv)
      on.exit(assign(".Random.seed", rng_state_old, envir = .GlobalEnv))
    }
    set.seed(seed)
  }

  if (rlang::inherits_any(data_cube, c("processed_cube", "sim_cube"))) {
    # Generate bootstrap replicates
    resample_df <- modelr::bootstrap(data_cube$data, samples, id = "id")

    # Function for bootstrapping
    bootstrap_resample <- function(x, fun) {
      resample_obj <- x$strap[[1]]
      indices <- as.integer(resample_obj)
      data <- resample_obj$data[indices, ]

      data_cube_copy <- data_cube
      data_cube_copy$data <- data

      fun(data_cube_copy)$data %>%
        dplyr::mutate(sample = as.integer(x$id))
    }
  } else {
    # Generate bootstrap replicates
    resample_df <- modelr::bootstrap(data_cube, samples, id = "id")

    # Function for bootstrapping
    bootstrap_resample <- function(x, fun) {
      resample_obj <- x$strap[[1]]
      indices <- as.integer(resample_obj)
      data <- resample_obj$data[indices, ]

      fun(data) %>%
        dplyr::mutate(sample = as.integer(x$id))
    }
  }

  # Perform bootstrapping
  bootstrap_samples_list_raw <- resample_df %>%
    split(seq_len(nrow(resample_df))) %>%
    purrr::map(
      bootstrap_resample,
      fun = fun,
      .progress = ifelse(progress, "Bootstrapping", progress))

  if (!is.na(ref_group)) {
    # Calculate true statistic
    if (rlang::inherits_any(data_cube, c("processed_cube", "sim_cube"))) {
      t0_full <- fun(data_cube)$data
    } else {
      t0_full <- fun(data_cube)
    }

    ref_val <- t0_full %>%
      dplyr::filter(.data[[grouping_var]] == !!ref_group) %>%
      dplyr::pull(.data$diversity_val)

    t0 <- t0_full %>%
      dplyr::filter(.data[[grouping_var]] != !!ref_group) %>%
      dplyr::mutate(diversity_val = .data$diversity_val - ref_val)

    # Get bootstrap samples as a list
    bootstrap_samples_list <- lapply(bootstrap_samples_list_raw, function(df) {
      ref_val <- df %>%
        dplyr::filter(.data[[grouping_var]] == !!ref_group) %>%
        dplyr::pull(.data$diversity_val)

      df %>%
        dplyr::filter(.data[[grouping_var]] != !!ref_group) %>%
        dplyr::mutate(diversity_val = .data$diversity_val - ref_val)
    })
  } else {
    # Calculate true statistic
    if (rlang::inherits_any(data_cube, c("processed_cube", "sim_cube"))) {
      t0 <- fun(data_cube)$data
    } else {
      t0 <- fun(data_cube)
    }

    # Get bootstrap samples as a list
    bootstrap_samples_list <- bootstrap_samples_list_raw
  }

  # Summarise in dataframe
  bootstrap_samples_df <- bootstrap_samples_list %>%
    dplyr::bind_rows() %>%
    dplyr::rename("rep_boot" = "diversity_val") %>%
    dplyr::left_join(t0, by = grouping_var) %>%
    dplyr::rename("est_original" = "diversity_val") %>%
    dplyr::mutate(
      est_boot = mean(.data$rep_boot),
      se_boot = stats::sd(.data$rep_boot),
      .by = dplyr::all_of(grouping_var)) %>%
    dplyr::mutate(bias_boot = .data$est_boot - .data$est_original) %>%
    dplyr::arrange(.data[[grouping_var]]) %>%
    dplyr::select("sample", dplyr::all_of(grouping_var), "est_original",
                  dplyr::everything())

  return(bootstrap_samples_df)
}
