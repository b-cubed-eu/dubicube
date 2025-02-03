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
#' @returns A dataframe summarizing the bootstrap results. The returned
#' dataframe contains the original statistic (`est_original`), the bootstrapped
#' estimate (`est_boot`), the standard error of the bootstrap estimates
#' (`se_boot`), and the bias of the bootstrapped estimates (`bias_boot`). Each
#' row corresponds to a bootstrap replicate.
#'
#' @export
#'
#' @examples
#' # Example usage
#' data_cube <- b3gbi::process_cube(insect_data_new)
#' bootstrap_results <- bootstrap_cube(
#'   data_cube = data_cube,
#'   fun = my_statistic_function,
#'   grouping_var = "time_point",
#'   samples = 1000,
#'   ref_group = "baseline"
#' )

bootstrap_cube <- function(
    data_cube,
    fun,
    grouping_var,
    samples = 1000,
    ref_group = NA,
    seed = NA,
    progress = FALSE) {
  require("dplyr")
  require("rlang")

  # Check if seed is NA or a number
  stopifnot("`seed` must be a numeric vector of length 1 or NA." =
              (is.numeric(seed) | is.na(seed)) &
              length(seed) == 1)

  # Set seed if provided
  if (!is.na(seed)) {
    if (exists(".Random.seed", envir = .GlobalEnv)) {
      rng_state_old <- get(".Random.seed", envir = .GlobalEnv)
      on.exit(assign(".Random.seed", rng_state_old, envir = .GlobalEnv))
    }
    set.seed(seed)
  }

  if (inherits(data_cube, "processed_cube")) {
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
        mutate(sample = as.integer(x$id))
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
        mutate(sample = as.integer(x$id))
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
    if (inherits(data_cube, "processed_cube")) {
      t0_full <- fun(data_cube)$data
    } else {
      t0_full <- fun(data_cube)
    }

    ref_val <- t0_full %>%
      filter(.data[[grouping_var]] == !!ref_group) %>%
      pull(.data$diversity_val)

    t0 <- t0_full %>%
      filter(.data[[grouping_var]] != !!ref_group) %>%
      mutate(diversity_val = .data$diversity_val - ref_val)

    # Get bootstrap samples as a list
    bootstrap_samples_list <- lapply(bootstrap_samples_list_raw, function(df) {
      ref_val <- df %>%
        filter(.data[[grouping_var]] == !!ref_group) %>%
        pull(.data$diversity_val)

      df %>%
        filter(.data[[grouping_var]] != !!ref_group) %>%
        mutate(diversity_val = .data$diversity_val - ref_val)
    })
  } else {
    # Calculate true statistic
    if (inherits(data_cube, "processed_cube")) {
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
      .by = all_of(grouping_var)) %>%
    dplyr::mutate(bias_boot = .data$est_boot - .data$est_original) %>%
    dplyr::arrange(.data[[grouping_var]]) %>%
    dplyr::select("sample", all_of(grouping_var), "est_original",
                  dplyr::everything())

  return(bootstrap_samples_df)
}
