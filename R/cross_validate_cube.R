#' Leave-one-out cross-validation for data cubes
#'
#' This function performs leave-one-out (LOO) or k-fold (experimental)
#' cross-validation (CV) on a biodiversity data cube to assess the performance
#' of a specified indicator function. It partitions the data by a specified
#' variable, calculates the specified indicator on training data, and compares
#' it with the true values to evaluate the influence of one or more categories
#' on the final result.
#'
#' @param data_cube A data cube object (class 'processed_cube' or 'sim_cube',
#' see `b3gbi::process_cube()`) or a dataframe (from `$data` slot of
#' 'processed_cube' or 'sim_cube').
#' @param fun A function which, when applied to `data_cube` returns the
#' statistic(s) of interest. This function must return a dataframe with a column
#' `diversity_val` containing the statistic of interest.
#' @param ... Additional arguments passed on to `fun`.
#' @param grouping_var A string specifying the grouping variable(s) for `fun`.
#' The output of `fun(data_cube)` returns a row per group.
#' @param out_var A string specifying the column by which the data should be
#' left out iteratively. Default is `"taxonKey"` which can be used for
#' leave-one-species-out CV.
#' @param crossv_method Method of data partitioning.
#' If `crossv_method = "loo"` (default),
#' `S = number of unique values in out_var` training partitions are created
#' containing `S - 1` rows each.
#' If `crossv_method = "kfold"`, the aggregated data is split the data into
#' `k` exclusive partitions containing `S / k` rows each. K-fold CV is
#' experimental and results should be interpreted with caution.
#' @param k Number of folds (an integer). Used only if
#' `crossv_method = "kfold"`. Default 5.
#' @param progress Logical. Whether to show a progress bar. Set to `TRUE` to
#' display a progress bar, `FALSE` (default) to suppress it.
#' @param max_out_cats An integer specifying the maximum number of unique
#' categories in `out_var` to leave out iteratively. Default is `1000`.
#' This can be increased if needed, but higher values may significantly increase
#' runtime.
#'
#' @returns A dataframe containing the cross-validation results with the
#' following columns:
#'   - Cross-Validation id (`id_cv`)
#'   - The grouping variable `grouping_var` (e.g., year)
#'   - The category left out during each cross-validation iteration
#'   (`cat_left_out`)
#'   - The computed statistic values for both training (`rep_cv`) and true
#' datasets (`est_original`)
#'   - Error metrics: error (`error`), squared error (`sq_error`),
#'   absolute difference (`abs_diff`), relative difference (`rel_diff`), and
#'   percent difference (`perc_diff`)
#'   - Error metrics summarised by `grouping_var`: mean relative difference
#' (`mre`), mean squared error (`mse`) and root mean squared error (`rmse`)
#'
#' See Details section on how these error metrics are calculated.
#'
#' @details
#' Details here
#'
#' @export
#'
#' @family uncertainty
#'
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom modelr crossv_kfold
#' @importFrom purrr map
#'
#' @examples
#' # Example here

cross_validate_cube <- function(
    data_cube,
    fun,
    grouping_var,
    out_var = "taxonKey",
    crossv_method = c("loo", "kfold"),
    k = ifelse(crossv_method == "kfold", 5, NA),
    max_out_cats = 1000,
    progress = FALSE) {

  # Define cross-validation function
  cross_validate_f <- function(x, fun) {
    data_cube_copy <- data_cube
    data_cube_copy$data <- x

    fun(data_cube_copy)$data
  }

  # Calculate true statistic
  t0 <- fun(data_cube)$data

  # Check if crossv_method is loo or kfold
  crossv_method <- tryCatch({
    match.arg(crossv_method, c("loo", "kfold"))
  }, error = function(e) {
    stop("`crossv_method` must be one of 'loo', 'kfold'.",
         call. = FALSE)
  })

  if (crossv_method == "loo") {
    # Create cross validation datasets
    taxon_list <- unique(data_cube$data$taxonKey)
    cv_datasets <- lapply(taxon_list, function(taxon) {
      data_cube$data[data_cube$data$taxonKey != taxon, ]
    })

    # Get species left out
    species_df <- data.frame(
      id_cv = seq_along(taxon_list),
      cat_left_out = taxon_list
    )
  } else {
    # Species partitioning
    taxon_list <- data_cube$data %>%
      distinct(.data$taxonKey) %>%
      modelr::crossv_kfold(id = "id_cv", k = k)

    # Get species left out
    cat_left_out_list <- lapply(lapply(taxon_list$test, as.integer),
                                    function(indices) {
                                      df <- data_cube$data %>%
                                        distinct(.data$taxonKey)

                                      df[indices, ] %>%
                                        pull(.data$taxonKey)
                                    }
    )
    names(cat_left_out_list) <- NULL

    species_df <- tibble(
      id_cv = as.numeric(taxon_list$id_cv),
      cat_left_out = cat_left_out_list
    )

    # Create cross validation datasets
    cv_datasets <- lapply(cat_left_out_list, function(taxa) {
      data_cube$data[!data_cube$data$taxonKey %in% taxa, ]
    })
  }

  # Perform function on training data
  results <- cv_datasets %>%
    purrr::map(
      cross_validate_f,
      fun = fun,
      .progress = ifelse(progress, "Cross-Validation", progress))

  # Summarise CV statistics in dataframe
  out_df <- results %>%
    dplyr::bind_rows(.id = "id_cv") %>%
    dplyr::mutate(id_cv = as.numeric(.data$id_cv)) %>%
    dplyr::full_join(species_df, by = join_by("id_cv")) %>%
    dplyr::rename("rep_cv" = "diversity_val") %>%
    dplyr::left_join(t0, by = grouping_var) %>%
    dplyr::rename("est_original" = "diversity_val") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      error =  .data$rep_cv - .data$est_original,
      sq_error = .data$error^2,
      abs_diff = abs(.data$error),
      rel_diff = .data$abs_diff / (.data$est_original + 10^-8),
      perc_diff = .data$rel_diff * 100
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      mre = mean(.data$rel_diff),
      mse = mean(.data$sq_error),
      rmse = sqrt(.data$mse),
      .by = all_of(grouping_var)) %>%
    dplyr::arrange(.data[[grouping_var]]) %>%
    dplyr::select("id_cv", all_of(grouping_var), "cat_left_out", "rep_cv",
                  "est_original", dplyr::everything())

  return(out_df)
}
