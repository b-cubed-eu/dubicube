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
#' 'processed_cube' or 'sim_cube'). To limit runtime, we recommend using a
#' dataframe with custom function as `fun`.
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
#' This can be increased if needed, but keep in mind that a high number of
#' categories in `out_var` may significantly increase runtime.
#'
#' @returns A dataframe containing the cross-validation results with the
#' following columns:
#'   - Cross-Validation id (`id_cv`)
#'   - The grouping variable `grouping_var` (e.g., year)
#'   - The category left out during each cross-validation iteration
#'   (specified `out_var` with suffix '_out')
#'   - The computed statistic values for both training (`rep_cv`) and true
#' datasets (`est_original`)
#'   - Error metrics: error (`error`), squared error (`sq_error`),
#'   absolute difference (`abs_error`), relative difference (`rel_error`), and
#'   percent difference (`perc_error`)
#'   - Error metrics summarised by `grouping_var`: mean relative difference
#' (`mre`), mean squared error (`mse`) and root mean squared error (`rmse`)
#'
#' See Details section on how these error metrics are calculated.
#'
#' @details
#' This function assesses the influence of each category in `out_var` on the
#' indicator value by iteratively leaving out one category at a time, similar to
#' leave-one-out cross-validation. K-fold CV works in a similar fashion but is
#' experimental and will not be covered here.
#'
#' 1. **Original Sample Data**:
#' \eqn{\mathbf{X} = \{X_{11}, X_{12}, X_{13}, \ldots, X_{sn}\}}
#'    - The initial set of observed data points, where there are \eqn{s}
#'    different categories in `out_var` and \eqn{n}
#'    total samples across all
#'    categories (= the sample size). \eqn{n}
#'    corresponds to the number of cells
#'    in a data cube or the number of rows in tabular format.
#'
#' 2. **Statistic of Interest**: \eqn{\theta}
#'    - The parameter or statistic being estimated, such as the mean
#'    \eqn{\bar{X}}, variance \eqn{\sigma^2}, or a biodiversity indicator. Let
#'    \eqn{\hat{\theta}}
#'    denote the estimated value of \eqn{\theta}
#'    calculated from the complete dataset \eqn{\mathbf{X}}.
#'
#' 3. **Cross-Validation (CV) Sample**: \eqn{\mathbf{X}_{-s_j}}
#'    - The full dataset \eqn{\mathbf{X}}
#'    excluding all samples belonging to
#'    category \eqn{j}. This subset is used to investigate the influence of
#'    category \eqn{j} on the estimated statistic \eqn{\hat{\theta}}.
#'
#' 4. **CV Estimate for Category** \eqn{\mathbf{j}}: \eqn{\hat{\theta}_{-s_j}}
#'    - The value of the statistic of interest calculated from
#'    \eqn{\mathbf{X}_{-s_j}}, which excludes category \eqn{j}.
#'    For example, if \eqn{\theta} is the sample mean,
#'    \eqn{\hat{\theta}_{-s_j} = \bar{X}_{-s_j}}.
#'
#' 5. **Error Measures**:
#'
#'    - The **Error** is the difference between the statistic estimated without
#'    category \eqn{j} (\eqn{\hat{\theta}_{-s_j}}) and the statistic calculated
#'    on the complete dataset (\eqn{\hat{\theta}}).
#'
#'    \deqn{\text{Error}_{s_j} = \hat{\theta}_{-s_j} - \hat{\theta}}
#'
#'    - The **Relative Error** is the absolute error, normalised by the true
#'    estimate \eqn{\hat{\theta}}
#'    and a small error term
#'    \eqn{\epsilon = 10^{-8}} to avoid division by zero.
#'
#'    \deqn{\text{Rel. Error}_{s_j} = \frac{|\hat{\theta}_{-s_j} -
#'    \hat{\theta}|}{\hat{\theta} +\epsilon}}
#'
#'    - The **Percent Error** is the relative error expressed as a percentage.
#'
#'    \deqn{\text{Perc. Error}_{s_j} = \text{Rel. Error}_{s_j} \times 100 \%}
#'
#' 6. **Summary Measures**:
#'
#'    - The **Mean Relative Error (MRE)** is the average of the relative errors
#'    over all categories.
#'
#'    \deqn{\text{MRE} = \frac{1}{s} \sum_{j=1}^s \text{Rel. Error}_{s_j}}
#'
#'    - The **Mean Squared Error (MSE)** is the average of the squared errors.
#'
#'    \deqn{\text{MSE} = \frac{1}{s} \sum_{j=1}^s (\text{Error}_{s_j})^2}
#'
#'    - The **Root Mean Squared Error (RMSE)** is the square root of the MSE.
#'
#'    \deqn{\text{RMSE} = \sqrt{\text{MSE}}}
#'
#' @export
#'
#' @family robustness
#'
#' @import dplyr
#' @import assertthat
#' @importFrom rlang .data
#' @importFrom stats setNames
#' @importFrom data.table :=
#' @importFrom modelr crossv_kfold
#' @importFrom purrr map
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
#' # Perform leave-one-species-out CV
#' \donttest{
#' cv_mean_obs <- cross_validate_cube(
#'   data_cube = denmark_cube$data,
#'   fun = mean_obs,
#'   grouping_var = "year",
#'   out_var = "taxonKey",
#'   crossv_method = "loo",
#'   progress = FALSE)
#' head(cv_mean_obs)
#' }

cross_validate_cube <- function(
    data_cube,
    fun,
    ...,
    grouping_var,
    out_var = "taxonKey",
    crossv_method = c("loo", "kfold"),
    k = ifelse(crossv_method == "kfold", 5, NA),
    max_out_cats = 1000,
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

  # Check fun input
  stopifnot("`fun` must be a function." = is.function(fun))

  # Check if grouping_var is a character vector of length 1
  stopifnot("`grouping_var` must be a character vector of length 1." =
              assertthat::is.string(grouping_var))

  # Check if out_var is a character vector of length 1
  stopifnot("`out_var` must be a character vector of length 1." =
              assertthat::is.string(out_var))

  # Check if crossv_method is loo or kfold
  crossv_method <- tryCatch({
    match.arg(crossv_method, c("loo", "kfold"))
  }, error = function(e) {
    stop("`crossv_method` must be one of 'loo', 'kfold'.",
         call. = FALSE)
  })

  # Check if k is NA or an integer
  stopifnot(
    "`k` must be a positive integer of length 1 or NA." =
      (assertthat::is.count(k) | is.na(k)) & length(k) == 1)

  # Check if max_out_cats is a positive integer
  stopifnot(
    "`max_out_cats` must be a single positive integer." =
      assertthat::is.count(max_out_cats))

  # Check if progress is a logical vector of length 1
  stopifnot("`progress` must be a logical vector of length 1." =
              assertthat::is.flag(progress))
  ### End checks

  if (rlang::inherits_any(data_cube, c("processed_cube", "sim_cube"))) {
    # Check if grouping_var column is present in data cube
    stopifnot("`data_cube` should contain column `grouping_var`." =
                grouping_var %in% names(data_cube$data))

    # Check if out_var column is present in data cube
    stopifnot("`data_cube` should contain column `out_var`." =
                out_var %in% names(data_cube$data))

    # Define cross-validation function
    cross_validate_f <- function(x, fun, ...) {
      data_cube_copy <- data_cube
      data_cube_copy$data <- x

      fun(data_cube_copy, ...)$data
    }

    # Calculate true statistic
    t0 <- fun(data_cube, ...)$data

    # Save data cube data
    data_cube_df <- data_cube$data
  } else {
    # Check if grouping_var column is present in data cube
    stopifnot("`data_cube` should contain column `grouping_var`." =
                grouping_var %in% names(data_cube))

    # Check if out_var column is present in data cube
    stopifnot("`data_cube` should contain column `out_var`." =
                out_var %in% names(data_cube))

    # Define cross-validation function
    cross_validate_f <- function(x, fun, ...) {
      fun(x, ...)
    }

    # Calculate true statistic
    t0 <- fun(data_cube, ...)

    # Save data cube data
    data_cube_df <- data_cube
  }

  # Checks for number of categories in out_var
  num_cats <- length(unique(data_cube_df[[out_var]]))

  # Check if number of categories is not larger than control argument
  cat_message <- paste(
    "Number of categories in `out_var` is larger than `max_out_cats`.",
    "Increase the number of `max_out_cats`.", sep = "\n")
  do.call(stopifnot,
          stats::setNames(list(
            num_cats <= max_out_cats),
            cat_message)
  )
  # Warn for long runtime
  if (num_cats > 1000) {
    warning(
      paste("Number of categories in `out_var` is larger than 1000.",
            "Runtime of Cross-Validation may be substantial.", sep = "\n")
    )
  }

  # Perform cross-validation
  if (crossv_method == "loo") {
    # Create cross validation datasets
    cat_list <- unique(data_cube_df[[out_var]])
    cv_datasets <- lapply(cat_list, function(cat) {
      data_cube_df[data_cube_df[[out_var]] != cat, ]
    })

    # Get category left out
    category_df <- data.frame(
      id_cv = seq_along(cat_list),
      cat_left_out = cat_list
    )
  } else {
    # Category partitioning
    cat_list <- data_cube_df %>%
      distinct(.data[[out_var]]) %>%
      modelr::crossv_kfold(id = "id_cv", k = k)

    # Get category left out
    cat_left_out_list <- lapply(lapply(cat_list$test, as.integer),
                                    function(indices) {
                                      df <- data_cube_df %>%
                                        distinct(.data[[out_var]])

                                      df[indices, ] %>%
                                        pull(.data[[out_var]])
                                    }
    )
    names(cat_left_out_list) <- NULL

    category_df <- dplyr::tibble(
      id_cv = as.numeric(cat_list$id_cv),
      cat_left_out = cat_left_out_list
    )

    # Create cross validation datasets
    cv_datasets <- lapply(cat_left_out_list, function(cats) {
      data_cube_df[!data_cube_df[[out_var]] %in% cats, ]
    })
  }

  # Perform function on training data
  results <- cv_datasets %>%
    purrr::map(
      cross_validate_f,
      fun = fun,
      .progress = ifelse(progress, "Cross-Validation", progress))

  # Summarise CV statistics in dataframe
  out_col_name <- paste(gsub("[^a-zA-Z0-9]", "_", tolower(out_var)),
                        "out", sep = "_")
  out_df <- results %>%
    dplyr::bind_rows(.id = "id_cv") %>%
    dplyr::mutate(id_cv = as.numeric(.data$id_cv)) %>%
    dplyr::full_join(category_df, by = join_by("id_cv")) %>%
    dplyr::rename("rep_cv" = "diversity_val") %>%
    dplyr::left_join(t0, by = grouping_var) %>%
    dplyr::rename("est_original" = "diversity_val") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      error =  .data$rep_cv - .data$est_original,
      sq_error = .data$error^2,
      abs_error = abs(.data$error),
      rel_error = .data$abs_error / (.data$est_original + 10^-8),
      perc_error = .data$rel_error * 100
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      mre = mean(.data$rel_error),
      mse = mean(.data$sq_error),
      rmse = sqrt(.data$mse),
      .by = dplyr::all_of(grouping_var)) %>%
    dplyr::arrange(.data[[grouping_var]]) %>%
    dplyr::select("id_cv", all_of(grouping_var),
                  !!out_col_name := "cat_left_out",
                  "rep_cv", "est_original",
                  dplyr::everything()) %>%
    as.data.frame()

  return(out_df)
}
