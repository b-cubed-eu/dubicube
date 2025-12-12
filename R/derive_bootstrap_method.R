#' Derive Whether a Statistic Requires Whole-Cube or Group-Specific Bootstrapping
#'
#' Infers whether a user-supplied indicator function (`fun`) behaves as a
#' **group-specific** statistic (computed independently within each category)
#' or a **whole-cube** statistic (requiring pooled information across all
#' categories).
#'
#' @param df A `data.frame` or tibble containing the full dataset used by the
#'   indicator function.
#' @param fun A function that computes an indicator. It must take a data frame
#'   as its first argument and return a data frame with one row per category and
#'   a column named `diversity_val` (or another comparable numeric result).
#' @param ... Additional arguments passed to `fun()`.
#' @param cat_var A character string giving the name of the categorical variable
#'   that defines the groups (e.g. `"year"`, `"species"`, `"site"`).
#' @param min_cat Integer. The minimum number of categories to include in the
#'   "short" dataset used for comparison. Defaults to `2`.
#' @param max_cat Integer. The maximum number of categories to include in the
#'   "long" dataset. Defaults to `5`.
#' @param index Integer. Position from the end of the sorted category list at
#'   which the subsets should be constructed. Use `-1` (default) to set this
#'   automatically based on `max_cat`.
#'
#' @return
#' A single character string: either
#'
#' * `"group_specific"` or
#' * `"whole_cube"`
#'
#' indicating the appropriate bootstrapping strategy.
#'
#' @details
#' The function evaluates bootstrap method by comparing the statistic computed
#' on two filtered subsets of the data:
#'
#' * **Short subset**: contains `min_cat` consecutive categories.
#' * **Long subset**: contains `max_cat` consecutive categories.
#'
#' Both subsets end at the same category index to ensure overlap.
#'
#' The indicator function is evaluated on both subsets. If the indicator values
#' for the overlapping categories match exactly, the function is considered
#' **group-specific**. Otherwise, it is assumed to require **whole-cube**
#' bootstrapping.
#'
#' @export
#'
#' @family indicator_uncertainty
#'
#' @import dplyr
#' @import assertthat
#' @importFrom rlang .data
#'
#' @examples
#' # 1. Calculate mean per group
#' sepal_length_per_species <- function(x, f) {
#'   out_df <- aggregate(x$Sepal.Length, by = list(x$Species), FUN = f)
#'   names(out_df) <- c("Species", "diversity_val")
#'   out_df
#' }
#' sepal_length_per_species(iris, mean)
#'
#' # The mean is calculated per species, so we expect the group-specific method
#' derive_bootstrap_method(
#'   df = iris,
#'   fun = sepal_length_per_species,
#'   cat_var = "Species",
#'   min_cat = 1,
#'   max_cat = 3,
#'   mean
#' )
#'
#' # 2. Calculate indicator based on whole dataset
#' sepal_length_per_species2 <- function(x, f) {
#'   out_df <- aggregate(x$Sepal.Length, by = list(x$Species), FUN = f)
#'   out_df$x <- out_df$x / nrow(out_df) # new line
#'   names(out_df) <- c("Species", "diversity_val")
#'   out_df
#' }
#' sepal_length_per_species2(iris, mean)
#'
#' # Now we expect the whole-cube method
#' derive_bootstrap_method(
#'   df = iris,
#'   fun = sepal_length_per_species2,
#'   cat_var = "Species",
#'   min_cat = 1,
#'   max_cat = 3,
#'   mean
#' )

derive_bootstrap_method <- function(
  df,
  fun,
  ...,
  cat_var,
  min_cat = 2,
  max_cat = 5,
  index = -1
) {
  ### Start checks
  # Check df input
  stopifnot("`df` must be a dataframe." = inherits(df, "data.frame"))

  # Check fun input
  stopifnot("`fun` must be a function." = is.function(fun))

  # Check if cat_var is a character vector
  stopifnot("`cat_var` must be a character vector." =
              is.character(cat_var))

  # Check if cat_var column is present in data cube
  stopifnot("`df` should contain column `cat_var`." =
              all(cat_var %in% names(df)))

  # Check if `min_cat` and `max_cat` are specified correctly
  stopifnot(
    "`min_cat` must be a single positive integer." =
      assertthat::is.count(min_cat)
  )
  stopifnot(
    "`max_cat` must be a single positive integer." =
      assertthat::is.count(max_cat)
  )
  stopifnot(
    "The min. number of categories must be smaller than the max. number." =
      min_cat < max_cat
  )

  # Check index input
  stopifnot(
    "`index` must be a single positive integer or -1." =
      assertthat::is.count(index) | index == -1
  )
  ### End checks

  # Return fast in easy case
  n_cat <- length(unique(df[[cat_var]]))
  if (n_cat == 1) return("whole_cube")

  # Parse number of categories to filter
  stopifnot(
    "`max_cat` cannot exceed the total number of categories in `cat_var`." =
      max_cat <= n_cat
  )
  min_cat <- min(n_cat, min_cat)
  max_cat <- min(n_cat, max_cat)
  if (index == -1) index <- n_cat - max_cat

  # Get dataset indices
  stop_index <- min(index + max_cat, n_cat)
  start_index_short <- stop_index - min_cat + 1
  start_index_long <- stop_index - max_cat + 1

  # Filter datasets
  data_short <- data %>%
    mutate(group = cur_group_id(),
           .by = all_of(cat_var)) %>%
    filter(.data$group %in% start_index_short:stop_index)
  short_groups <- unique(data_short[[cat_var]])

  data_long <- data %>%
    mutate(group = cur_group_id(),
           .by = all_of(cat_var)) %>%
    filter(group %in% start_index_long:stop_index)

  # Calculate statistics on short and long dataset
  stat_short <- fun(data_short[, -ncol(data_short)], ...)
  stat_long <- fun(data_long[, -ncol(data_long)], ...)

  # Compare results
  is_group_specific <- identical(
    stat_short$diversity_val,
    stat_long[stat_long[[cat_var]] %in% short_groups, ]$diversity_val
  )

  # Return bootstrap method
  ifelse(is_group_specific, "group_specific", "whole_cube")
}
