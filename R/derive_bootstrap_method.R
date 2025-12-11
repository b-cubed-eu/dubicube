derive_bootstrap_method <- function(
  df,
  fun,
  ...,
  cat_var,
  min_cat = 2,
  max_cat = 5,
  index = -1
) {
  # Checks

  # Return fast in easy case
  n_cat <- length(unique(df[[cat_var]]))
  if (n_cat == 1) return("whole_cube")

  min_cat <- min(n_cat, min_cat)
  max_cat <- min(n_cat, max_cat)
  if (index == -1) index <- n_cat

  # This only works for going back (index is final) but if not -1 it should be the first
  # unless index is 8 where n_cat is 10 and max_cat is 5, then it should be 6-10
  start_index_short <- index - min_cat
  start_index_long <- index - max_cat
  stop_index <- index

  # Filter datasets
  data_short <- data
  data_long <- data

  # Calculate statistics on short and long dataset
  stat_short <- fun(data_short, ...)
  stat_long <- fun(data_long, ...)

  # Compare results
  is_group_specific <- identical(
    stat_short$data$diversity_val,
    subset(stat_long$data, year >= first_year, year <= last_year)$diversity_val
  )

  # Return bootstrap method
  ifelse(is_group_specific, "group_specific", "whole_cube")
}
