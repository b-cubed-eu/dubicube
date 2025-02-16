## Create example data
years <- 2014:2020
ref_year <- 2020
grid_cells <- c("E003N55BA", "E003N55BB", "E003N55BC")
species <- paste0("spec", 1:3)

# Simulate observations
get_obs <- function(x, int, slope) {
  sapply(seq_along(x), function(i) {
    rpois(1, int + slope * i)
  })
}

set.seed(123)
obs1 <- as.vector(
  sapply(seq_along(grid_cells), function(i) get_obs(years, 30, 3))
)
obs2 <- as.vector(
  sapply(seq_along(grid_cells), function(i) get_obs(years, 50, 0))
)
obs3 <- as.vector(
  sapply(seq_along(grid_cells), function(i) get_obs(years, 60, -2))
)

# Create data cube as data.frame
cube_df <- expand.grid(
  year = years,
  cellCode = grid_cells,
  taxonKey = species)
cube_df$obs <- c(obs1, obs2, obs3)

# Create data cube as 'processed_cube'
processed_cube <- NULL
processed_cube$meta <- "This is a processed occurrence cube"
processed_cube$data <- cube_df
class(processed_cube) <- "processed_cube"

## Function to calculate statistic of interest
# Mean observations per year
mean_obs <- function(data) {
  if (inherits(data, "processed_cube")) {
    data <- data$data
  }
  out_df <- aggregate(obs ~ year, data, mean) # Calculate mean obs per year
  names(out_df) <- c("year", "diversity_val") # Rename columns
  return(out_df)
}

mean_obs_processed <- function(data) {
  out_df <- NULL
  out_df$meta <- "Mean number of observations per year"

  # Calculate mean obs per year
  out_df$data <- aggregate(obs ~ year, data$data, mean)
  names(out_df$data) <- c("year", "diversity_val") # Rename columns

  return(out_df)
}

## Perform Cross-Validation
# Perform LOO CV dataframe
result1 <- cross_validate_cube(
  data_cube = cube_df,
  fun = mean_obs,
  grouping_var = "year",
  out_var = "taxonKey",
  crossv_method = "loo",
  progress = FALSE)

# Perform LOO CV 'processed_cube'
result2 <- cross_validate_cube(
  data_cube = processed_cube,
  fun = mean_obs_processed,
  grouping_var = "year",
  out_var = "taxonKey",
  crossv_method = "loo",
  progress = FALSE)

# Create extra data from k-fold CV
species2 <- paste0("spec", 4:6)
obs1 <- as.vector(
  sapply(seq_along(grid_cells), function(i) get_obs(years, 30, 3))
)
obs2 <- as.vector(
  sapply(seq_along(grid_cells), function(i) get_obs(years, 50, 0))
)
obs3 <- as.vector(
  sapply(seq_along(grid_cells), function(i) get_obs(years, 60, -2))
)

# Create data cube as data.frame
cube_df2 <- expand.grid(
  year = years,
  cellCode = grid_cells,
  taxonKey = species2)
cube_df2$obs <- c(obs1, obs2, obs3)

cube_df2 <- rbind(cube_df, cube_df2)

# Create data cube as 'processed_cube'
processed_cube2 <- NULL
processed_cube2$meta <- "This is a processed occurrence cube"
processed_cube2$data <- cube_df
class(processed_cube2) <- "processed_cube"

# Perform kfold CV dataframe
result3 <- cross_validate_cube(
  data_cube = cube_df2,
  fun = mean_obs,
  grouping_var = "year",
  out_var = "taxonKey",
  crossv_method = "kfold",
  k = 3,
  progress = FALSE)

# Perform kfold CV 'processed_cube'
result4 <- cross_validate_cube(
  data_cube = processed_cube2,
  fun = mean_obs_processed,
  grouping_var = "year",
  out_var = "taxonKey",
  crossv_method = "kfold",
  k = 3,
  progress = FALSE)

loo_results <- list(result1, result2)
kfold_results <- list(result3, result4)
results_ls <- c(loo_results, kfold_results)

## Perform tests
# Test cross_validate_cube output
test_that("cross_validate_cube returns a dataframe with expected structure", {
  # Data frame
  lapply(results_ls, function(df) {
    expect_s3_class(df, "data.frame")
  })

  # Correct column names
  # All results
  lapply(results_ls, function(df) {
    expect_true(all(
      c("id_cv", "year", "taxonkey_out", "rep_cv", "est_original", "error",
        "sq_error", "abs_error", "rel_error", "perc_error",
        "mre", "mse", "rmse") %in% names(df)
    ))
  })
  # Naming of out_var column
  cube_df3 <- cube_df
  names(cube_df3) <- c("year", "cellCode", "Taxon key", "obs")
  result5 <- cross_validate_cube(
    data_cube = cube_df3,
    fun = mean_obs,
    grouping_var = "year",
    out_var = "Taxon key",
    crossv_method = "loo",
    progress = FALSE)

  expect_true(all(
    c("id_cv", "year", "taxon_key_out", "rep_cv", "est_original", "error",
      "sq_error", "abs_error", "rel_error", "perc_error",
      "mre", "mse", "rmse") %in% names(result5)
  ))
})

# Test that Cross-Validation produces reasonable values
test_that("cross_validate_cube computes bootstrap statistics correctly", {
  # Calculated statistics are numeric
  lapply(results_ls, function(df) {
    lapply(df[, match("error", names(df)):match("rmse", names(df))],
           function(column) {
             expect_true(all(is.numeric(column)))
           })
  })

  # Calculated statistics are positive (not error)
  lapply(results_ls, function(df) {
    lapply(df[, match("sq_error", names(df)):match("rmse", names(df))],
           function(column) {
             expect_true(all(column > 0))
           })
  })

  # Check if out_var output is correct
})

# Test handling of out_var argument
# test_that("out_var argument works correctly", {
  # leave one dataset out cv

  # number of categories warning enzo
# })

# Test handling of invalid input
# test_that("cross_validate_cube handles invalid inputs gracefully", {

# })
