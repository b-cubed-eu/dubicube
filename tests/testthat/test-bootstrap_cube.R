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
  time_point = seq_along(years),
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
  # Calculate mean obs per year
  out_df <- aggregate(obs ~ time_point + year, data, mean)
  # Rename columns
  names(out_df) <- c("time_point", "year", "diversity_val")
  return(out_df)
}

mean_obs_processed <- function(data) {
  # Initiate output variable
  out_df <- NULL
  out_df$meta <- "Mean number of observations per year"
  # Calculate mean obs per year
  out_df$data <- aggregate(obs ~ time_point + year, data$data, mean)
  # Rename columns
  names(out_df$data) <- c("time_point", "year", "diversity_val")

  return(out_df)
}

## Perform bootstrapping
# Perform bootstrapping dataframe
result1 <- bootstrap_cube(
  data_cube = cube_df,
  fun = mean_obs,
  grouping_var = c("time_point", "year"),
  samples = 10,
  seed = 123
)

# Perform bootstrapping 'processed_cube'
result2 <- bootstrap_cube(
  data_cube = processed_cube,
  fun = mean_obs_processed,
  grouping_var = c("time_point", "year"),
  samples = 10,
  seed = 123
)

# Perform bootstrapping dataframe with reference group
result3 <- bootstrap_cube(
  data_cube = cube_df,
  fun = mean_obs,
  grouping_var = "year",
  samples = 10,
  seed = 123,
  ref_group = ref_year
)

# Perform bootstrapping 'processed_cube' with reference group
result4 <- bootstrap_cube(
  data_cube = processed_cube,
  fun = mean_obs_processed,
  grouping_var = "year",
  samples = 10,
  seed = 123,
  ref_group = ref_year
)

## Perform tests
# Test bootstrap_cube output
test_that("bootstrap_cube returns a dataframe with expected structure", {
  expect_s3_class(result1, "data.frame")
  expect_true(all(c("sample", "time_point", "year", "est_original", "rep_boot",
                    "est_boot", "se_boot", "bias_boot") %in% names(result1)))

  expect_s3_class(result2, "data.frame")
  expect_true(all(c("sample", "time_point", "year", "est_original", "rep_boot",
                    "est_boot", "se_boot", "bias_boot")  %in% names(result2)))

  expect_s3_class(result3, "data.frame")
  expect_true(all(c("sample", "year", "est_original", "rep_boot", "est_boot",
                    "se_boot", "bias_boot") %in% names(result3)))

  expect_s3_class(result4, "data.frame")
  expect_true(all(c("sample", "year", "est_original", "rep_boot", "est_boot",
                    "se_boot", "bias_boot") %in% names(result4)))
})

# Test that bootstrapping produces reasonable values
test_that("bootstrap_cube computes bootstrap statistics correctly", {
  years <- unique(cube_df$year)

  expect_true(all(result1$sample > 0))
  expect_true(all(result1$se_boot >= 0))
  expect_true(all(result1$year %in% years))

  expect_true(all(result2$sample > 0))
  expect_true(all(result2$se_boot >= 0))
  expect_true(all(result2$year %in% years))

  years_ref <- setdiff(years, ref_year)

  expect_true(all(result3$sample > 0))
  expect_true(all(result3$se_boot >= 0))
  expect_true(all(result3$year %in% years_ref))

  expect_true(all(result4$sample > 0))
  expect_true(all(result4$se_boot >= 0))
  expect_true(all(result4$year %in% years_ref))
})

# Test results based on data cube and processed data cube
test_that("identical results with normal and processed cube", {
  # Without reference group
  expect_equal(result1, result2)

  # With reference group
  expect_equal(result1, result2)
})

# Test reproducibility with seed
test_that("bootstrap_cube is reproducible with set seed", {
  result5 <- bootstrap_cube(
    data_cube = cube_df,
    fun = mean_obs,
    grouping_var = c("time_point", "year"),
    samples = 10,
    seed = 123
  )

  expect_equal(result1, result5)

  # without seed not identical
  result6 <- bootstrap_cube(
    data_cube = cube_df,
    fun = mean_obs,
    grouping_var = c("time_point", "year"),
    samples = 10
  )

  result7 <- bootstrap_cube(
    data_cube = cube_df,
    fun = mean_obs,
    grouping_var = c("time_point", "year"),
    samples = 10
  )

  expect_false(identical(result6, result7))
})

# Test handling of invalid input
test_that("bootstrap_cube handles invalid inputs gracefully", {
  expect_error(
    bootstrap_cube(
      data_cube = NULL,
      fun = mean_obs,
      grouping_var = "year",
      samples = 10),
    paste("`data_cube` must be a data cube object (class 'processed_cube' or",
          "'sim_cube') or a dataframe."),
    fixed = TRUE)

  expect_error(
    bootstrap_cube(
      data_cube = cube_df,
      fun = mean_obs,
      grouping_var = 2,
      samples = 10),
    "`grouping_var` must be a character vector.",
    fixed = TRUE)

  expect_error(
    bootstrap_cube(
      data_cube = cube_df,
      fun = mean_obs,
      grouping_var = "year",
      samples = -10),
    "`samples` must be a single positive integer.",
    fixed = TRUE)

  expect_error(
    bootstrap_cube(
      data_cube = cube_df,
      fun = mean_obs,
      grouping_var = "year",
      samples = 10,
      ref_group = 1:2),
    "`ref_group` must be a numeric/character vector of length 1 or NA.",
    fixed = TRUE)

  expect_error(
    bootstrap_cube(
      data_cube = cube_df,
      fun = mean_obs,
      grouping_var = "year",
      samples = 10,
      ref_group = "2020"),
    "`ref_group` is not present in `grouping_var` column of `data_cube`.",
    fixed = TRUE)

  expect_error(
    bootstrap_cube(
      data_cube = cube_df,
      fun = mean_obs,
      grouping_var = "year",
      samples = 10,
      progress = "TRUE"),
    "`progress` must be a logical vector of length 1.",
    fixed = TRUE)
})
