# Create example data
library(b3gbi)
cube_path <- system.file(
  "extdata", "denmark_mammals_cube_eqdgc.csv",
  package = "b3gbi")
denmark_cube <- process_cube(
  cube_path,
  first_year = 2014,
  last_year = 2020)
ref_year <- 2020

# Function to calculate statistic of interest
# Mean observerations per year
mean_obs <- function(data) {
  out_df <- aggregate(obs ~ year, data, mean) # Calculate mean obs per year
  names(out_df) <- c("year", "diversity_val") # Rename columns
  return(out_df)
}

# Perform bootsrapping dataframe
result1 <- bootstrap_cube(
  data_cube = denmark_cube$data,
  fun = mean_obs,
  grouping_var = "year",
  samples = 10,
  seed = 123
)

# Perform bootsrapping 'processed_cube'
result2 <- bootstrap_cube(
  data_cube = denmark_cube,
  fun = b3gbi::pielou_evenness_ts,
  grouping_var = "year",
  samples = 10,
  seed = 123
)

# Perform bootsrapping dataframe with reference group
result3 <- bootstrap_cube(
  data_cube = denmark_cube$data,
  fun = mean_obs,
  grouping_var = "year",
  samples = 10,
  seed = 123,
  ref_group = ref_year
)

# Perform bootsrapping 'processed_cube' with reference group
result4 <- bootstrap_cube(
  data_cube = denmark_cube,
  fun = b3gbi::pielou_evenness_ts,
  grouping_var = "year",
  samples = 10,
  seed = 123,
  ref_group = ref_year
)

# Test bootstrap_cube output
test_that("bootstrap_cube returns a dataframe with expected structure", {
  expect_s3_class(result1, "data.frame")
  expect_true(all(c("sample", "year", "est_original", "rep_boot", "est_boot",
                    "se_boot", "bias_boot") %in% names(result1)))

  expect_s3_class(result2, "data.frame")
  expect_true(all(c("sample", "year", "est_original", "rep_boot", "est_boot",
                    "se_boot", "bias_boot") %in% names(result2)))

  expect_s3_class(result3, "data.frame")
  expect_true(all(c("sample", "year", "est_original", "rep_boot", "est_boot",
                    "se_boot", "bias_boot") %in% names(result3)))

  expect_s3_class(result4, "data.frame")
  expect_true(all(c("sample", "year", "est_original", "rep_boot", "est_boot",
                    "se_boot", "bias_boot") %in% names(result4)))
})

# Test that bootstrapping produces reasonable values
test_that("bootstrap_cube computes bootstrap statistics correctly", {
  years <- unique(denmark_cube$data$year)

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

# Test reproducibility with seed
test_that("bootstrap_cube is reproducible with set seed", {
  result5 <- bootstrap_cube(
    data_cube = denmark_cube$data,
    fun = mean_obs,
    grouping_var = "year",
    samples = 10,
    seed = 123
  )

  expect_equal(result1, result5)

  # without seed not identical
  result6 <- bootstrap_cube(
    data_cube = denmark_cube$data,
    fun = mean_obs,
    grouping_var = "year",
    samples = 10
  )

  result7 <- bootstrap_cube(
    data_cube = denmark_cube$data,
    fun = mean_obs,
    grouping_var = "year",
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
          "'sim_cube') or a dataframe."))
  expect_error(
    bootstrap_cube(
      data_cube = denmark_cube$data,
      fun = mean_obs,
      grouping_var = 2,
      samples = 10),
    "`grouping_var` must be a character vector of length 1.")
  expect_error(
    bootstrap_cube(
      data_cube = denmark_cube$data,
      fun = mean_obs,
      grouping_var = "year",
      samples = -10),
    "`samples` must be a single positive integer.")
  expect_error(
    bootstrap_cube(
      data_cube = denmark_cube$data,
      fun = mean_obs,
      grouping_var = "year",
      samples = 10,
      ref_group = 1:2),
    "`ref_group` must be a numeric/character vector of length 1 or NA.")
  expect_error(
    bootstrap_cube(
      data_cube = denmark_cube$data,
      fun = mean_obs,
      grouping_var = "year",
      samples = 10,
      progress = "TRUE"),
    "`progress` must be a logical vector of length 1.")
})
