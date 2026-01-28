## Create example data
years <- 2014:2020
ref_year <- 2020
grid_cells <- c("E003N55BA", "E003N55BB", "E003N55BC")
species <- paste0("spec", 1:3)

set.seed(123)

# Create data cube as data.frame
cube_df <- expand.grid(
  year = years,
  cellCode = grid_cells,
  taxonKey = species,
  obs = rpois(5, 50)
)

## Function to calculate statistic of interest
# Mean observations per year per species
mean_obs <- function(data) {
  # Calculate mean obs per year
  out_df <- aggregate(obs ~ year + taxonKey, data, mean)
  # Rename columns
  names(out_df) <- c("year", "taxonKey", "diversity_val")
  return(out_df)
}

## Perform bootstrapping
# Perform bootstrapping dataframe
result1 <- bootstrap_cube_raw(
  data_cube = cube_df,
  fun = mean_obs,
  grouping_var = c("year", "taxonKey"),
  samples = 10,
  seed = 123
)

# Perform bootstrapping dataframe with reference group
result3 <- bootstrap_cube_raw(
  data_cube = cube_df,
  fun = mean_obs,
  grouping_var = c("year", "taxonKey"),
  samples = 10,
  seed = 123,
  ref_group = ref_year
)


## Perform tests
# Test bootstrap_cube_raw output
test_that("bootstrap_cube_raw returns a dataframe with expected structure", {
  expect_s3_class(result1, "data.frame")
  expect_true(all(c("sample", "taxonKey", "year", "est_original", "rep_boot",
                    "est_boot", "se_boot", "bias_boot") %in% names(result1)))

  expect_s3_class(result3, "data.frame")
  expect_true(all(c("sample", "taxonKey", "year", "est_original", "rep_boot",
                    "est_boot", "se_boot", "bias_boot") %in% names(result3)))
})

# Test that bootstrapping produces reasonable values
test_that("bootstrap_cube_raw computes bootstrap statistics correctly", {
  years <- unique(cube_df$year)

  expect_true(all(result1$sample > 0))
  expect_true(all(result1$se_boot >= 0))
  expect_true(all(result1$year %in% years))

  years_ref <- setdiff(years, ref_year)

  expect_true(all(result3$sample > 0))
  expect_true(all(result3$se_boot >= 0))
  expect_true(all(result3$year %in% years_ref))
})

# Test reproducibility with seed
test_that("bootstrap_cube_raw is reproducible with set seed", {
  result5 <- bootstrap_cube_raw(
    data_cube = cube_df,
    fun = mean_obs,
    grouping_var = c("year", "taxonKey"),
    samples = 10,
    seed = 123
  )

  expect_equal(result1, result5)

  # without seed not identical
  result6 <- bootstrap_cube_raw(
    data_cube = cube_df,
    fun = mean_obs,
    grouping_var = c("year", "taxonKey"),
    samples = 10
  )

  result7 <- bootstrap_cube_raw(
    data_cube = cube_df,
    fun = mean_obs,
    grouping_var = c("year", "taxonKey"),
    samples = 10
  )

  expect_false(identical(result6, result7))
})

# Test handling of invalid input
test_that("bootstrap_cube_raw handles invalid inputs gracefully", {
  expect_error(
    bootstrap_cube_raw(
      data_cube = NULL,
      fun = mean_obs,
      grouping_var = "year",
      samples = 10
    ),
    "`data_cube` must be a dataframe.",
    fixed = TRUE
  )

  expect_error(
    bootstrap_cube_raw(
      data_cube = cube_df,
      fun = mean_obs,
      grouping_var = 2,
      samples = 10
    ),
    "`grouping_var` must be a character vector.",
    fixed = TRUE
  )

  expect_error(
    bootstrap_cube_raw(
      data_cube = cube_df,
      fun = mean_obs,
      grouping_var = "year",
      samples = -10
    ),
    "`samples` must be a single positive integer.",
    fixed = TRUE
  )

  expect_error(
    bootstrap_cube_raw(
      data_cube = cube_df,
      fun = mean_obs,
      grouping_var = "year",
      samples = 10,
      ref_group = 1:2
    ),
    "`ref_group` must be a numeric/character vector of length 1 or NA.",
    fixed = TRUE
  )

  expect_error(
    bootstrap_cube_raw(
      data_cube = cube_df,
      fun = mean_obs,
      grouping_var = "year",
      samples = 10,
      ref_group = "twothousandtwenty"
    ),
    "`ref_group` is not present in `grouping_var` column of `data_cube`.",
    fixed = TRUE
  )

  expect_error(
    bootstrap_cube_raw(
      data_cube = cube_df,
      fun = mean_obs,
      grouping_var = "year",
      samples = 10,
      progress = "TRUE"
    ),
    "`progress` must be a logical vector of length 1.",
    fixed = TRUE
  )
})
