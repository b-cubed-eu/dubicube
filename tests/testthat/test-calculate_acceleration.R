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

# Create data cube as 'processed_cube'
processed_cube <- NULL
processed_cube$meta <- "This is a processed occurrence cube"
processed_cube$data <- cube_df
class(processed_cube) <- "processed_cube"

## Function to calculate statistic of interest
# Mean observations per year per species
mean_obs <- function(data) {
  # Calculate mean obs per year
  out_df <- aggregate(obs ~ year + taxonKey, data, mean)
  # Rename columns
  names(out_df) <- c("year", "taxonKey", "diversity_val")
  return(out_df)
}

mean_obs_simple <- function(data) {
  # Calculate mean obs per year
  out_df <- aggregate(obs ~ year, data, mean)
  # Rename columns
  names(out_df) <- c("year", "diversity_val")
  return(out_df)
}

## Calculate acceleration
# Input dataframe
acceleration_df1 <- calculate_acceleration(
  data_cube = cube_df,
  fun = mean_obs,
  grouping_var = c("year", "taxonKey"),
  influence_method = "usual",
  processed_cube = FALSE
)

# Input 'processed_cube'
acceleration_df2 <- calculate_acceleration(
  data_cube = processed_cube,
  fun = mean_obs,
  grouping_var = c("year", "taxonKey"),
  influence_method = "pos"
)

# Input dataframe with reference group
acceleration_df3 <- calculate_acceleration(
  data_cube = cube_df,
  fun = mean_obs,
  grouping_var = c("year", "taxonKey"),
  ref_group = ref_year,
  influence_method = "pos",
  processed_cube = FALSE
)

# Input 'processed_cube' with reference group
acceleration_df4 <- calculate_acceleration(
  data_cube = processed_cube,
  fun = mean_obs,
  grouping_var = c("year", "taxonKey"),
  ref_group = ref_year,
  influence_method = "usual"
)

# Single grouping variable
acceleration_df12 <- calculate_acceleration(
  data_cube = cube_df,
  fun = mean_obs_simple,
  grouping_var = c("year"),
  influence_method = "usual",
  processed_cube = FALSE
)
acceleration_df22 <- calculate_acceleration(
  data_cube = processed_cube,
  fun = mean_obs_simple,
  grouping_var = c("year"),
  influence_method = "pos"
)
acceleration_df32 <- calculate_acceleration(
  data_cube = cube_df,
  fun = mean_obs_simple,
  grouping_var = c("year"),
  ref_group = ref_year,
  influence_method = "pos",
  processed_cube = FALSE
)
acceleration_df42 <- calculate_acceleration(
  data_cube = processed_cube,
  fun = mean_obs_simple,
  grouping_var = c("year"),
  ref_group = ref_year,
  influence_method = "usual"
)

example_results <- list(
  acceleration_df1, acceleration_df2, acceleration_df3, acceleration_df4,
  acceleration_df12, acceleration_df22, acceleration_df32, acceleration_df42
)

## Perform tests
# Test calculate_acceleration output
test_that("calculate_acceleration returns a df with expected structure", {
  # Data frame
  lapply(example_results, function(df) {
    expect_s3_class(df, "data.frame")
  })

  # Correct column names
  lapply(example_results[1:4], function(df) {
    expect_true(all(
      c("year", "taxonKey", "acceleration") %in% names(df)
    ))
  })
  lapply(example_results[5:8], function(df) {
    expect_true(all(
      c("year", "acceleration") %in% names(df)
    ))
  })

  # Correct number of columns
  expect_true(nrow(acceleration_df1) == length(species) * length(years))
  expect_true(nrow(acceleration_df2) == length(species) * length(years))
  expect_true(nrow(acceleration_df3) == length(species) * (length(years) - 1))
  expect_true(nrow(acceleration_df4) == length(species) * (length(years) - 1))
})

# Test whether processed cube and dataframe result in the same
test_that("Identical results for processed cube and dataframe", {
  # Input dataframe
  acceleration_df12 <- calculate_acceleration(
    data_cube = cube_df,
    fun = mean_obs,
    grouping_var = c("year", "taxonKey"),
    influence_method = "pos",
    processed_cube = FALSE
  )

  # Input 'processed_cube'
  acceleration_df22 <- calculate_acceleration(
    data_cube = processed_cube,
    fun = mean_obs,
    grouping_var = c("year", "taxonKey"),
    influence_method = "usual"
  )

  # Input dataframe with reference group
  acceleration_df32 <- calculate_acceleration(
    data_cube = cube_df,
    fun = mean_obs,
    grouping_var = c("year", "taxonKey"),
    ref_group = ref_year,
    influence_method = "usual",
    processed_cube = FALSE
  )

  # Input 'processed_cube' with reference group
  acceleration_df42 <- calculate_acceleration(
    data_cube = processed_cube,
    fun = mean_obs,
    grouping_var = c("year", "taxonKey"),
    ref_group = ref_year,
    influence_method = "pos"
  )

  expect_identical(acceleration_df2, acceleration_df12)
  expect_identical(acceleration_df1, acceleration_df22)
  expect_identical(acceleration_df4, acceleration_df32)
  expect_identical(acceleration_df3, acceleration_df42)
})
