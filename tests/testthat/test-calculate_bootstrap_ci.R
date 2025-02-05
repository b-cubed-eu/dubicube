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

## Perform bootstrapping
# Perform bootstrapping dataframe
result1 <- bootstrap_cube(
  data_cube = cube_df,
  fun = mean_obs,
  grouping_var = "year",
  samples = 1000,
  seed = 123
)

# Perform bootstrapping 'processed_cube'
result2 <- bootstrap_cube(
  data_cube = processed_cube,
  fun = mean_obs_processed,
  grouping_var = "year",
  samples = 1000,
  seed = 123
)

# Perform bootstrapping dataframe with reference group
result3 <- bootstrap_cube(
  data_cube = cube_df,
  fun = mean_obs,
  grouping_var = "year",
  samples = 1000,
  seed = 123,
  ref_group = ref_year
)

# Perform bootstrapping 'processed_cube' with reference group
result4 <- bootstrap_cube(
  data_cube = processed_cube,
  fun = mean_obs_processed,
  grouping_var = "year",
  samples = 1000,
  seed = 123,
  ref_group = ref_year
)

## Perform tests
