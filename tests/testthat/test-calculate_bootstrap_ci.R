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
boot_df1 <- bootstrap_cube(
  data_cube = cube_df,
  fun = mean_obs,
  grouping_var = "year",
  samples = 1000,
  seed = 123
)

# Perform bootstrapping 'processed_cube'
boot_df2 <- bootstrap_cube(
  data_cube = processed_cube,
  fun = mean_obs_processed,
  grouping_var = "year",
  samples = 1000,
  seed = 123
)

# Perform bootstrapping dataframe with reference group
boot_df3 <- bootstrap_cube(
  data_cube = cube_df,
  fun = mean_obs,
  grouping_var = "year",
  samples = 1000,
  seed = 123,
  ref_group = ref_year
)

# Perform bootstrapping 'processed_cube' with reference group
boot_df4 <- bootstrap_cube(
  data_cube = processed_cube,
  fun = mean_obs_processed,
  grouping_var = "year",
  samples = 1000,
  seed = 123,
  ref_group = ref_year
)

## Calculate confidence intervals
# Percentile
result_perc1 <- calculate_bootstrap_ci(
  bootstrap_samples_df = boot_df1,
  grouping_var = "year",
  type = "perc",
  conf = 0.95,
  aggregate = TRUE)

# BCa with dataframe without reference group
result_bca1 <- calculate_bootstrap_ci(
  bootstrap_samples_df = boot_df1,
  grouping_var = "year",
  type = "bca",
  conf = 0.95,
  aggregate = TRUE,
  data_cube = cube_df,
  fun = mean_obs,
  ref_group = NA,
  jackknife = "pos")

# BCa with dataframe with reference group
result_bca2 <- calculate_bootstrap_ci(
  bootstrap_samples_df = boot_df3,
  grouping_var = "year",
  type = "bca",
  conf = 0.95,
  aggregate = TRUE,
  data_cube = cube_df,
  fun = mean_obs,
  ref_group = ref_year,
  jackknife = "usual")

# BCa with 'processed_cube' without reference group
result_bca3 <- calculate_bootstrap_ci(
  bootstrap_samples_df = boot_df2,
  grouping_var = "year",
  type = "bca",
  conf = 0.95,
  aggregate = TRUE,
  data_cube = processed_cube,
  fun = mean_obs_processed,
  ref_group = NA,
  jackknife = "usual")

# BCa with 'processed_cube' with reference group
result_bca4 <- calculate_bootstrap_ci(
  bootstrap_samples_df = boot_df4,
  grouping_var = "year",
  type = "bca",
  conf = 0.95,
  aggregate = TRUE,
  data_cube = processed_cube,
  fun = mean_obs_processed,
  ref_group = ref_year,
  jackknife = "usual")

# Normal
result_norm1 <- calculate_bootstrap_ci(
  bootstrap_samples_df = boot_df1,
  grouping_var = "year",
  type = "norm",
  conf = 0.95,
  aggregate = TRUE)

# Basic
result_basic1 <- calculate_bootstrap_ci(
  bootstrap_samples_df = boot_df1,
  grouping_var = "year",
  type = "basic",
  conf = 0.95,
  aggregate = TRUE)

# All with dataframe without reference group
result_all1 <- calculate_bootstrap_ci(
  bootstrap_samples_df = boot_df1,
  grouping_var = "year",
  type = "all",
  conf = 0.95,
  aggregate = TRUE,
  data_cube = cube_df,
  fun = mean_obs,
  ref_group = NA,
  jackknife = "pos")

## Perform tests
