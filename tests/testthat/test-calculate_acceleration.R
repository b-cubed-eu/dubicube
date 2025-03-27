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
  obs = rpois(5, 50))

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

mean_obs_processed <- function(data) {
  # Initiate output variable
  out_df <- NULL
  out_df$meta <- "Mean number of observations per year"
  # Calculate mean obs per year
  out_df$data <- aggregate(obs ~ year + taxonKey, data$data, mean)
  # Rename columns
  names(out_df$data) <- c("year", "taxonKey", "diversity_val")

  return(out_df)
}

## Calculate acceleration
# Input dataframe
acceleration_df1 <- calculate_acceleration(
  data_cube = cube_df,
  fun = mean_obs,
  grouping_var = c("year", "taxonKey"),
  influence_method = "usual")

# Input 'processed_cube'
acceleration_df2 <- calculate_acceleration(
  data_cube = processed_cube,
  fun = mean_obs_processed,
  grouping_var = c("year", "taxonKey"),
  influence_method = "pos")

# Input dataframe with reference group
acceleration_df3 <- calculate_acceleration(
  data_cube = cube_df,
  fun = mean_obs,
  grouping_var = c("year", "taxonKey"),
  ref_group = ref_year,
  influence_method = "pos")

# Input 'processed_cube' with reference group
acceleration_df4 <- calculate_acceleration(
  data_cube = processed_cube,
  fun = mean_obs_processed,
  grouping_var = c("year", "taxonKey"),
  ref_group = ref_year,
  influence_method = "usual")
