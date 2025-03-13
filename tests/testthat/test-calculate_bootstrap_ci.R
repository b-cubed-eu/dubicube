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
  obs = rpois(20, 50))

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

## Perform bootstrapping
# Perform bootstrapping dataframe
boot_df1 <- bootstrap_cube(
  data_cube = cube_df,
  fun = mean_obs,
  grouping_var = c("year", "taxonKey"),
  samples = 1000,
  seed = 123
)

# Perform bootstrapping 'processed_cube'
boot_df2 <- bootstrap_cube(
  data_cube = processed_cube,
  fun = mean_obs_processed,
  grouping_var = c("year", "taxonKey"),
  samples = 1000,
  seed = 123
)

# Perform bootstrapping dataframe with reference group
boot_df3 <- bootstrap_cube(
  data_cube = cube_df,
  fun = mean_obs,
  grouping_var = c("year", "taxonKey"),
  samples = 1000,
  seed = 123,
  ref_group = ref_year
)

# Perform bootstrapping 'processed_cube' with reference group
boot_df4 <- bootstrap_cube(
  data_cube = processed_cube,
  fun = mean_obs_processed,
  grouping_var = c("year", "taxonKey"),
  samples = 1000,
  seed = 123,
  ref_group = ref_year
)

## Calculate confidence intervals
# Percentile
result_perc1 <- calculate_bootstrap_ci(
  bootstrap_samples_df = boot_df1,
  grouping_var = c("year", "taxonKey"),
  type = "perc",
  conf = 0.95,
  aggregate = TRUE)

# BCa with dataframe without reference group
result_bca1 <- calculate_bootstrap_ci(
  bootstrap_samples_df = boot_df1,
  grouping_var = c("year", "taxonKey"),
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
  grouping_var = c("year", "taxonKey"),
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
  grouping_var = c("year", "taxonKey"),
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
  grouping_var = c("year", "taxonKey"),
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
  grouping_var = c("year", "taxonKey"),
  type = "norm",
  conf = 0.95,
  aggregate = TRUE)

# Basic
result_basic1 <- calculate_bootstrap_ci(
  bootstrap_samples_df = boot_df1,
  grouping_var = c("year", "taxonKey"),
  type = "basic",
  conf = 0.95,
  aggregate = TRUE)

# All with dataframe without reference group
result_all1 <- calculate_bootstrap_ci(
  bootstrap_samples_df = boot_df1,
  grouping_var = c("year", "taxonKey"),
  type = "all",
  conf = 0.95,
  aggregate = TRUE,
  data_cube = cube_df,
  fun = mean_obs,
  ref_group = NA,
  jackknife = "pos")

example_ci_results_noref <- list(
  result_perc1, result_bca1, result_bca3,
  result_norm1, result_basic1, result_all1)

example_ci_results_ref <- list(result_bca2, result_bca4)

example_ci_results <- c(example_ci_results_noref, example_ci_results_ref)

# Without aggregation
result_perc2 <- calculate_bootstrap_ci(
  bootstrap_samples_df = boot_df1,
  grouping_var = c("year", "taxonKey"),
  type = "perc",
  conf = 0.95,
  aggregate = FALSE)

## Perform tests
# Test calculate_bootstrap_ci output
test_that("calculate_bootstrap_ci returns a df with expected structure", {
  # Data frame
  lapply(c(example_ci_results, list(result_perc2)), function(df) {
    expect_s3_class(df, "data.frame")
  })

  # Correct column names
  # Total
  lapply(c(example_ci_results, list(result_perc2)), function(df) {
    expect_true(all(
      c("year", "est_original", "est_boot", "se_boot", "bias_boot",
        "int_type", "ll", "ul", "conf") %in% names(df)
      ))
  })
  # Not aggregated
  expect_true(all(
    c("sample", "year", "est_original", "rep_boot", "est_boot", "se_boot",
      "bias_boot", "int_type", "ll", "ul", "conf") %in% names(result_perc2)
  ))
})

# Test whether calculate_bootstrap_ci produces reasonable values
test_that("calculate_bootstrap_ci computes values correctly", {
  # Check values
  years <- unique(cube_df$year)
  years_ref <- setdiff(years, ref_year)

  lapply(c(example_ci_results, list(result_perc2)), function(df) {
    expect_true(all(df$se_boot >= 0))
  })
  lapply(c(example_ci_results, list(result_perc2)), function(df) {
    expect_true(all(df$conf == 0.95))
  })
  lapply(example_ci_results_noref, function(df) {
    expect_true(all(df$year %in% years))
  })
  lapply(example_ci_results_ref, function(df) {
    expect_true(all(df$year %in% years_ref))
  })

  # Check int_type
  expect_true(all(result_perc1$int_type == "perc"))
  expect_true(all(result_perc2$int_type == "perc"))
  expect_true(all(result_bca1$int_type == "bca"))
  expect_true(all(result_bca2$int_type == "bca"))
  expect_true(all(result_bca3$int_type == "bca"))
  expect_true(all(result_bca4$int_type == "bca"))
  expect_true(all(result_norm1$int_type == "norm"))
  expect_true(all(result_basic1$int_type == "basic"))

  result_test <- calculate_bootstrap_ci(
    bootstrap_samples_df = boot_df1,
    grouping_var = c("year", "taxonKey"),
    type = c("perc", "norm"),
    conf = 0.95,
    aggregate = TRUE)

  expect_identical(sort(unique(result_test$int_type)), sort(c("perc", "norm")))
  expect_identical(sort(unique(result_all1$int_type)),
                   sort(c("perc", "bca", "norm", "basic")))

})

# Test whether processed cube and dataframe result in the same
test_that("Identical results for processed cube and dataframe", {
  result_bca12 <- calculate_bootstrap_ci(
    bootstrap_samples_df = boot_df1,
    grouping_var = c("year", "taxonKey"),
    type = "bca",
    conf = 0.95,
    aggregate = TRUE,
    data_cube = cube_df,
    fun = mean_obs,
    ref_group = NA,
    jackknife = "usual")

  result_bca32 <- calculate_bootstrap_ci(
    bootstrap_samples_df = boot_df2,
    grouping_var = c("year", "taxonKey"),
    type = "bca",
    conf = 0.95,
    aggregate = TRUE,
    data_cube = processed_cube,
    fun = mean_obs_processed,
    ref_group = NA,
    jackknife = "pos")

  expect_identical(result_bca12, result_bca3)
  expect_identical(result_bca1, result_bca32)
  expect_identical(result_bca2, result_bca4)

  result_all2 <- calculate_bootstrap_ci(
    bootstrap_samples_df = boot_df2,
    grouping_var = c("year", "taxonKey"),
    type = "all",
    conf = 0.95,
    aggregate = TRUE,
    data_cube = processed_cube,
    fun = mean_obs_processed,
    ref_group = NA,
    jackknife = "pos")

  expect_identical(result_all1, result_all2)
})

# Smaller confidence intervals
test_that("Confidence intervals are smaller with smaller conf argument", {
  result_all3 <- calculate_bootstrap_ci(
    bootstrap_samples_df = boot_df1,
    grouping_var = c("year", "taxonKey"),
    type = "all",
    conf = 0.9,
    aggregate = TRUE,
    data_cube = cube_df,
    fun = mean_obs,
    ref_group = NA,
    jackknife = "pos")

  expect_true(all(result_all1$ll < result_all3$ll))
  expect_true(all(result_all1$ul > result_all3$ul))
})

# Test handling of invalid input
test_that("calculate_bootstrap_ci handles invalid inputs gracefully", {
  expect_error(
    calculate_bootstrap_ci(
      bootstrap_samples_df = boot_df1,
      grouping_var = c("year", "taxonKey"),
      type = c("perc", "normal"),
      conf = 0.95,
      aggregate = TRUE),
    "`type` must be one of 'perc', 'bca', 'norm', 'basic'.",
    fixed = TRUE)

  expect_error(
    calculate_bootstrap_ci(
      bootstrap_samples_df = boot_df1,
      grouping_var = c("year", "taxonKey"),
      type = c("perc", "norm"),
      conf = 0.95,
      aggregate = "TRUE"),
    "`aggregate` must be a logical vector of length 1.",
    fixed = TRUE)

  expect_error(
    calculate_bootstrap_ci(
      bootstrap_samples_df = boot_df1,
      grouping_var = c("year", "taxonKey"),
      type = c("perc", "norm"),
      conf = 1.5,
      aggregate = TRUE),
    "`conf` must be a numeric value between 0 and 1.",
    fixed = TRUE)

  expect_error(
    calculate_bootstrap_ci(
      bootstrap_samples_df = boot_df1,
      grouping_var = c("year", "taxonKey"),
      type = c("perc", "bca"),
      conf = 0.95,
      aggregate = TRUE),
    "`data_cube` and `fun` must be provided to calculate BCa interval.",
    fixed = TRUE)

  expect_error(
    calculate_bootstrap_ci(
      bootstrap_samples_df = boot_df1,
      grouping_var = "month",
      type = "perc",
      conf = 0.95,
      aggregate = TRUE),
    paste("`bootstrap_samples_df` should contain columns: 'rep_boot',",
          "'est_original' and `grouping_var`."),
    fixed = TRUE)

  expect_error(
    calculate_bootstrap_ci(
      bootstrap_samples_df = boot_df3,
      grouping_var = c("year", "taxonKey"),
      type = "bca",
      conf = 0.95,
      aggregate = TRUE,
      data_cube = cube_df,
      fun = mean_obs,
      ref_group = "twothousandtwenty",
      jackknife = "usual"),
    "`ref_group` is not present in `grouping_var` column of `data_cube`.",
    fixed = TRUE)

  expect_error(
    calculate_bootstrap_ci(
      bootstrap_samples_df = boot_df3,
      grouping_var = c("year", "taxonKey"),
      type = "bca",
      conf = 0.95,
      aggregate = TRUE,
      data_cube = cube_df,
      fun = mean_obs,
      ref_group = "2020",
      jackknife = "negative"),
    "`jackknife` must be one of 'usual', 'pos'.",
    fixed = TRUE)
})
