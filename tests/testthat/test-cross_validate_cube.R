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
  taxonKey = species
)
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
  progress = FALSE
)

# Perform LOO CV 'processed_cube'
result2 <- cross_validate_cube(
  data_cube = processed_cube,
  fun = mean_obs_processed,
  grouping_var = "year",
  out_var = "taxonKey",
  crossv_method = "loo",
  progress = FALSE
)

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
  taxonKey = species2
)
cube_df2$obs <- c(obs1, obs2, obs3)

cube_df2 <- rbind(cube_df, cube_df2)

# Create data cube as 'processed_cube'
processed_cube2 <- NULL
processed_cube2$meta <- "This is a processed occurrence cube"
processed_cube2$data <- cube_df2
class(processed_cube2) <- "processed_cube"

# Perform kfold CV dataframe
result3 <- cross_validate_cube(
  data_cube = cube_df2,
  fun = mean_obs,
  grouping_var = "year",
  out_var = "taxonKey",
  crossv_method = "kfold",
  k = 3,
  progress = FALSE
)

# Perform kfold CV 'processed_cube'
result4 <- cross_validate_cube(
  data_cube = processed_cube2,
  fun = mean_obs_processed,
  grouping_var = "year",
  out_var = "taxonKey",
  crossv_method = "kfold",
  k = 3,
  progress = FALSE
)

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
    progress = FALSE
  )

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
  # LOO
  lapply(loo_results, function(df) {
    test_table <- table(expand.grid(year = years, taxonkey_out = species))
    df_table <- table(year = df$year, taxonkey_out = df$taxonkey_out)

    expect_identical(test_table, df_table)
  })
  # K-fold
  lapply(results_ls, function(df) {
    expect_true(length(unique(df$taxonkey_out)) == 3)
  })
})

# Test loo vs. k-fold
test_that("loo and k-fold return same results", {
  # Character vector
  cube_df_char <- cube_df
  cube_df_char$taxonKey <- as.character(cube_df_char$taxonKey)

  # Result with character vector
  result12 <- cross_validate_cube(
    data_cube = cube_df_char,
    fun = mean_obs,
    grouping_var = "year",
    out_var = "taxonKey",
    crossv_method = "loo",
    progress = FALSE
  )

  # Result with k-fold and k = number of categories
  result_kloo2 <- cross_validate_cube(
    data_cube = cube_df_char,
    fun = mean_obs,
    grouping_var = "year",
    out_var = "taxonKey",
    crossv_method = "kfold",
    k = length(species),
    progress = FALSE
  )
  # Adjust sorting and rownames
  result_kloo2 <- result_kloo2[order(result_kloo2[, "year"],
                                     result_kloo2[, "taxonkey_out"]),
  ]
  rownames(result_kloo2) <- seq_len(nrow(result_kloo2))

  expect_identical(result12[, -1], result_kloo2[, -1])
})

# Test handling of invalid input
test_that("cross_validate_cube handles invalid inputs gracefully", {
  # Problems with number of categories
  cat_message <- paste(
    "Number of categories in `out_var` is larger than `max_out_cats`.",
    "Increase the number of `max_out_cats`.", sep = "\n"
  )

  expect_error(
    cross_validate_cube(
      data_cube = cube_df,
      fun = mean_obs,
      grouping_var = "year",
      out_var = "taxonKey",
      crossv_method = "loo",
      progress = FALSE,
      max_out_cats = 2
    ),
    cat_message,
    fixed = TRUE
  )

  expect_error(
    cross_validate_cube(
      data_cube = processed_cube,
      fun = mean_obs_processed,
      grouping_var = "year",
      out_var = "taxonKey",
      crossv_method = "loo",
      progress = FALSE,
      max_out_cats = 2
    ),
    cat_message,
    fixed = TRUE
  )

  # Warning message for large number of categories
  grid <- expand.grid(
    year = years,
    cellCode = grid_cells,
    taxonKey = paste0("spec", 1:1001)
  )
  grid$obs <- 1

  expect_warning(
    cross_validate_cube(
      data_cube = grid,
      fun = mean_obs,
      grouping_var = "year",
      out_var = "taxonKey",
      crossv_method = "loo",
      progress = FALSE,
      max_out_cats = 2000
    ),
    paste("Number of categories in `out_var` is larger than 1000.",
          "Runtime of Cross-Validation may be substantial.", sep = "\n"),
    fixed = TRUE
  )

  # Errors
  expect_error(
    cross_validate_cube(
      data_cube = cube_df,
      fun = mean_obs,
      grouping_var = "year",
      out_var = "taxon_key",
      crossv_method = "loo",
      progress = FALSE
    ),
    "`data_cube` should contain column `out_var`.",
    fixed = TRUE
  )

  expect_error(
    cross_validate_cube(
      data_cube = NULL,
      fun = mean_obs,
      grouping_var = "year",
      out_var = "taxonKey",
      crossv_method = "loo",
      progress = FALSE
    ),
    paste("`data_cube` must be a data cube object (class 'processed_cube' or",
          "'sim_cube') or a dataframe."),
    fixed = TRUE
  )

  expect_error(
    cross_validate_cube(
      data_cube = cube_df,
      fun = mean_obs,
      grouping_var = "year",
      out_var = "taxonKey",
      crossv_method = "LOO",
      progress = FALSE
    ),
    "`crossv_method` must be one of 'loo', 'kfold'.",
    fixed = TRUE
  )

  expect_error(
    cross_validate_cube(
      data_cube = cube_df,
      fun = mean_obs,
      grouping_var = "year",
      out_var = "taxonKey",
      crossv_method = "kfold",
      k = 7,
      progress = FALSE
    ),
    "`k` must be smaller than the number of categories in `out_var`.",
    fixed = TRUE
  )
})

# Test grouping variable
test_that("grouping_var length > 1", {
  # Function that add extra column
  mean_obs2 <- function(data) {
    out_df <- mean_obs(data)
    out_df$id <- seq_len(nrow(out_df))

    return(out_df)
  }
  # Hack to avoid error
  cube_df$id <- seq_len(nrow(cube_df))

  result <- cross_validate_cube(
    data_cube = cube_df,
    fun = mean_obs2,
    grouping_var = c("year", "id"),
    out_var = "taxonKey",
    crossv_method = "loo",
    progress = FALSE
  )

  # Data frame
  expect_s3_class(result, "data.frame")

  # Correct column names
  expect_true(all(
    c("id_cv", "year", "id", "taxonkey_out", "rep_cv", "est_original",
      "error", "sq_error", "abs_error", "rel_error", "perc_error",
      "mre", "mse", "rmse") %in% names(result)
  ))
})
