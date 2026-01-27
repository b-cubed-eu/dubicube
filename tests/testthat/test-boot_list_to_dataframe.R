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

test_that("boot_list_to_dataframe returns a tidy data frame", {

  # Create simple boot objects per year
  stat_fun <- function(data, indices) {
    mean(data$obs[indices])
  }

  set.seed(123)
  boot_list <- lapply(
    split(cube_df, cube_df$year),
    function(df) boot::boot(df, statistic = stat_fun, R = 100)
  )

  out_df <- boot_list_to_dataframe(
    boot_list = boot_list,
    grouping_var = "year"
  )

  # Output class
  expect_s3_class(out_df, "data.frame")

  # Required columns
  expect_true(all(c(
    "sample",
    "year",
    "est_original",
    "rep_boot",
    "est_boot",
    "se_boot",
    "bias_boot"
  ) %in% names(out_df)))

  # One row per bootstrap replicate per group
  expect_equal(
    nrow(out_df),
    length(boot_list) * boot_list[[1]]$R
  )

  # Sample indices correct
  expect_setequal(out_df$sample, seq_len(boot_list[[1]]$R))
})

test_that("boot_list_to_dataframe computes bootstrap summaries correctly", {

  stat_fun <- function(data, indices) {
    mean(data$obs[indices])
  }

  set.seed(123)
  boot_list <- lapply(
    split(cube_df, cube_df$year),
    function(df) boot::boot(df, statistic = stat_fun, R = 100)
  )

  out_df <- boot_list_to_dataframe(
    boot_list = boot_list,
    grouping_var = "year"
  )

  # Check statistics per group
  lapply(names(boot_list), function(yr) {
    df <- subset(out_df, year == yr)
    boot <- boot_list[[yr]]

    expect_equal(unique(df$est_original), boot$t0)
    expect_equal(unique(df$est_boot), mean(boot$t))
    expect_equal(unique(df$se_boot), sd(boot$t))
    expect_equal(
      unique(df$bias_boot),
      mean(boot$t) - boot$t0
    )
  })
})

test_that("boot_list_to_dataframe preserves grouping variable values", {

  stat_fun <- function(data, indices) {
    mean(data$obs[indices])
  }

  set.seed(123)
  boot_list <- lapply(
    split(cube_df, cube_df$year),
    function(df) boot::boot(df, statistic = stat_fun, R = 50)
  )

  out_df <- boot_list_to_dataframe(
    boot_list = boot_list,
    grouping_var = "year"
  )

  expect_setequal(
    unique(out_df$year),
    names(boot_list)
  )
})

test_that("boot_list_to_dataframe handles numeric-like group names correctly", {

  stat_fun <- function(data, indices) {
    mean(data$obs[indices])
  }

  set.seed(123)
  boot_list <- lapply(
    split(cube_df, cube_df$year),
    function(df) boot::boot(df, statistic = stat_fun, R = 50)
  )

  out_df <- boot_list_to_dataframe(
    boot_list = boot_list,
    grouping_var = "year"
  )

  # Stored as character (by design)
  expect_true(is.character(out_df$year))
})

test_that("boot_list_to_dataframe rejects invalid input", {

  stat_fun <- function(data, indices) {
    mean(data$obs[indices])
  }

  set.seed(123)
  boot_obj <- boot::boot(cube_df, statistic = stat_fun, R = 10)

  # Not a list
  expect_error(
    boot_list_to_dataframe(boot_obj, "year"),
    "`boot_list` must be a named list of `boot` objects.",
    fixed = TRUE
  )

  # List but not boot objects
  expect_error(
    boot_list_to_dataframe(list(a = 1, b = 2), "year"),
    "`boot_list` must be a named list of `boot` objects.",
    fixed = TRUE
  )

  # Unnamed list
  expect_error(
    boot_list_to_dataframe(
      list(boot_obj, boot_obj),
      "year"
    ),
    "must be a named list",
    fixed = TRUE
  )

  # Invalid grouping_var
  expect_error(
    boot_list_to_dataframe(
      list(year1 = boot_obj),
      grouping_var = c("year", "month")
    ),
    "`grouping_var` must be a scalar character vector.",
    fixed = TRUE
  )
})
