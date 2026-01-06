## Reuse example data
years <- 2014:2020
ref_year <- 2020
grid_cells <- c("E003N55BA", "E003N55BB", "E003N55BC")
species <- paste0("spec", 1:3)

set.seed(123)

cube_df <- expand.grid(
  year = years,
  cellCode = grid_cells,
  taxonKey = species,
  obs = rpois(5, 50)
)

mean_obs <- function(data) {
  out_df <- aggregate(obs ~ year + taxonKey, data, mean)
  names(out_df) <- c("year", "taxonKey", "diversity_val")
  out_df
}

test_that("bootstrap_cube performs whole-cube bootstrapping", {

  res <- bootstrap_cube(
    data_cube = cube_df,
    fun = mean_obs,
    grouping_var = c("year", "taxonKey"),
    samples = 5,
    seed = 123,
    processed_cube = FALSE,
    method = "whole_cube"
  )

  expect_s3_class(res, "data.frame")
  expect_true("method_boot" %in% names(res))
  expect_true(all(res$method_boot == "whole_cube"))
})

test_that("bootstrap_cube performs group-specific bootstrapping", {

  res <- bootstrap_cube(
    data_cube = cube_df,
    fun = mean_obs,
    grouping_var = "taxonKey",
    samples = 5,
    seed = 123,
    processed_cube = FALSE,
    method = "group_specific"
  )

  expect_s3_class(res, "data.frame")
  expect_true(all(res$method_boot == "group_specific"))
})

test_that("bootstrap_cube handles reference group correctly", {

  res <- bootstrap_cube(
    data_cube = cube_df,
    fun = mean_obs,
    grouping_var = c("year", "taxonKey"),
    ref_group = ref_year,
    samples = 5,
    seed = 123,
    processed_cube = FALSE,
    method = "whole_cube"
  )

  expect_false(any(res$year == ref_year))
})

test_that("bootstrap_cube smart method runs and sets method_boot", {

  res <- bootstrap_cube(
    data_cube = cube_df,
    fun = mean_obs,
    grouping_var = "taxonKey",  # <- only one group
    samples = 5,
    seed = 123,
    processed_cube = FALSE,
    method = "smart"
  )

  expect_true("method_boot" %in% names(res))
  expect_true(all(res$method_boot %in% c("whole_cube", "group_specific")))
})

test_that("bootstrap_cube detects redundant grouping variables", {

  cube_df2 <- cube_df
  cube_df2$time_point <- match(cube_df2$year, sort(unique(cube_df2$year)))

  expect_error(
    bootstrap_cube(
      data_cube = cube_df2,
      fun = mean_obs,
      grouping_var = c("year", "time_point", "taxonKey"),
      samples = 5,
      seed = 123,
      processed_cube = FALSE,
      method = "whole_cube"
    )
  )
})

test_that("bootstrap_cube handles invalid inputs", {

  expect_error(
    bootstrap_cube(
      data_cube = cube_df,
      fun = mean_obs,
      grouping_var = "year",
      processed_cube = FALSE,
      method = "invalid_method"
    )
  )

  expect_error(
    bootstrap_cube(
      data_cube = cube_df,
      fun = mean_obs,
      grouping_var = c("year", "taxonKey"),
      processed_cube = FALSE,
      method = "group_specific"
    ),
    "requires exactly one grouping variable"
  )
})
