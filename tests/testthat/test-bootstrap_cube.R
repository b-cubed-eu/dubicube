## Example data
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

test_that("non-boot whole-cube and group-specific bootstraps", {
  # whole_cube
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

  # group_specific
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

test_that("bootstrap_cube smart method resolves method_boot", {
  res <- bootstrap_cube(
    data_cube = cube_df,
    fun = mean_obs,
    grouping_var = "taxonKey",
    samples = 5,
    seed = 123,
    processed_cube = FALSE,
    method = "smart"
  )
  expect_true(all(sapply(res, inherits, "boot")))
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
    ),
    "redundant"
  )
})

test_that("bootstrap_cube validates inputs and method", {
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

test_that("bootstrap_cube supports boot::boot() method", {
  # boot_whole_cube
  res <- bootstrap_cube(
    data_cube = cube_df,
    fun = mean_obs,
    grouping_var = c("year", "taxonKey"),
    samples = 5,
    seed = 123,
    processed_cube = FALSE,
    method = "boot_whole_cube"
  )
  expect_s3_class(res, "boot")
  expect_equal(res$R, 5)
  expect_true(nrow(res$t) == 5)

  # boot_group_specific
  res_list <- bootstrap_cube(
    data_cube = cube_df,
    fun = mean_obs,
    grouping_var = "taxonKey",
    samples = 5,
    seed = 123,
    processed_cube = FALSE,
    method = "boot_group_specific"
  )
  expect_type(res_list, "list")
  expect_true(all(sapply(res_list, inherits, "boot")))
  expect_true(all(sapply(res_list, function(x) x$R == 5)))
})

test_that("bootstrap_cube passes boot_args correctly", {
  res <- bootstrap_cube(
    data_cube = cube_df,
    fun = mean_obs,
    grouping_var = c("year", "taxonKey"),
    samples = 5,
    seed = 123,
    processed_cube = FALSE,
    method = "boot_whole_cube",
    boot_args = list(parallel = "no")
  )
  expect_s3_class(res, "boot")
  expect_equal(res$R, 5)
})

test_that("bootstrap_cube boot method respects seed for reproducibility", {
  res1 <- bootstrap_cube(
    data_cube = cube_df,
    fun = mean_obs,
    grouping_var = c("year", "taxonKey"),
    samples = 5,
    seed = 123,
    processed_cube = FALSE,
    method = "boot_whole_cube"
  )
  res2 <- bootstrap_cube(
    data_cube = cube_df,
    fun = mean_obs,
    grouping_var = c("year", "taxonKey"),
    samples = 5,
    seed = 123,
    processed_cube = FALSE,
    method = "boot_whole_cube"
  )
  expect_equal(res1$t, res2$t)
})
