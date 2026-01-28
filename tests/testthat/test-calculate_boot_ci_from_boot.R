# Load required package
suppressWarnings(library(boot))

# We bootstrap the mean of the 'mpg' variable in the mtcars dataset
set.seed(123)
boot_obj <- boot(
  data = mtcars$mpg,
  statistic = function(data, indices) mean(data[indices]),
  R = 200
)

test_that("calculate_boot_ci_from_boot returns expected structure", {
  # Calculate multiple CI types
  res <- calculate_boot_ci_from_boot(
    boot_obj = boot_obj,
    type = c("norm", "basic", "perc"),
    conf = 0.95
  )

  # Output should be a dataframe
  expect_s3_class(res, "data.frame")

  # Check presence of required columns
  expect_true(all(c(
    "stat_index",
    "est_original",
    "int_type",
    "ll",
    "ul",
    "conf"
  ) %in% names(res)))

  # Interval types should match requested ones
  expect_true(all(res$int_type %in% c("norm", "basic", "perc")))

  # Confidence level should be propagated correctly
  expect_true(all(res$conf == 0.95))
})


test_that("type = 'all' returns all supported interval types", {
  # Request all available confidence interval types
  res <- calculate_boot_ci_from_boot(
    boot_obj = boot_obj,
    type = "all"
  )

  # All four CI types should be present
  expect_setequal(
    unique(res$int_type),
    c("norm", "basic", "perc", "bca")
  )
})


test_that("function handles multiple statistics correctly", {
  # Define a statistic that returns two values (mean and SD)
  stat_fun <- function(data, indices) {
    x <- data[indices]
    c(mean = mean(x), sd = sd(x))
  }

  # Bootstrap object with two statistics
  set.seed(123)
  boot_obj_multi <- boot(
    data = mtcars$mpg,
    statistic = stat_fun,
    R = 200
  )

  res <- calculate_boot_ci_from_boot(
    boot_obj = boot_obj_multi,
    type = c("norm", "perc")
  )

  # There should be two distinct statistic indices
  expect_true(all(res$stat_index %in% c(1, 2)))
  expect_equal(length(unique(res$stat_index)), 2)
})


test_that("confidence interval bounds are finite and ordered", {
  res <- calculate_boot_ci_from_boot(
    boot_obj = boot_obj,
    type = c("norm", "basic", "perc")
  )

  # Lower and upper limits should be finite numbers
  expect_true(all(is.finite(res$ll)))
  expect_true(all(is.finite(res$ul)))

  # Lower limits should never exceed upper limits
  expect_true(all(res$ll <= res$ul))
})


test_that("conf argument is correctly propagated to output", {
  res <- calculate_boot_ci_from_boot(
    boot_obj = boot_obj,
    type = "perc",
    conf = 0.9
  )

  # All rows should report the requested confidence level
  expect_true(all(res$conf == 0.9))
})


test_that("non-boot input fails with an error", {
  # Passing a non-boot object should error
  expect_error(
    calculate_boot_ci_from_boot(
      boot_obj = mtcars,
      type = "perc"
    )
  )
})
