## Create example data
# Example dataset 1: Simple case with a single threshold
example_df1 <- data.frame(
  mean = c(0, 0.5, -0.5, 1, -1, 1.5, -1.5),
  sd = c(1, 0.5, 0.5, 0.5, 0.5, 0.25, 0.25)
)
example_df1$lcl <- qnorm(0.05, example_df1$mean, example_df1$sd)
example_df1$ucl <- qnorm(0.95, example_df1$mean, example_df1$sd)

# Apply function
result1 <- add_effect_classification(
  df = example_df1,
  cl_columns = c("lcl", "ucl"),
  threshold = 1,
  reference = 0,
  coarse = TRUE
)

result2 <- add_effect_classification(
  df = example_df1,
  cl_columns = c("lcl", "ucl"),
  threshold = 1,
  reference = 0,
  coarse = FALSE
)

# Example dataset 2: More complex case with two thresholds
example_df2 <- data.frame(
  mean = c(0, 0.75, -0.75, 1.25, -1.25, 2, -2),
  sd = c(0.8, 0.6, 0.6, 0.4, 0.4, 0.3, 0.3)
)
example_df2$lcl <- qnorm(0.05, example_df2$mean, example_df2$sd)
example_df2$ucl <- qnorm(0.95, example_df2$mean, example_df2$sd)

# Apply function
result3 <- add_effect_classification(
  df = example_df2,
  cl_columns = c("lcl", "ucl"),
  threshold = c(0.5, 1.5),
  reference = 1,
  coarse = TRUE
)

result4 <- add_effect_classification(
  df = example_df2,
  cl_columns = c("lcl", "ucl"),
  threshold = c(0.5, 1.5),
  reference = 1,
  coarse = FALSE
)

results <- list(result1, result2, result3, result4)

## Perform tests
# Test add_effect_classification output
test_that("function returns a dataframe with expected structure", {
  lapply(results, function(df) {
    expect_s3_class(df, "data.frame")
  })

  expect_true(all(c("mean", "sd", "lcl", "ucl", "effect_code",
                    "effect_code_coarse", "effect", "effect_coarse") %in%
                    names(result1)))
  expect_true(all(c("mean", "sd", "lcl", "ucl", "effect_code",
                    "effect_code_coarse", "effect", "effect_coarse") %in%
                    names(result3)))

  expect_true(all(c("mean", "sd", "lcl", "ucl", "effect_code", "effect") %in%
                    names(result2)))
  expect_true(all(c("mean", "sd", "lcl", "ucl", "effect_code", "effect") %in%
                    names(result4)))
})

# Test that effect classification produces reasonable values
test_that("function computes effects correctly", {
  # Check if effect columns ordered factors
  lapply(results, function(df) {
    expect_true(is.ordered(df$effect_code))
  })
  lapply(results, function(df) {
    expect_true(is.ordered(df$effect))
  })

  lapply(list(result1, result3), function(df) {
    expect_true(is.ordered(df$effect_code_coarse))
  })
  lapply(list(result1, result3), function(df) {
    expect_true(is.ordered(df$effect_coarse))
  })

  # Check levels of effect columns
  fine_levels <- c(
    "strong increase", "increase", "moderate increase", "stable",
    "moderate decrease", "decrease", "strong decrease",
    "potential increase", "potential decrease", "unknown"
  )
  coarse_levels <- c(
    "increase", "stable", "decrease", "unknown"
  )

  lapply(results, function(df) {
    expect_equal(fine_levels, levels(df$effect))
  })

  lapply(list(result1, result3), function(df) {
    expect_equal(coarse_levels, levels(df$effect_coarse))
  })
})

# Test handling of invalid input
test_that("bootstrap_cube handles invalid inputs gracefully", {
  expect_error(
    add_effect_classification(
      df = example_df2,
      cl_columns = c("ll", "ul"),
      threshold = c(0.5, 1.5),
      reference = 1,
      coarse = FALSE),
    "`cl_columns` columns are not present in `df`.",
    fixed = TRUE)

  expect_error(
    add_effect_classification(
      df = example_df2,
      cl_columns = c("lcl", "ucl"),
      threshold = 1:3,
      reference = 1,
      coarse = FALSE),
    "`threshold` must be a numeric vector of length 1 or 2.",
    fixed = TRUE)

  expect_error(
    add_effect_classification(
      df = example_df2,
      cl_columns = c("lcl", "ucl"),
      threshold = c(0.5, 1.5),
      reference = "1",
      coarse = FALSE),
    "`reference` must be a numeric vector of length 1.",
    fixed = TRUE)

  expect_error(
    add_effect_classification(
      df = example_df2,
      cl_columns = c("lcl", "ucl"),
      threshold = c(0.5, 1.5),
      reference = 1,
      coarse = "FALSE"),
    "`coarse` must be a logical vector of length 1.",
    fixed = TRUE)
})
