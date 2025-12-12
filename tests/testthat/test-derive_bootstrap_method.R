# Helper functions used in the examples
sepal_length_per_species <- function(x, f) {
  out_df <- aggregate(Sepal.Length ~ Species, x, f)
  names(out_df) <- c("Species", "diversity_val")
  out_df
}

sepal_length_per_species2 <- function(x, f) {
  out_df <- aggregate(Sepal.Length ~ Species, x, f)
  out_df$Sepal.Length <- out_df$Sepal.Length / nrow(out_df)
  names(out_df) <- c("Species", "diversity_val")
  out_df
}

# Test expected results
test_that("detects group-specific statistic correctly", {
  expect_identical(
    derive_bootstrap_method(
      df = iris,
      fun = sepal_length_per_species,
      cat_var = "Species",
      min_cat = 1,
      max_cat = 3,
      mean
    ),
    "group_specific"
  )
})

test_that("detects whole-cube statistic correctly", {
  expect_identical(
    derive_bootstrap_method(
      df = iris,
      fun = sepal_length_per_species2,
      cat_var = "Species",
      min_cat = 1,
      max_cat = 3,
      mean
    ),
    "whole_cube"
  )
})

# Test correct input
test_that("errors on wrong df input", {
  expect_error(
    derive_bootstrap_method(df = 5, fun = sepal_length_per_species,
                            cat_var = "Species"),
    "`df` must be a dataframe"
  )
})

test_that("errors on non-function fun", {
  expect_error(
    derive_bootstrap_method(df = iris, fun = 5, cat_var = "Species"),
    "`fun` must be a function"
  )
})

test_that("errors when cat_var missing from data", {
  expect_error(
    derive_bootstrap_method(df = iris, fun = sepal_length_per_species,
                            cat_var = "missing"),
    "should contain column"
  )
})

test_that("min_cat and max_cat must be positive integers", {
  expect_error(
    derive_bootstrap_method(df = iris, fun = sepal_length_per_species,
                            cat_var = "Species", min_cat = 0, max_cat = 3),
    "must be a single positive integer"
  )
  expect_error(
    derive_bootstrap_method(df = iris, fun = sepal_length_per_species,
                            cat_var = "Species", min_cat = 2, max_cat = 1),
    "min. number of categories must be smaller"
  )
})

test_that("index must be positive or -1", {
  expect_error(
    derive_bootstrap_method(df = iris, fun = sepal_length_per_species,
                            cat_var = "Species", index = -5),
    "`index` must be a single positive integer or -1"
  )
})

# Test specific cases
test_that("returns whole_cube when only one category exists", {
  df <- subset(iris, Species == "setosa")
  expect_identical(
    derive_bootstrap_method(df = df,
                            fun = sepal_length_per_species,
                            cat_var = "Species"),
    "whole_cube"
  )
})

test_that("handles cases where max_cat exceeds available categories", {
  # iris has 3 species, set max_cat = 10, should be reduced to n_cat internally
  expect_identical(
    derive_bootstrap_method(
      df = iris,
      fun = sepal_length_per_species,
      cat_var = "Species",
      min_cat = 1,
      max_cat = 10,
      mean
    ),
    "group_specific"
  )
})

test_that("custom index works and returns expected method", {
  # index = 1 means use categories 1:max_cat starting at position 1
  expect_identical(
    derive_bootstrap_method(
      df = iris,
      fun = sepal_length_per_species,
      cat_var = "Species",
      min_cat = 1,
      max_cat = 2,
      index = 1,
      mean
    ),
    "group_specific"
  )
})
