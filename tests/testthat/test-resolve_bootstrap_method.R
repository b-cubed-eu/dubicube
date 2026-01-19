# Group-specific indicator: each group is independent
fun_group_specific <- function(x) {
  out <- aggregate(Sepal.Length ~ Species, x, mean)
  names(out) <- c("Species", "diversity_val")
  out
}

# Whole-cube indicator: result per group depends on all groups
fun_whole_cube <- function(x) {
  out <- aggregate(Sepal.Length ~ Species, x, mean)
  out$Sepal.Length <- out$Sepal.Length / nrow(out)
  names(out) <- c("Species", "diversity_val")
  out
}

test_that("resolve_bootstrap_method returns expected method variants", {
  # boot_group_specific
  expect_equal(
    resolve_bootstrap_method(
      df = iris,
      fun = fun_group_specific,
      cat_var = "Species",
      ref_group = NA,
      method = "smart"
    ),
    "boot_group_specific"
  )

  # group_specific
  expect_equal(
    resolve_bootstrap_method(
      df = iris,
      fun = fun_group_specific,
      cat_var = "Species",
      ref_group = "setosa",
      method = "smart"
    ),
    "group_specific"
  )

  # boot_whole_cube
  expect_equal(
    resolve_bootstrap_method(
      df = iris,
      fun = fun_whole_cube,
      cat_var = "Species",
      ref_group = NA,
      method = "smart"
    ),
    "boot_whole_cube"
  )

  # whole_cube
  expect_equal(
    resolve_bootstrap_method(
      df = iris,
      fun = fun_whole_cube,
      cat_var = "Species",
      ref_group = "setosa",
      method = "smart"
    ),
    "whole_cube"
  )
})

test_that("resolve_bootstrap_method throws errors for invalid inputs", {
  # df not a dataframe
  expect_error(
    resolve_bootstrap_method(
      df = list(a = 1),
      fun = fun_group_specific,
      cat_var = "Species",
      ref_group = NA,
      method = "smart"
    ),
    "`df` must be a dataframe"
  )

  # fun not a function
  expect_error(
    resolve_bootstrap_method(
      df = iris,
      fun = 123,
      cat_var = "Species",
      ref_group = NA,
      method = "smart"
    ),
    "`fun` must be a function"
  )

  # cat_var not a character vector
  expect_error(
    resolve_bootstrap_method(
      df = iris,
      fun = fun_group_specific,
      cat_var = 1,
      ref_group = NA,
      method = "smart"
    ),
    "`cat_var` must be a character vector"
  )

  # method not character
  expect_error(
    resolve_bootstrap_method(
      df = iris,
      fun = fun_group_specific,
      cat_var = "Species",
      ref_group = NA,
      method = 123
    ),
    "`method` must be a character string"
  )

  # Illegal combination: boot_* + ref_group
  expect_error(
    resolve_bootstrap_method(
      df = iris,
      fun = fun_group_specific,
      cat_var = "Species",
      ref_group = "setosa",
      method = "boot_group_specific"
    ),
    "Cannot use a 'boot' method when a reference group is specified"
  )
})
