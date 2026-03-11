# Example cube used in tests
cube <- list(
  data = data.frame(
    obs = c(5, 2, 10, 1),
    year = c(2001, 2001, 2002, 2005),
    minCoordinateUncertaintyInMeters = c(50, 2000, NA, 10)
  ),
  resolutions = 1000
)
class(cube) <- "processed_cube"

# Filtering based on a rule
filtered_cube1 <- suppressWarnings(
  filter_cube(
    cube,
    rules = list(rule_spatial_miss_uncertainty())
  )
)

# Diagnostics object used for filtering
diag <- diagnose_cube(
  cube,
  rules = list(
    rule_spatial_miss_uncertainty(),
    rule_temporal_min_years()
  ),
  verbose = FALSE
)

filtered_cube2 <- suppressWarnings(
  filter_cube(
    cube,
    diagnostics = diag
  )
)

# Test: output class
test_that("filter_cube returns a processed_cube", {
  expect_s3_class(filtered_cube1, "processed_cube")
  expect_s3_class(filtered_cube2, "processed_cube")
})

# Test: correct rows are filtered
test_that("rows with missing coordinate uncertainty are removed", {
  expect_identical(
    filtered_cube1$data,
    cube$data[!is.na(cube$data$minCoordinateUncertaintyInMeters), ]
  )
  expect_identical(
    filtered_cube2$data,
    cube$data[!is.na(cube$data$minCoordinateUncertaintyInMeters), ]
  )
})

# Test: filtering via diagnostics produces identical result
test_that("filtering using diagnostics equals filtering using rules", {
  expect_identical(
    filtered_cube1$data,
    filtered_cube2$data
  )
})

# Test: rules without filter_fn are ignored
test_that("rules without filter_fn are ignored", {
  # Use rule that has no filter function
  filtered <- suppressWarnings(
    filter_cube(
      cube,
      rules = list(rule_temporal_min_years())
    )
  )

  expect_identical(filtered$data, cube$data)
})

# Test: rules argument ignored when diagnostics supplied
test_that("rules argument is ignored when diagnostics is supplied", {
  filtered <- suppressWarnings(
    filter_cube(
      cube,
      rules = list(rule_temporal_min_years()),  # should be ignored
      diagnostics = diag
    )
  )

  expect_identical(filtered$data, filtered_cube1$data)
  expect_identical(filtered$data, filtered_cube2$data)
})

# Test: filter_fn must return logical vector of correct length
test_that("filter_fn must return logical vector with correct length", {
  bad_rule <- list(
    filter_fn = function(cube) c(TRUE, FALSE)
  )
  class(bad_rule) <- "cube_rule"

  expect_error(
    filter_cube(
      cube,
      rules = list(bad_rule)
    ),
    "must return a logical vector"
  )
})

# Test: fallback behaviour triggers warning when cube metadata not rebuilt
test_that("warning is issued when cube metadata cannot be recomputed", {
  expect_warning(
    filter_cube(
      cube,
      rules = list(rule_spatial_miss_uncertainty())
    ),
    "metadata was not recomputed"
  )
  expect_warning(
    filter_cube(
      cube,
      diagnostics = diag
    ),
    "metadata was not recomputed"
  )
})
