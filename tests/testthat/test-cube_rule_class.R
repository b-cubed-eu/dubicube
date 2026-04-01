# ------------------------------------------------------------------------------
# Helper rule used in tests
# ------------------------------------------------------------------------------
valid_rule <- list(
  id = "test_rule",
  dimension = "temporal",
  thresholds = c(ok = 5, note = 3, important = 1),
  compute = function(cube) 1,
  severity = function(value, thresholds) "ok",
  message = function(value, thresholds) "test message"
)

# ------------------------------------------------------------------------------
# Test: validate_cube_rule returns rule when structure is correct
# ------------------------------------------------------------------------------
test_that("validate_cube_rule accepts valid rule structure", {
  validated <- validate_cube_rule(valid_rule)

  expect_identical(validated, valid_rule)
})

# ------------------------------------------------------------------------------
# Test: new_cube_rule attaches cube_rule class
# ------------------------------------------------------------------------------
test_that("new_cube_rule attaches cube_rule class", {
  rule <- new_cube_rule(valid_rule)

  expect_s3_class(rule, "cube_rule")
})

# ------------------------------------------------------------------------------
# Test: new_cube_rule preserves rule contents
# ------------------------------------------------------------------------------
test_that("new_cube_rule does not modify rule contents", {
  rule <- new_cube_rule(valid_rule)

  expect_identical(rule$id, valid_rule$id)
  expect_identical(rule$dimension, valid_rule$dimension)
  expect_identical(rule$thresholds, valid_rule$thresholds)
})

# ------------------------------------------------------------------------------
# Test: validate_cube_rule detects missing elements
# ------------------------------------------------------------------------------
test_that("validate_cube_rule errors when required elements are missing", {
  bad_rule <- valid_rule
  bad_rule$severity <- NULL

  expect_error(
    validate_cube_rule(bad_rule),
    "Missing rule elements"
  )
})

# ------------------------------------------------------------------------------
# Test: multiple missing elements are reported
# ------------------------------------------------------------------------------
test_that("validate_cube_rule reports all missing elements", {
  bad_rule <- valid_rule
  bad_rule$id <- NULL
  bad_rule$message <- NULL

  expect_error(
    validate_cube_rule(bad_rule),
    "id, message"
  )
})

# ------------------------------------------------------------------------------
# Test: new_cube_rule also validates rule structure
# ------------------------------------------------------------------------------
test_that("new_cube_rule fails if rule structure invalid", {
  bad_rule <- valid_rule
  bad_rule$compute <- NULL

  expect_error(
    new_cube_rule(bad_rule),
    "Missing rule elements"
  )
})
