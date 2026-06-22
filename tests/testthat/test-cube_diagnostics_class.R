# ------------------------------------------------------------------------------
# Helper diagnostics table used in tests
# ------------------------------------------------------------------------------
valid_diag <- data.frame(
  dimension = c("spatial", "temporal"),
  metric = c("rule_a", "rule_b"),
  value = c(10, 5),
  severity = c("ok", "note"),
  message = c("All good", "Some warning"),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# Test: validate_cube_diagnostics accepts valid structure
# ------------------------------------------------------------------------------
test_that("validate_cube_diagnostics accepts valid diagnostics table", {
  validated <- validate_cube_diagnostics(valid_diag)

  expect_identical(validated, valid_diag)
})

# ------------------------------------------------------------------------------
# Test: new_cube_diagnostics attaches cube_diagnostics class
# ------------------------------------------------------------------------------
test_that("new_cube_diagnostics attaches cube_diagnostics class", {
  diag <- new_cube_diagnostics(valid_diag)

  expect_s3_class(diag, "cube_diagnostics")
})

# ------------------------------------------------------------------------------
# Test: new_cube_diagnostics preserves diagnostics content
# ------------------------------------------------------------------------------
test_that("new_cube_diagnostics does not modify diagnostics content", {
  diag <- new_cube_diagnostics(valid_diag)

  expect_identical(diag$dimension, valid_diag$dimension)
  expect_identical(diag$metric, valid_diag$metric)
  expect_identical(diag$value, valid_diag$value)
})

# ------------------------------------------------------------------------------
# Test: validate_cube_diagnostics detects missing columns
# ------------------------------------------------------------------------------
test_that("validate_cube_diagnostics errors when required columns missing", {
  bad_diag <- valid_diag
  bad_diag$severity <- NULL

  expect_error(
    validate_cube_diagnostics(bad_diag),
    "Missing diagnostic columns"
  )
})

# ------------------------------------------------------------------------------
# Test: multiple missing columns are reported
# ------------------------------------------------------------------------------
test_that("validate_cube_diagnostics reports all missing columns", {
  bad_diag <- valid_diag
  bad_diag$metric <- NULL
  bad_diag$message <- NULL

  expect_error(
    validate_cube_diagnostics(bad_diag),
    "metric, message"
  )
})

# ------------------------------------------------------------------------------
# Test: new_cube_diagnostics also validates structure
# ------------------------------------------------------------------------------
test_that("new_cube_diagnostics fails if diagnostics table invalid", {
  bad_diag <- valid_diag
  bad_diag$value <- NULL

  expect_error(
    new_cube_diagnostics(bad_diag),
    "Missing diagnostic columns"
  )
})
