# ------------------------------------------------------------------------------
# Tests for resolution_to_meters()
# ------------------------------------------------------------------------------
test_that("resolution_to_meters converts km correctly", {
  expect_equal(resolution_to_meters("1km"), 1000)
  expect_equal(resolution_to_meters("10km"), 10000)
})

test_that("resolution_to_meters converts degrees correctly", {
  expect_equal(resolution_to_meters("1degree"), 111320)
  expect_equal(resolution_to_meters("1degrees"), 111320)
  expect_equal(resolution_to_meters("0.25degrees"), 0.25 * 111320)
})

test_that("resolution_to_meters errors for unsupported formats", {
  expect_error(
    resolution_to_meters("100m"),
    "Unsupported resolution format"
  )
  expect_error(
    resolution_to_meters("abc"),
    "Unsupported resolution format"
  )
})


# ------------------------------------------------------------------------------
# Tests for resolve_cube_rules()
# ------------------------------------------------------------------------------
test_that("resolve_cube_rules returns list when rules already list", {
  dummy_rule <- list(id = "rule_a")
  rules <- list(dummy_rule)
  resolved <- resolve_cube_rules(rules)

  expect_identical(resolved, rules)
})

test_that("resolve_cube_rules expands character rule sets", {
  rules <- resolve_cube_rules("basic")

  expect_true(is.list(rules))
})

test_that("resolve_cube_rules errors when rules not character or list", {
  expect_error(
    resolve_cube_rules(123),
    "`rules` must be a character vector or list of rule objects"
  )
})


# ------------------------------------------------------------------------------
# Tests for get_cube_rule_set()
# ------------------------------------------------------------------------------
test_that("get_cube_rule_set returns list for known rule set", {
  rules <- get_cube_rule_set("basic")

  expect_true(is.list(rules))
})

test_that("get_cube_rule_set errors for unknown rule set", {
  expect_error(
    get_cube_rule_set("unknown"),
    "Unknown rule set"
  )
})
