# ------------------------------
# Minimal example cube_rule
# ------------------------------
rule <- list(
  id = "obs_min_total",
  dimension = "observation",
  threshold = 40,
  compute = function(x) NULL,
  severity = function(x, y) NULL,
  message = function(x, y) NULL
)
class(rule) <- c("cube_rule", class(rule))

# ------------------------------
# Test print.cube_rule
# ------------------------------
test_that("print.cube_rule runs and produces output", {
  expect_output(print(rule), "<cube_rule>")
  expect_output(print(rule), "id:")
  expect_output(print(rule), "dimension:")
  expect_output(print(rule), "threshold:")
  expect_output(print(rule), "Functions:")
})

# ------------------------------
# Test invisible return
# ------------------------------
test_that("print.cube_rule returns object invisibly", {
  expect_invisible(print(rule))
})

# ------------------------------
# Test behavior when optional fields missing
# ------------------------------
test_that("print.cube_rule works if optional fields missing", {
  rule2 <- list(
    compute = function(x) NULL,
    severity = function(x, y) NULL,
    message = function(x, y) NULL
  )
  class(rule2) <- c("cube_rule", class(rule2))

  expect_output(print(rule2), "<cube_rule>")
})
