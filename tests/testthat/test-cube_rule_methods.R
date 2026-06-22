# ------------------------------
# Minimal example cube_rule
# ------------------------------
rule <- list(
  id = "obs_min_total",
  dimension = "observation",
  thresholds = c(ok = 40),
  compute = function(x) NULL,
  severity = function(x, y) NULL,
  message = function(x, y) NULL,
  function_fn = function(x, y) NULL
)
class(rule) <- c("cube_rule", class(rule))

# ------------------------------
# Test print.cube_rule
# ------------------------------
test_that("print.cube_rule runs and produces output", {
  expect_output(print(rule), "<cube_rule>")
  expect_output(print(rule), "id:")
  expect_output(print(rule), "dimension:")
  expect_output(print(rule), "thresholds:")
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

# ------------------------------
# Minimal example cube_rule_list
# ------------------------------
rule1 <- list(
  id = "obs_min_total",
  dimension = "observation",
  thresholds = c(ok = 40),
  compute = function(x) NULL,
  severity = function(x, y) NULL,
  message = function(x, y) NULL
)
class(rule1) <- c("cube_rule", class(rule1))

rule2 <- list(
  id = "temporal_min_years",
  dimension = "temporal",
  thresholds = c(ok = 10),
  compute = function(x) NULL,
  severity = function(x, y) NULL,
  message = function(x, y) NULL
)
class(rule2) <- c("cube_rule", class(rule2))

rules <- list(rule1, rule2)
class(rules) <- "cube_rule_list"


# ------------------------------
# Test print.cube_rule_list
# ------------------------------
test_that("print.cube_rule_list runs and produces output", {
  expect_output(print(rules), "<cube_rule_list>")
  expect_output(print(rules), "rules:")
  expect_output(print(rules), "obs_min_total")
  expect_output(print(rules), "temporal_min_years")
})

# ------------------------------
# Test invisible return
# ------------------------------
test_that("print.cube_rule_list returns object invisibly", {
  expect_invisible(print(rules))
})

# ------------------------------
# Test behavior when rule fields missing
# ------------------------------
test_that("print.cube_rule_list works if rule fields missing", {
  rule_missing <- list()
  class(rule_missing) <- c("cube_rule", class(rule_missing))

  rules2 <- list(rule_missing)
  class(rules2) <- "cube_rule_list"

  expect_output(print(rules2), "<cube_rule_list>")
})
