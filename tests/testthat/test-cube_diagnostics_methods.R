# ------------------------------
# Minimal example cube_diagnostics
# ------------------------------
x <- data.frame(
  metric = c("m1", "m2", "m3", "m4"),
  dimension = c("temporal", "temporal", "spatial", "taxonomical"),
  severity = c("ok", "note", "important", "ok"),
  message = c("msg1", "msg2", "msg3", "msg4")
)
class(x) <- c("cube_diagnostics", class(x))

# ------------------------------
# Test print.cube_diagnostics
# ------------------------------
test_that("print.cube_diagnostics runs and produces output", {
  expect_output(print(x), "Data cube diagnostics")
  expect_output(print(x, filter_summary = "note"), "Data cube diagnostics")
  expect_output(print(x, sort_summary = "asc"), "Data cube diagnostics")
  expect_error(print(x, sort_summary = "invalid"), "`sort_summary` must be")
})

# ------------------------------
# Test summary.cube_diagnostics
# ------------------------------
test_that("summary.cube_diagnostics returns correct structure", {
  s <- summary(x)
  expect_s3_class(s, "summary_cube_diagnostics")
  expect_named(s, c("n_rules", "severity", "dimensions", "flagged"))
  expect_equal(s$n_rules, nrow(x))
  expect_equal(sum(s$severity), nrow(x))
  expect_true(all(names(s$severity) %in% unique(x$severity)))
  expect_true(all(names(s$dimensions) %in% unique(x$dimension)))
  expect_equal(nrow(s$flagged), sum(x$severity != "ok"))
})

# ------------------------------
# Test print.summary_cube_diagnostics
# ------------------------------
test_that("print.summary_cube_diagnostics runs and produces output", {
  s <- summary(x)
  expect_output(print(s), "<cube_diagnostics_summary>")
})

# ------------------------------
# Test plot.cube_diagnostics
# ------------------------------
test_that("plot.cube_diagnostics produces ggplot objects", {
  p1 <- plot(x, type = "severity")
  p2 <- plot(x, type = "dimension")
  p3 <- plot(x, type = "heatmap")

  expect_s3_class(p1, "gg")
  expect_s3_class(p2, "gg")
  expect_s3_class(p3, "gg")

  expect_error(plot(x, type = "invalid"), "`type` must be one of")
})
