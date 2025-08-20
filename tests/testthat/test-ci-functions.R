set.seed(123)
boot_reps <- rnorm(1000)        # bootstrap replicates
t0 <- mean(boot_reps)           # observed statistic
a <- 0.01                       # fake acceleration value for BCa

# -------------------------------------------------------------------------
# Percentile CI
# -------------------------------------------------------------------------
test_that("percentile CI works as expected", {
  res <- perc_ci(boot_reps, conf = 0.95)

  expect_true(is.matrix(res))
  expect_equal(colnames(res), c("conf", "rk_lower", "rk_upper", "ll", "ul"))
  expect_equal(nrow(res), 1)

  expect_equal(res[[1, "conf"]], 0.95)
  expect_true(res[1, "ll"] < res[1, "ul"])
})

# -------------------------------------------------------------------------
# Normal CI
# -------------------------------------------------------------------------
test_that("normal CI works as expected", {
  res <- norm_ci(t0, boot_reps, conf = 0.90)

  expect_true(is.matrix(res))
  expect_equal(colnames(res), c("conf", "ll", "ul"))
  expect_equal(nrow(res), 1)

  expect_equal(res[[1, "conf"]], 0.90)
  expect_true(res[1, "ll"] < res[1, "ul"])
})

# -------------------------------------------------------------------------
# Basic CI
# -------------------------------------------------------------------------
test_that("basic CI works as expected", {
  res <- basic_ci(t0, boot_reps, conf = 0.95)

  expect_true(is.matrix(res))
  expect_equal(colnames(res), c("conf", "rk_lower", "rk_upper", "ll", "ul"))
  expect_equal(nrow(res), 1)

  expect_equal(res[[1, "conf"]], 0.95)
  expect_true(res[1, "ll"] < res[1, "ul"])
})

# -------------------------------------------------------------------------
# BCa CI
# -------------------------------------------------------------------------
test_that("BCa CI works as expected", {
  res <- bca_ci(t0, boot_reps, a, conf = 0.95)

  expect_true(is.matrix(res))
  expect_equal(colnames(res), c("conf", "rk_lower", "rk_upper", "ll", "ul"))
  expect_equal(nrow(res), 1)

  expect_equal(res[[1, "conf"]], 0.95)
  expect_true(res[1, "ll"] < res[1, "ul"])
})

test_that("BCa CI handles infinite z0 gracefully", {
  t_inf <- rep(1, 10)  # all replicates below t0
  t0_inf <- 2

  expect_warning(
    bca_ci(t0_inf, t_inf, a),
    "Estimated adjustment 'z0' is infinite.",
    fixed = TRUE
  )
  suppressWarnings(res <- bca_ci(t0_inf, t_inf, a))
  expect_true(all(is.na(res[, c("ll", "ul")])))
})
