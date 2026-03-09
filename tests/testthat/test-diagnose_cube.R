# Helper cube constructor
make_cube <- function(
    years,
    cells,
    taxa,
    obs = 1,
    uncertainty = 50,
    resolution = "1km"
) {
  # Create data frame
  df <- expand.grid(
    year = years,
    cellCode = cells,
    taxonKey = taxa
  )
  df$obs <- obs
  df$minCoordinateUncertaintyInMeters <- uncertainty
  # Create processed cube
  cube <- list(
    meta = "test cube",
    data = df,
    resolutions = resolution
  )
  class(cube) <- "processed_cube"
  # Return cube
  cube
}

################################################################################
## Structural tests
################################################################################
test_that("diagnose_cube returns cube_diagnostics object", {
  cube <- make_cube(
    years = 2015:2020,
    cells = paste0("c",1:5),
    taxa = paste0("t",1:5)
  )
  res <- diagnose_cube(cube, verbose = FALSE)

  expect_s3_class(res, "cube_diagnostics")
})

test_that("all basic rules are evaluated", {
  cube <- make_cube(
    years = 2015:2020,
    cells = paste0("c",1:5),
    taxa = paste0("t",1:5)
  )
  res <- diagnose_cube(cube, verbose = FALSE)

  expect_equal(
    length(basic_cube_rules()),
    nrow(res)
  )
  expect_equal(
    sapply(basic_cube_rules(), function(rule) rule$dimension),
    res$dimension
  )
  expect_equal(
    sapply(basic_cube_rules(), function(rule) rule$id),
    res$metric
  )
  expect_equal(
    rep("ok", length(basic_cube_rules())),
    res$severity
  )
})

################################################################################
## Temporal rules
################################################################################

test_that("temporal_min_years severity levels", {
  # very few years -> important
  cube <- make_cube(
    years = 2019:2020,
    cells = paste0("c", 1:5),
    taxa = paste0("t", 1:5)
  )
  res <- diagnose_cube(cube, verbose = FALSE)

  expect_equal(
    res$severity[res$metric == "temporal_min_points"],
    "important"
  )

  # moderate years -> note
  cube <- make_cube(
    years = 2017:2019,
    cells = paste0("c", 1:5),
    taxa = paste0("t", 1:5)
  )
  res <- diagnose_cube(cube, verbose = FALSE)

  expect_equal(
    res$severity[res$metric == "temporal_min_points"],
    "note"
  )

  # sufficient years -> ok
  cube <- make_cube(
    years = 2015:2020,
    cells = paste0("c", 1:5),
    taxa = paste0("t", 1:5)
  )
  res <- diagnose_cube(cube, verbose = FALSE)

  expect_equal(
    res$severity[res$metric == "temporal_min_points"],
    "ok"
  )
})

test_that("temporal_missing_years severity levels", {
  cube <- make_cube(
    years = c(2015, 2016, 2019, 2020),
    cells = paste0("c", 1:5),
    taxa = paste0("t", 1:5)
  )
  res <- diagnose_cube(cube, verbose = FALSE)

  expect_true(
    res$metric[res$metric == "temporal_missing_years"]
      == "temporal_missing_years"
  )

})

################################################################################
## Spatial rules
################################################################################

test_that("spatial_min_cells severity levels", {
  cube <- make_cube(
    years = 2015:2020,
    cells = c("c1","c2"),
    taxa = paste0("t",1:5)
  )
  res <- diagnose_cube(cube, verbose = FALSE)

  expect_equal(
    res$severity[res$metric == "spatial_min_cells"],
    "important"
  )
})

test_that("spatial_max_uncertainty works", {
  cube <- make_cube(
    years = 2015:2020,
    cells = paste0("c",1:5),
    taxa = paste0("t",1:5),
    uncertainty = 2000   # > 1km resolution
  )
  res <- diagnose_cube(cube, verbose = FALSE)

  expect_true(
    res$value[res$metric == "spatial_max_uncertainty"] > 0
  )
})

test_that("spatial_miss_uncertainty detects missing values", {
  cube <- make_cube(
    years = 2015:2020,
    cells = paste0("c",1:5),
    taxa = paste0("t",1:5),
    uncertainty = NA
  )
  res <- diagnose_cube(cube, verbose = FALSE)

  expect_true(
    res$value[res$metric == "spatial_miss_uncertainty"] > 0
  )
})

################################################################################
## Taxonomic rules
################################################################################

test_that("taxon_min_taxa severity", {
  cube <- make_cube(
    years = 2015:2020,
    cells = paste0("c",1:5),
    taxa = c("t1","t2")
  )
  res <- diagnose_cube(cube, verbose = FALSE)

  expect_equal(
    res$severity[res$metric == "taxon_min_taxa"],
    "important"
  )
})

################################################################################
## Observation rules
################################################################################

test_that("obs_min_records severity", {
  cube <- make_cube(
    years = 2019:2020,
    cells = c("c1","c2"),
    taxa = c("t1","t2")
  )
  res <- diagnose_cube(cube, verbose = FALSE)

  expect_true(
    res$metric[res$metric == "obs_min_records"] == "obs_min_records"
  )
})

test_that("obs_min_total works", {
  cube <- make_cube(
    years = 2015:2020,
    cells = paste0("c",1:5),
    taxa = paste0("t",1:5),
    obs = 1
  )
  res <- diagnose_cube(cube, verbose = FALSE)

  expect_true(
    res$metric[res$metric == "obs_min_total"] == "obs_min_total"
  )
})
