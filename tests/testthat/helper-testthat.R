expect_valid <- function(object, class) {
  expect_s4_class(object, class)
  expect_true(validObject(object))
}

# Return the platform name (e.g., "Windows", "Linux", "MacOS")
platform_name <- function() {
  Sys.info()[["sysname"]]
}

# Wrapper around vdiffr::expect_doppelganger to include platform name as variant.
expect_doppel <- function(...) {
  vdiffr::expect_doppelganger(..., variant = platform_name())
}

# Wrapper around testthat::expect_snapshot to include platform name as variant.
expect_snap <- function(...) {
  testthat::expect_snapshot(..., variant = platform_name())
}
