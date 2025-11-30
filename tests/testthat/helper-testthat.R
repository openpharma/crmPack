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

# Same for the two others.
expect_snap_value <- function(...) {
  testthat::expect_snapshot_value(..., variant = platform_name())
}

expect_snap_file <- function(...) {
  testthat::expect_snapshot_file(..., variant = platform_name())
}

# Skip if use CRAN settings and not on CI.
skip_on_cran_but_not_ci <- function() {
  is_on_cran <- !interactive() && !env_var_is_true("NOT_CRAN")
  is_on_ci <- env_var_is_true("CI")
  testthat::skip_if(
    is_on_cran && !is_on_ci,
    "On CRAN, but not on CI"
  )
}

env_var_is_true <- function(var) {
  isTRUE(as.logical(Sys.getenv(var, "false")))
}
