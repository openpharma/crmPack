# h_model_dual_endpoint_sigma2W ----

test_that("h_model_dual_endpoint_sigma2W updates model components for fixed sigma2W", {
  comp <- h_get_model_4comp()
  result <- h_model_dual_endpoint_sigma2W(TRUE, sigma2W = 5, comp = comp)
  comp$modelspecs <- c(comp$modelspecs, list(precW = 1 / 5))
  expect_identical(result, comp)
})

test_that("h_model_dual_endpoint_sigma2W updates model components", {
  comp <- h_get_model_4comp()

  result <- h_model_dual_endpoint_sigma2W(FALSE, sigma2W = c(a = 2, b = 4), comp = comp)
  comp$priormodel <- function() {
    y ~ x + 1
    precW ~ dgamma(precWa, precWb)
  }
  environment(comp$priormodel) <- environment(result$priormodel)
  comp$modelspecs <- c(comp$modelspecs, list(precWa = 2, precWb = 4))
  comp$init$precW <- 1 # nolint
  comp$sample <- c(comp$sample, "precW")

  expect_identical(result, comp)
})

test_that("h_model_dual_endpoint_sigma2W throws error for no valid fixed sigma2W", {
  comp <- h_get_model_4comp()
  expect_error(
    h_model_dual_endpoint_sigma2W(TRUE, sigma2W = 1:5, comp = comp),
    "Assertion on 'sigma2W' failed: Must have length 1."
  )
  expect_error(
    h_model_dual_endpoint_sigma2W(TRUE, sigma2W = -2, comp = comp),
    "Assertion on 'sigma2W' failed: Element 1 is not >= 2.22507e-308."
  )
})

test_that("h_model_dual_endpoint_sigma2W throws error for no valid sigma2W", {
  comp <- h_get_model_4comp()
  expect_error(
    h_model_dual_endpoint_sigma2W(FALSE, sigma2W = 1:5, comp = comp),
    "Assertion on .* failed"
  )
})

# h_model_dual_endpoint_rho ----

test_that("h_model_dual_endpoint_rho updates model components for fixed rho", {
  comp <- h_get_model_4comp()
  result <- h_model_dual_endpoint_rho(TRUE, rho = 0.5, comp = comp)
  comp$modelspecs <- c(comp$modelspecs, list(rho = 0.5))
  expect_identical(result, comp)
})

test_that("h_model_dual_endpoint_rho updates model components", {
  comp <- h_get_model_4comp()

  result <- h_model_dual_endpoint_rho(FALSE, rho = c(a = 2, b = 4), comp = comp)
  comp$priormodel <- function() {
    y ~ x + 1
    kappa ~ dbeta(rhoa, rhob)
    rho <- 2 * kappa - 1
  }
  environment(comp$priormodel) <- environment(result$priormodel)
  comp$modelspecs <- c(comp$modelspecs, list(rhoa = 2, rhob = 4))
  comp$init$kappa <- 0.5
  comp$sample <- c(comp$sample, "rho")

  expect_identical(result, comp)
})

test_that("h_model_dual_endpoint_rho throws error for no valid fixed rho", {
  comp <- h_get_model_4comp()
  expect_error(
    h_model_dual_endpoint_rho(TRUE, rho = 1:5, comp = comp),
    "Assertion on 'rho' failed: Must have length 1."
  )
  expect_error(
    h_model_dual_endpoint_rho(TRUE, rho = 2, comp = comp),
    "Assertion on 'rho' failed: Element 1 is not <= 1."
  )
  expect_error(
    h_model_dual_endpoint_rho(TRUE, rho = -2, comp = comp),
    "Assertion on 'rho' failed: Element 1 is not >= -1."
  )
})

test_that("h_model_dual_endpoint_rho throws error for no valid rho", {
  comp <- h_get_model_4comp()
  expect_error(
    h_model_dual_endpoint_rho(FALSE, rho = 1:5, comp = comp),
    "Assertion on .* failed"
  )
})
