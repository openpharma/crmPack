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

test_that("h_model_dual_endpoint_sigma2W throws error for non-valid fixed sigma2W", {
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

test_that("h_model_dual_endpoint_sigma2W throws error for non-valid sigma2W", {
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

test_that("h_model_dual_endpoint_rho throws error for non-valid fixed rho", {
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

test_that("h_model_dual_endpoint_rho throws error for non-valid rho", {
  comp <- h_get_model_4comp()
  expect_error(
    h_model_dual_endpoint_rho(FALSE, rho = 1:5, comp = comp),
    "Assertion on .* failed"
  )
})

# h_model_dual_endpoint_sigma2betaW ----

test_that("h_model_dual_endpoint_sigma2betaW updates model components for fixed sigma2betaW", {
  de <- h_get_dual_endpoint()
  result <- h_model_dual_endpoint_sigma2betaW(TRUE, sigma2betaW = 5, de = de)
  ms <- de@modelspecs
  de@modelspecs <- function() {
    c(ms(), list(precBetaW = 1 / 5))
  }

  expect_identical(result@modelspecs(), de@modelspecs()) # nolintr
})

test_that("h_model_dual_endpoint_sigma2betaW updates model components", {
  de <- h_get_dual_endpoint()
  result <- h_model_dual_endpoint_sigma2betaW(FALSE, sigma2betaW = c(a = 2, b = 4), de = de)

  ms <- de@modelspecs
  init <- de@init
  de@priormodel <- h_jags_join_models(
    de@priormodel,
    function() {
      precBetaW ~ dgamma(precBetaWa, precBetaWb)
    }
  )
  environment(de@priormodel) <- environment(result@priormodel)
  de@modelspecs <- function() {
    c(ms(), list(precBetaWa = 2, precBetaWb = 4))
  }
  de@init <- function(y) {
    c(init(y), precBetaW = 1)
  }
  de@sample <- c(de@sample, "precBetaW")

  expect_identical(result@priormodel, de@priormodel) # nolintr
  expect_identical(result@modelspecs(), de@modelspecs()) # nolintr
  expect_identical(result@init(0), de@init(0)) # nolintr
  expect_identical(result@init(2), de@init(2)) # nolintr
  expect_identical(result@sample, de@sample) # nolintr
})

test_that("h_model_dual_endpoint_sigma2betaW throws error for non-valid fixed sigma2betaW", {
  de <- h_get_dual_endpoint()
  expect_error(
    h_model_dual_endpoint_sigma2betaW(TRUE, sigma2betaW = 1:5, de = de),
    "Assertion on 'sigma2betaW' failed: Must have length 1."
  )
  expect_error(
    h_model_dual_endpoint_sigma2betaW(TRUE, sigma2betaW = -2, de = de),
    "Assertion on 'sigma2betaW' failed: Element 1 is not >= 2.22507e-308."
  )
})

test_that("h_model_dual_endpoint_sigma2betaW throws error for non-valid sigma2betaW", {
  de <- h_get_dual_endpoint()
  expect_error(
    h_model_dual_endpoint_sigma2betaW(FALSE, sigma2betaW = 1:5, de = de),
    "Assertion on .* failed"
  )
})

# h_model_dual_endpoint_beta ----

test_that("h_model_dual_endpoint_beta updates model components for scalar param", {
  de <- h_get_dual_endpoint()
  result <- h_model_dual_endpoint_beta(
    param = 5,
    param_name = "some_name",
    de = de
  )

  # Expect that modelspecs is updated correctly.
  expected_ms <- c(de@modelspecs(), c(list(some_name = 5))) # nolintr
  expect_identical(result@modelspecs(), expected_ms) # nolintr

  # Expect that use_fixed is updated correctly, and others (except modelspecs) are not changed.
  de@use_fixed <- c(de@use_fixed, some_name = TRUE)
  other_slots <- setdiff(slotNames(de), "modelspecs")
  expect_identical(
    h_slots(de, other_slots),
    h_slots(result, other_slots)
  )
})

test_that("h_model_dual_endpoint_beta updates model components", {
  de <- h_get_dual_endpoint()

  priormodel <- function() {
    E0 ~ dunif(E0_a, E0_b)
  }
  result <- h_model_dual_endpoint_beta(
    param = c(2, 6),
    param_name = "E0",
    param_suffix = c("a", "b"),
    priormodel = priormodel,
    de = de
  )

  # Expect that priormodel is updated correctly.
  expected_prior <- h_jags_join_models(de@priormodel, priormodel)
  expect_identical(result@priormodel, expected_prior)

  # Expect that modelspecs is updated correctly.
  expected_ms <- c(de@modelspecs(), c(list(E0a = 2, E0b = 6))) # nolintr
  expect_identical(result@modelspecs(), expected_ms) # nolintr

  # Expect that init is updated correctly.
  expected_init <- c(de@init(0), c(list(E0 = 4))) # nolintr
  expect_identical(result@init(0), expected_init) # nolintr

  # Expect that use_fixed is updated correctly, and others are not changed.
  de@use_fixed <- c(de@use_fixed, E0 = FALSE)
  de@sample <- c(de@sample, "E0")
  other_slots <- setdiff(slotNames(de), c("priormodel", "modelspecs", "init"))
  expect_identical(
    h_slots(de, other_slots),
    h_slots(result, other_slots)
  )
})

test_that("h_model_dual_endpoint_beta throws error for non-valid param", {
  de <- h_get_dual_endpoint()
  expect_error(
    h_model_dual_endpoint_beta(param = 1:5, param_name = "some_name", de = de),
    "Assertion on 'param' failed: Must have length <= 2, but has length 5."
  )
})

test_that("h_model_dual_endpoint_beta throws error for non-valid param_name", {
  de <- h_get_dual_endpoint()
  expect_error(
    h_model_dual_endpoint_beta(param = 5, param_name = c("p1", "p2"), de = de),
    "Assertion on 'param_name' failed: Must have length 1."
  )
})

test_that("h_model_dual_endpoint_beta throws error for non-valid param_suffix", {
  de <- h_get_dual_endpoint()
  expect_error(
    h_model_dual_endpoint_beta(
      param = c(1, 1),
      param_name = "p1",
      param_suffix = c("s1", "s2", "s3"),
      priormodel = function() {}, # nolintr
      de = de
    ),
    "Assertion on 'param_suffix' failed: Must have length 2, but has length 3."
  )
})

test_that("h_model_dual_endpoint_beta throws error for non-valid priormodel", {
  de <- h_get_dual_endpoint()
  expect_error(
    h_model_dual_endpoint_beta(
      param = c(1, 1),
      param_name = "p1",
      priormodel = "wrong",
      de = de
    ),
    "Assertion on 'priormodel' failed: Must be a function, not 'character'."
  )
})
