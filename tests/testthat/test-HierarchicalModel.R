local_hierarchical_mono_model <- function() {
  LogisticLogNormal(
    mean = c(-0.85, 1),
    cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
    ref_dose = 10
  )
}

local_hierarchical_combo_model <- function(log_normal_eta = FALSE) {
  LogisticLogNormalCombo(
    single_models = list(
      drug1 = LogisticLogNormal(
        mean = c(-0.85, 1),
        cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
        ref_dose = 10
      ),
      drug2 = LogisticLogNormal(
        mean = c(-0.7, 0.8),
        cov = matrix(c(1.1, -0.3, -0.3, 0.9), nrow = 2),
        ref_dose = 20
      )
    ),
    gamma = 0,
    tau = 1,
    log_normal_eta = log_normal_eta
  )
}

local_hierarchical_parameter_pools <- function() {
  list(
    mono_intercept = list(
      my_mono = "alpha0",
      my_combo = "alpha0[1]"
    ),
    mono_slope = list(
      my_mono = "alpha1",
      my_combo = "alpha1[1]"
    )
  )
}

local_hierarchical_model <- function(log_normal_eta = FALSE) {
  HierarchicalModel(
    my_mono = local_hierarchical_mono_model(),
    my_combo = local_hierarchical_combo_model(log_normal_eta = log_normal_eta),
    exchangeable_parameters = local_hierarchical_parameter_pools()
  )
}

local_hierarchical_data <- function(empty = FALSE) {
  mono_y <- if (empty) integer() else c(0L, 0L, 0L, 1L)
  combo_y <- if (empty) integer() else c(0L, 0L, 0L, 1L)
  mono_x <- if (empty) numeric() else c(10, 10, 20, 20)
  combo_x <- if (empty) {
    matrix(
      numeric(),
      nrow = 0,
      ncol = 2,
      dimnames = list(NULL, c("drug1", "drug2"))
    )
  } else {
    cbind(
      drug1 = c(10, 10, 20, 20),
      drug2 = c(20, 40, 20, 40)
    )
  }
  mono_id <- seq_along(mono_y)
  combo_id <- seq_along(combo_y)

  HierarchicalData(
    arms = list(
      my_mono = Data(
        x = mono_x,
        y = mono_y,
        doseGrid = c(10, 20, 30),
        ID = mono_id,
        cohort = c(1L, 1L, 2L, 2L)[seq_along(mono_y)]
      ),
      my_combo = DataCombo(
        x = combo_x,
        y = combo_y,
        doseGrid = list(
          drug1 = c(10, 20, 30),
          drug2 = c(20, 40)
        ),
        ID = combo_id,
        cohort = seq_along(combo_y)
      )
    )
  )
}

test_that("hierarchical helper primitives return expected metadata", {
  mono_model <- local_hierarchical_mono_model()
  combo_model <- local_hierarchical_combo_model()

  expect_equal(h_hierarchical_safe_name("My combo-arm"), "My_combo_arm")
  expect_equal(h_hierarchical_model_type(mono_model), "mono")
  expect_equal(h_hierarchical_model_type(combo_model), "combo")
  expect_equal(h_hierarchical_supported_refs(mono_model), c("alpha0", "alpha1"))
  expect_equal(
    h_hierarchical_supported_refs(combo_model),
    c("alpha0[1]", "alpha1[1]", "alpha0[2]", "alpha1[2]")
  )

  expect_equal(
    h_hierarchical_parse_ref(mono_model, "my_mono", "alpha0"),
    list(
      kind = "alpha0",
      index = 1L,
      latent = "theta_my_mono[1]",
      sample = "alpha0_my_mono"
    )
  )
  expect_equal(
    h_hierarchical_parse_ref(combo_model, "my_combo", "alpha1[2]"),
    list(
      kind = "alpha1",
      index = 2L,
      latent = "theta_my_combo[2, 2]",
      sample = "alpha1_my_combo[2]"
    )
  )
})

test_that("hierarchical pool lookup helpers work as expected", {
  pooled_map <- h_hierarchical_make_pool_map(local_hierarchical_parameter_pools())

  expect_snapshot(pooled_map)
})

test_that("hierarchical compiler helpers produce readable compiled functions", {
  model <- local_hierarchical_model()
  body_datamodel <- paste(deparse(body(model@datamodel)), collapse = "\n")
  body_priormodel <- paste(deparse(body(model@priormodel)), collapse = "\n")

  expect_snapshot(cat(body_datamodel))
  expect_snapshot(cat(body_priormodel))
})

test_that("hierarchical modelspecs and init compilers return expected fields", {
  model <- local_hierarchical_model()
  data <- local_hierarchical_data()

  specs <- model@modelspecs(arms = data@arms, from_prior = FALSE)
  prior_specs <- model@modelspecs(arms = data@arms, from_prior = TRUE)
  inits <- model@init(arms = data@arms)

  expect_snapshot_value(specs, style = "deparse")
  expect_snapshot_value(prior_specs, style = "deparse")
  expect_snapshot_value(inits, style = "deparse")
})

test_that("HierarchicalModel constructor creates a valid object", {
  result <- expect_silent(local_hierarchical_model())

  expect_valid(result, "HierarchicalModel")
  expect_identical(names(result@models_to_arms), c("my_mono", "my_combo"))
  expect_identical(
    names(result@parameter_pools),
    c("mono_intercept", "mono_slope")
  )
  expect_true(all(
    c(
      "alpha0_my_mono",
      "alpha1_my_mono",
      "alpha0_my_combo",
      "alpha1_my_combo",
      "eta_my_combo",
      "mu_mono_intercept",
      "tau_mono_intercept",
      "mu_mono_slope",
      "tau_mono_slope"
    ) %in%
      result@sample
  ))
})

test_that("v_hierarchical_model catches invalid exchangeable parameters", {
  bad_unknown_arm <- local_hierarchical_model()
  bad_unknown_arm@parameter_pools <- list(
    bad_pool = list(
      my_mono = "alpha0",
      does_not_exist = "alpha0[1]"
    )
  )
  expect_match(
    v_hierarchical_model(bad_unknown_arm),
    "refers to unknown hierarchical arms"
  )

  bad_family <- local_hierarchical_model()
  bad_family@parameter_pools <- list(
    bad_pool = list(
      my_mono = "alpha0",
      my_combo = "alpha1[1]"
    )
  )
  expect_match(
    v_hierarchical_model(bad_family),
    "must target the same parameter family"
  )
})

test_that("h_mcmc_get_hierarchical_data flattens arm data for JAGS", {
  model <- local_hierarchical_model()
  data <- local_hierarchical_data()

  result <- h_mcmc_get_hierarchical_data(
    model = model,
    data = data,
    from_prior = FALSE
  )

  expect_snapshot_value(result, style = "deparse")
})

test_that("hierarchical mcmc runs and returns expected sample structure", {
  model <- local_hierarchical_model()
  data <- local_hierarchical_data()
  options <- McmcOptions(
    burnin = 10L,
    step = 1L,
    samples = 20L,
    rng_kind = "Mersenne-Twister",
    rng_seed = 12345L
  )

  result <- mcmc(data = data, model = model, options = options)

  expect_s4_class(result, "Samples")
  expect_true(all(
    c(
      "alpha0_my_mono",
      "alpha1_my_mono",
      "alpha0_my_combo",
      "alpha1_my_combo",
      "eta_my_combo",
      "mu_mono_intercept",
      "tau_mono_intercept"
    ) %in%
      names(result@data)
  ))
  expect_length(result@data$alpha0_my_mono, 20)
  expect_equal(dim(result@data$alpha0_my_combo), c(20L, 2L))
})
