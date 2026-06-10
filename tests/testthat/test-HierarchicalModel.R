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

local_hierarchical_samples <- function() {
  HierarchicalSamples(
    data = list(
      alpha0_my_mono = c(-3.0, -2.5),
      alpha1_my_mono = c(1.0, 0.8),
      alpha0_my_combo = matrix(
        c(-3.0, -3.5, -2.5, -3.0),
        nrow = 2L,
        byrow = TRUE,
        dimnames = list(NULL, c("drug1", "drug2"))
      ),
      alpha1_my_combo = matrix(
        c(1.0, 1.2, 0.8, 1.1),
        nrow = 2L,
        byrow = TRUE,
        dimnames = list(NULL, c("drug1", "drug2"))
      ),
      eta_my_combo = c(0.0, 0.2)
    ),
    options = h_get_mcmc_options(samples = 2L),
    arm_samples = list(
      my_mono = c(
        alpha0 = "alpha0_my_mono",
        alpha1 = "alpha1_my_mono"
      ),
      my_combo = c(
        alpha0 = "alpha0_my_combo",
        alpha1 = "alpha1_my_combo",
        eta = "eta_my_combo"
      )
    )
  )
}

local_hierarchical_design <- function() {
  HierarchicalDesign(
    DesignArm(
      name = "arm_a",
      active = TRUE,
      design = .DefaultDesign()
    ),
    DesignArm(
      name = "arm_b",
      active = FALSE,
      design = .DefaultDesign()
    ),
    exchangeable_parameters = list(
      shared_intercept = list(
        arm_a = "alpha0",
        arm_b = "alpha0"
      ),
      shared_slope = list(
        arm_a = "alpha1",
        arm_b = "alpha1"
      )
    )
  )
}

local_hierarchical_simulations <- function() {
  data <- list(
    HierarchicalData(
      arms = list(
        arm_a = Data(
          x = c(10, 20),
          y = c(0L, 1L),
          doseGrid = c(10, 20),
          ID = 1L:2L,
          cohort = 1L:2L
        ),
        arm_b = Data(
          x = 10,
          y = 0L,
          doseGrid = c(10, 20),
          ID = 1L,
          cohort = 1L
        )
      )
    ),
    HierarchicalData(
      arms = list(
        arm_a = Data(
          x = c(10, 10),
          y = c(0L, 0L),
          doseGrid = c(10, 20),
          ID = 1L:2L,
          cohort = 1L:2L
        ),
        arm_b = Data(
          x = 10,
          y = 0L,
          doseGrid = c(10, 20),
          ID = 1L,
          cohort = 1L
        )
      )
    )
  )
  fit <- list(
    list(
      arm_a = data.frame(
        middle = c(0.1, 0.3),
        lower = c(0.05, 0.2),
        upper = c(0.2, 0.4)
      ),
      arm_b = data.frame(
        middle = c(0.1, 0.2),
        lower = c(0.05, 0.1),
        upper = c(0.2, 0.3)
      )
    ),
    list(
      arm_a = data.frame(
        middle = c(0.15, 0.35),
        lower = c(0.1, 0.25),
        upper = c(0.25, 0.45)
      ),
      arm_b = data.frame(
        middle = c(0.1, 0.2),
        lower = c(0.05, 0.1),
        upper = c(0.2, 0.3)
      )
    )
  )

  HierarchicalSimulations(
    data = data,
    doses = list(
      list(arm_a = 20, arm_b = NULL),
      list(arm_a = 10, arm_b = NULL)
    ),
    samples = list(.HierarchicalSamples(), .HierarchicalSamples()),
    fit = fit,
    stop_reasons = list(
      list(arm_a = "Stopped A", arm_b = "Historical arm: not enrolling."),
      list(arm_a = "Stopped A", arm_b = "Historical arm: not enrolling.")
    ),
    stop_report = list(
      list(arm_a = c(`Minimum patients` = TRUE), arm_b = NULL),
      list(arm_a = c(`Minimum patients` = FALSE), arm_b = NULL)
    ),
    additional_stats = list(
      list(arm_a = list(), arm_b = list()),
      list(arm_a = list(), arm_b = list())
    ),
    seed = 123L
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

test_that("HierarchicalDesign constructor derives hierarchical data and model", {
  result <- expect_silent(local_hierarchical_design())

  expect_valid(result, "HierarchicalDesign")
  expect_identical(names(result@arms), c("arm_a", "arm_b"))
  expect_identical(names(result@data@arms), c("arm_a", "arm_b"))
  expect_identical(names(result@model@models_to_arms), c("arm_a", "arm_b"))
  expect_true(all(vapply(
    result@arms,
    inherits,
    logical(1L),
    what = "DesignArm"
  )))
  expect_true(is(result@data, "HierarchicalData"))
  expect_true(is(result@model, "HierarchicalModel"))

  # TODO: add tests for arm opening rules once hierarchical accrual is implemented.
})

test_that("HierarchicalData update can target one arm", {
  data <- local_hierarchical_design()@data

  result <- update(
    object = data,
    arm = "arm_a",
    x = 10,
    y = c(0L, 1L)
  )

  expect_valid(result, "HierarchicalData")
  expect_equal(result@arms$arm_a@nObs, data@arms$arm_a@nObs + 2L)
  expect_equal(result@arms$arm_b@nObs, data@arms$arm_b@nObs)
  expect_equal(tail(result@arms$arm_a@x, 2L), c(10, 10))
})

test_that("HierarchicalDesign simulate returns hierarchical simulations", {
  design <- local_hierarchical_design()
  design@arms$arm_a@design@stopping <- StoppingMinPatients(nPatients = 1L)
  design@arms$arm_a@design@cohort_size <- CohortSizeConst(size = 1L)

  result <- simulate(
    design,
    truth = function(dose, ...) 0.1,
    truthResponse = function(dose, ...) 0.5,
    nsim = 1L,
    seed = 123L,
    mcmcOptions = McmcOptions(
      burnin = 10L,
      step = 1L,
      samples = 20L,
      rng_kind = "Mersenne-Twister",
      rng_seed = 12345L
    ),
    parallel = FALSE
  )

  expect_valid(result, "HierarchicalSimulations")
  expect_s4_class(result@samples[[1L]], "HierarchicalSamples")
  expect_equal(
    result@data[[1L]]@arms$arm_a@nObs,
    design@data@arms$arm_a@nObs + 1L
  )
  expect_equal(result@data[[1L]]@arms$arm_b@nObs, design@data@arms$arm_b@nObs)
  expect_true(is.list(result@doses[[1L]]))
  expect_true(is.list(result@stop_reasons[[1L]]))
})

test_that("HierarchicalDesign simulate supports backfill cohorts", {
  design <- local_hierarchical_design()
  design@arms$arm_a@design@stopping <- StoppingMinPatients(nPatients = 4L)
  design@arms$arm_a@design@cohort_size <- CohortSizeConst(size = 1L)
  design@arms$arm_a@design@backfill <- Backfill(
    cohort_size = CohortSizeConst(size = 1L),
    opening = OpeningMinDose(min_dose = 0),
    recruitment = RecruitmentUnlimited(),
    max_size = 4L
  )

  result <- simulate(
    design,
    truth = function(dose, ...) 0.01,
    truthResponse = function(dose, ...) 0.5,
    nsim = 1L,
    seed = 123L,
    mcmcOptions = McmcOptions(
      burnin = 10L,
      step = 1L,
      samples = 20L,
      rng_kind = "Mersenne-Twister",
      rng_seed = 12345L
    ),
    parallel = FALSE
  )

  arm_data <- result@data[[1L]]@arms$arm_a
  expect_valid(result, "HierarchicalSimulations")
  expect_true(any(arm_data@backfilled))
})

test_that("hierarchical summary helpers select arm arguments", {
  shared_truth <- function(dose, ...) dose
  arm_truth <- list(
    arm_a = function(dose, ...) dose + 1,
    arm_b = function(dose, ...) dose + 2
  )

  expect_identical(
    h_hierarchical_get_arm_arg(shared_truth, "arm_a"),
    shared_truth
  )
  expect_identical(
    h_hierarchical_get_arm_arg(arm_truth, "arm_b"),
    arm_truth$arm_b
  )
  expect_error(
    h_hierarchical_get_arm_arg(arm_truth, "arm_c"),
    "must include the elements"
  )
})

test_that("hierarchical summary helpers bind stop reports", {
  result <- h_hierarchical_bind_stop_report(
    stop_report = list(
      list(arm_a = c(rule = TRUE)),
      list(arm_a = c(rule = FALSE))
    ),
    arm_name = "arm_a",
    nsim = 2L
  )

  expect_true(is.matrix(result))
  expect_equal(result[, "rule"], c(TRUE, FALSE))

  historical <- h_hierarchical_bind_stop_report(
    stop_report = list(list(arm_b = NULL), list(arm_b = NULL)),
    arm_name = "arm_b",
    nsim = 2L
  )

  expect_equal(dim(historical), c(2L, 1L))
  expect_equal(colnames(historical), "Historical arm")
  expect_true(all(historical))
})

test_that("hierarchical summary helpers rebuild arm simulations", {
  sims <- local_hierarchical_simulations()

  arm_sims <- h_hierarchical_arm_simulations(sims, "arm_a")

  expect_s4_class(arm_sims, "Simulations")
  expect_equal(arm_sims@doses, c(20, 10))
  expect_equal(length(arm_sims@data), 2L)
  expect_equal(arm_sims@stop_report[, "Minimum patients"], c(TRUE, FALSE))
})

test_that("summary-HierarchicalSimulations returns arm-level summaries", {
  sims <- local_hierarchical_simulations()

  result <- summary(
    sims,
    truth = list(
      arm_a = function(dose, ...) plogis(dose - 20),
      arm_b = function(dose, ...) plogis(dose - 20)
    )
  )

  expect_s4_class(result, "HierarchicalSimulationsSummary")
  expect_equal(result@nsim, 2L)
  expect_named(result@arms, c("arm_a", "arm_b"))
  expect_s4_class(result@arms$arm_a, "SimulationsSummary")
  expect_s4_class(result@arms$arm_b, "SimulationsSummary")
  expect_equal(result@arms$arm_a@dose_selected, c(20, 10))
  expect_equal(result@arms$arm_b@dose_selected, c(0, 0))
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

  expect_true(is(result, "HierarchicalSamples"))
  expect_true(is(result, "Samples"))
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
  expect_equal(
    result@arm_samples,
    list(
      my_mono = c(alpha0 = "alpha0_my_mono", alpha1 = "alpha1_my_mono"),
      my_combo = c(
        alpha0 = "alpha0_my_combo",
        alpha1 = "alpha1_my_combo",
        eta = "eta_my_combo"
      )
    )
  )
})

test_that("armSamples extracts arm-specific Samples objects", {
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
  mono_samples <- armSamples(result, "my_mono")
  combo_samples <- armSamples(result, "my_combo")

  expect_s4_class(mono_samples, "Samples")
  expect_equal(names(mono_samples), c("alpha0", "alpha1"))
  expect_equal(names(combo_samples), c("alpha0", "alpha1", "eta"))
  expect_equal(mono_samples@data$alpha0, result@data$alpha0_my_mono)
  expect_equal(combo_samples@data$alpha0, result@data$alpha0_my_combo)
  expect_equal(combo_samples@data$eta, result@data$eta_my_combo)
})

test_that("fit-HierarchicalSamples delegates to arm-specific fits", {
  result <- fit(
    object = local_hierarchical_samples(),
    model = local_hierarchical_model(),
    data = local_hierarchical_data()
  )

  expect_data_frame(result, nrows = 9L)
  expect_named(
    result,
    c("arm", "dose", "middle", "lower", "upper", "drug1", "drug2")
  )
  expect_equal(
    as.integer(table(result$arm)[c("my_combo", "my_mono")]),
    c(6L, 3L)
  )
  expect_true(all(result$middle >= 0 & result$middle <= 1))
  expect_true(all(is.na(result$dose[result$arm == "my_combo"])))
  expect_true(all(is.na(result$drug1[result$arm == "my_mono"])))
  expect_true(all(is.na(result$drug2[result$arm == "my_mono"])))
})

test_that("plot-HierarchicalSamples delegates to arm-specific plots", {
  result <- plot(
    x = local_hierarchical_samples(),
    y = local_hierarchical_model(),
    data = local_hierarchical_data()
  )

  expect_true(grid::is.grob(result))
})
