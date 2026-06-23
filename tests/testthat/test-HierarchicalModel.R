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
      arm_index = 2L,
      latent = "theta_drug2_my_combo[2]",
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

test_that("HierarchicalModel sources single-agent model code generically", {
  result <- HierarchicalModel(
    sub = LogisticLogNormalSub(
      mean = c(1, 5),
      cov = diag(2),
      ref_dose = 2
    ),
    raw = h_get_general_single_agent_no_ref(beta_mean = c(-2, 0.02)),
    exchangeable_parameters = list()
  )
  data_file <- h_jags_write_model(result@datamodel)
  prior_file <- h_jags_write_model(result@priormodel)
  on.exit(unlink(c(data_file, prior_file)))
  read_model <- function(file) {
    gsub("\\s+", " ", paste(readLines(file), collapse = " "))
  }
  data_text <- read_model(data_file)
  prior_text <- read_model(prior_file)

  expect_valid(result, "HierarchicalModel")
  expect_equal(
    h_hierarchical_supported_refs(result@models_to_arms$raw),
    c("beta0", "beta1")
  )
  expect_match(
    data_text,
    "logit\\(p_sub\\[i\\]\\) <- alpha0_sub \\+ alpha1_sub \\* \\(x_sub\\[i\\] - ref_dose_sub\\)" # nolint
  )
  expect_match(
    data_text,
    "logit\\(p_raw\\[i\\]\\) <- beta0_raw \\+ beta1_raw \\* x_raw\\[i\\]"
  )
  expect_match(prior_text, "theta_sub ~ dmnorm\\(mean_sub, prec_sub\\)")
  expect_match(prior_text, "beta0_raw ~ dnorm\\(beta_mean_raw\\[1\\], 1\\)")
  expect_subset(
    c("alpha0_sub", "alpha1_sub", "beta0_raw", "beta1_raw"),
    result@sample
  )
  expect_subset(
    c("mean_sub", "prec_sub", "ref_dose_sub", "beta_mean_raw"),
    names(result@modelspecs(arms = list(), from_prior = FALSE))
  )
})

test_that("HierarchicalModel supports TwoDrugsCombo without alpha parameters", {
  result <- local_hierarchical_no_alpha_combo_model()
  data <- local_hierarchical_no_alpha_combo_data()
  model_data <- h_mcmc_get_hierarchical_data(
    model = result,
    data = data,
    from_prior = FALSE
  )
  prior_file <- h_jags_write_model(result@priormodel)
  data_file <- h_jags_write_model(result@datamodel)
  on.exit(unlink(c(prior_file, data_file)))
  read_model <- function(file) {
    gsub("\\s+", " ", readLines(file))
  }
  prior_text <- paste(read_model(prior_file), collapse = "")
  data_text <- paste(read_model(data_file), collapse = "")

  expect_valid(result, "HierarchicalModel")
  expect_equal(
    h_hierarchical_supported_refs(result@models_to_arms$raw_combo),
    c("beta0[1]", "beta1[1]", "beta0[2]", "beta1[2]")
  )
  expect_subset(
    c(
      "beta0_raw_mono",
      "beta1_raw_mono",
      "beta0_raw_combo",
      "beta1_raw_combo",
      "eta_raw_combo"
    ),
    result@sample
  )
  expect_subset(
    c(
      "beta_mean_raw_mono",
      "beta_mean_drug1_raw_combo",
      "beta_mean_drug2_raw_combo",
      "gamma_raw_combo",
      "tau_raw_combo"
    ),
    names(result@modelspecs(arms = data@arms, from_prior = FALSE))
  )
  expect_subset(
    c("beta0_drug1_raw_combo", "beta1_drug2_raw_combo", "eta_raw_combo"),
    names(result@init(arms = data@arms))
  )
  expect_false(grepl("alpha0", prior_text, fixed = TRUE))
  expect_false(grepl("alpha1", prior_text, fixed = TRUE))
  expect_match(data_text, "beta0_drug1_raw_combo")
  expect_match(data_text, "beta1_drug2_raw_combo")
  expect_equal(model_data$x_raw_combo, data@arms$raw_combo@x)

  samples <- mcmc(
    data = data,
    model = result,
    options = McmcOptions(
      burnin = 10L,
      step = 1L,
      samples = 20L,
      rng_kind = "Mersenne-Twister",
      rng_seed = 12345L
    )
  )
  expect_s4_class(samples, "HierarchicalSamples")
  expect_subset(
    c(
      "beta0_raw_mono",
      "beta1_raw_mono",
      "beta0_raw_combo",
      "beta1_raw_combo",
      "eta_raw_combo"
    ),
    names(samples@data)
  )

  exchangeable_result <- HierarchicalModel(
    raw_mono = h_get_general_single_agent_no_ref(beta_mean = c(-2, 0.02)),
    raw_combo = h_get_two_drugs_combo_no_alpha_no_ref(),
    exchangeable_parameters = list(
      shared_beta0 = list(
        raw_mono = "beta0",
        raw_combo = "beta0[1]"
      ),
      shared_beta1 = list(
        raw_mono = "beta1",
        raw_combo = "beta1[1]"
      )
    )
  )
  exchangeable_prior_file <- h_jags_write_model(exchangeable_result@priormodel)
  on.exit(unlink(exchangeable_prior_file), add = TRUE)
  exchangeable_prior_text <- paste(
    read_model(exchangeable_prior_file),
    collapse = ""
  )

  expect_valid(exchangeable_result, "HierarchicalModel")
  expect_match(
    exchangeable_prior_text,
    "beta0_raw_mono ~ dnorm\\(mu_shared_beta0, pow\\(tau_shared_beta0,\\s*-2\\)\\)"
  )
  expect_match(
    exchangeable_prior_text,
    "beta0_drug1_raw_combo ~ dnorm\\(mu_shared_beta0, pow\\(tau_shared_beta0,\\s*-2\\)\\)" # nolint
  )
  expect_match(
    exchangeable_prior_text,
    "beta1_raw_mono ~ dnorm\\(mu_shared_beta1, pow\\(tau_shared_beta1,\\s*-2\\)\\)"
  )
  expect_match(
    exchangeable_prior_text,
    "beta1_drug1_raw_combo ~ dnorm\\(mu_shared_beta1, pow\\(tau_shared_beta1,\\s*-2\\)\\)" # nolint
  )
  expect_false(grepl(
    "beta0_raw_mono ~ dnorm(beta_mean_raw_mono[1], 1)",
    exchangeable_prior_text,
    fixed = TRUE
  ))
  expect_false(grepl(
    "beta0_drug1_raw_combo ~ dnorm(beta_mean_drug1_raw_combo[1], 1)",
    exchangeable_prior_text,
    fixed = TRUE
  ))

  exchangeable_samples <- mcmc(
    data = data,
    model = exchangeable_result,
    options = McmcOptions(
      burnin = 10L,
      step = 1L,
      samples = 20L,
      rng_kind = "Mersenne-Twister",
      rng_seed = 12345L
    )
  )
  expect_s4_class(exchangeable_samples, "HierarchicalSamples")
  expect_subset(
    c(
      "mu_shared_beta0",
      "tau_shared_beta0",
      "mu_shared_beta1",
      "tau_shared_beta1"
    ),
    names(exchangeable_samples@data)
  )
})

test_that("parallel mono and combo prior compiles generically", {
  model <- local_parallel_hierarchical_model()
  body_priormodel <- gsub(
    "\\s+",
    " ",
    paste(deparse(body(model@priormodel)), collapse = "\n")
  )
  prior_specs <- model@modelspecs(arms = list(), from_prior = TRUE)
  inits <- model@init(arms = list())

  expect_true(grepl(
    "theta_mono_drug1[1] ~ dnorm(mu_drug1_intercept, pow(tau_drug1_intercept, -2))",
    body_priormodel,
    fixed = TRUE
  ))
  expect_true(grepl(
    "theta_drug1_combo[1] ~ dnorm(mu_drug1_intercept, pow(tau_drug1_intercept, -2))",
    body_priormodel,
    fixed = TRUE
  ))
  expect_true(grepl(
    "theta_mono_drug2[2] ~ dnorm(mu_drug2_slope, pow(tau_drug2_slope, -2))",
    body_priormodel,
    fixed = TRUE
  ))
  expect_true(grepl(
    "theta_drug2_combo[2] ~ dnorm(mu_drug2_slope, pow(tau_drug2_slope, -2))",
    body_priormodel,
    fixed = TRUE
  ))
  expect_true(grepl(
    "eta_combo ~ dnorm(gamma_combo, tau_combo)",
    body_priormodel,
    fixed = TRUE
  ))
  expect_false(grepl("rho_drug1", body_priormodel, fixed = TRUE))
  expect_identical(
    names(prior_specs),
    c("kappa_hier", "gamma_combo", "tau_combo")
  )
  expect_true(all(
    c(
      "mu_drug1_intercept",
      "tau_drug1_intercept",
      "mu_drug2_slope",
      "tau_drug2_slope"
    ) %in%
      model@sample
  ))
  expect_true(all(
    c(
      "theta_mono_drug1",
      "theta_drug1_combo",
      "eta_combo",
      "mu_drug1_intercept",
      "tau_drug2_slope"
    ) %in%
      names(inits)
  ))
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
  expect_true(result@arms$arm_a@borrow)

  # TODO: add tests for arm opening rules once hierarchical accrual is implemented.
})

test_that("DesignArm constructor stores borrow flag", {
  arm <- DesignArm(
    name = "arm_no_borrow",
    active = TRUE,
    design = .DefaultDesign(),
    borrow = FALSE
  )

  expect_valid(arm, "DesignArm")
  expect_false(arm@borrow)
})

test_that("ArmCondition constructors and logical operators work", {
  no_condition <- expect_silent(NoArmCondition())
  finished_condition <- expect_silent(ArmFinishedCondition("arm_a"))
  min_dose_condition <- expect_silent(ArmMinDoseCondition("my_mono", 20))

  expect_valid(no_condition, "NoArmCondition")
  expect_valid(finished_condition, "ArmFinishedCondition")
  expect_valid(min_dose_condition, "ArmMinDoseCondition")
  expect_true(openArm(
    no_condition,
    data = local_hierarchical_design()@data
  ))
  expect_true(openArm(
    finished_condition,
    data = local_hierarchical_design()@data,
    finished_arms = c(arm_a = TRUE, arm_b = FALSE)
  ))
  expect_false(openArm(
    finished_condition,
    data = local_hierarchical_design()@data,
    finished_arms = c(arm_a = FALSE, arm_b = FALSE)
  ))
  expect_true(openArm(
    min_dose_condition,
    data = local_hierarchical_data()
  ))
  expect_false(openArm(
    ArmMinDoseCondition("my_mono", 30),
    data = local_hierarchical_data()
  ))
  expect_true(openArm(
    ArmMinDoseCondition("my_combo", c(20, 40)),
    data = local_hierarchical_data()
  ))
  expect_false(openArm(
    ArmMinDoseCondition("my_combo", c(30, 40)),
    data = local_hierarchical_data()
  ))

  expect_valid(no_condition & finished_condition, "ArmConditionAll")
  expect_valid(finished_condition | min_dose_condition, "ArmConditionAny")
})

test_that("DesignArm constructor stores opening condition", {
  condition <- ArmFinishedCondition("arm_a")
  arm <- DesignArm(
    name = "arm_delayed",
    active = TRUE,
    design = .DefaultDesign(),
    open_when = condition
  )

  expect_valid(arm, "DesignArm")
  expect_identical(arm@open_when, condition)
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

test_that("HierarchicalDesign simulate uses arm-level mcmc for non-borrowing arms", {
  mono_design <- Design(
    model = local_hierarchical_mono_model(),
    nextBest = NextBestNCRM(
      target = c(0.2, 0.35),
      overdose = c(0.35, 1),
      max_overdose_prob = 0.25
    ),
    stopping = StoppingMinPatients(nPatients = 1L),
    increments = IncrementsRelative(intervals = c(0), increments = c(1)),
    cohort_size = CohortSizeConst(size = 1L),
    data = Data(doseGrid = c(10, 20, 30)),
    startingDose = 10
  )
  combo_design <- DesignCombo(
    model = local_hierarchical_combo_model(),
    nextBest = NextBestNCRM(
      target = c(0.2, 0.35),
      overdose = c(0.35, 1),
      max_overdose_prob = 0.25
    ),
    stopping = StoppingMinPatients(nPatients = 1L),
    increments = IncrementsComboCartesian(
      drug1 = IncrementsRelative(intervals = c(0), increments = c(1)),
      drug2 = IncrementsRelative(intervals = c(0), increments = c(1))
    ),
    cohort_size = CohortSizeConst(size = 1L),
    data = DataCombo(
      doseGrid = list(
        drug1 = c(10, 20, 30),
        drug2 = c(20, 40)
      )
    ),
    startingDose = c(drug1 = 10, drug2 = 20)
  )
  design <- HierarchicalDesign(
    DesignArm(
      name = "my_mono",
      active = TRUE,
      design = mono_design,
      borrow = FALSE
    ),
    DesignArm(
      name = "my_combo",
      active = TRUE,
      design = combo_design,
      borrow = TRUE
    ),
    exchangeable_parameters = local_hierarchical_parameter_pools()
  )

  # Track calls to mcmc to check that the mono arm uses hierarchical mcmc
  # and the combo arm uses regular mcmc.
  mcmc_calls <- list()
  testthat::local_mocked_bindings(
    mcmc = function(data, model, options) {
      mcmc_calls[[length(mcmc_calls) + 1L]] <<- list(
        data_class = class(data)[1L],
        model_class = class(model)[1L]
      )
      if (is(data, "HierarchicalData")) {
        local_hierarchical_samples()
      } else {
        Samples(
          data = list(
            alpha0 = rep(-3, size(options)),
            alpha1 = rep(1, size(options))
          ),
          options = options
        )
      }
    }
  )

  result <- simulate(
    design,
    truth = function(dose, ...) 0.1,
    truthResponse = function(dose, ...) 0.5,
    nsim = 1L,
    seed = 123L,
    mcmcOptions = h_get_mcmc_options(samples = 2L),
    parallel = FALSE
  )

  expect_valid(result, "HierarchicalSimulations")
  expect_s4_class(result@samples[[1L]], "HierarchicalSamples")
  expect_identical(
    vapply(mcmc_calls, `[[`, character(1L), "data_class"),
    c("HierarchicalData", "Data", "HierarchicalData", "Data")
  )
  expect_false(any(vapply(
    mcmc_calls,
    function(call) identical(call$data_class, "DataCombo"),
    logical(1L)
  )))
})

test_that("HierarchicalDesign simulate opens delayed arms once and keeps them open", {
  if (!isClass("CountingArmCondition")) {
    setClass("CountingArmCondition", contains = "ArmCondition")
  }

  condition_env <- new.env(parent = emptyenv())
  condition_env$calls <- 0L
  setMethod(
    f = "openArm",
    signature = c(condition = "CountingArmCondition"),
    definition = function(condition, data, ...) {
      condition_env$calls <- condition_env$calls + 1L
      condition_env$calls >= 3L
    }
  )

  design <- local_hierarchical_design()
  design@arms$arm_b@active <- TRUE
  design@arms$arm_b@open_when <- new("CountingArmCondition")
  for (arm_name in names(design@arms)) {
    design@arms[[arm_name]]@borrow <- FALSE
    design@arms[[arm_name]]@design@stopping <-
      StoppingMinPatients(nPatients = 1L)
    design@arms[[arm_name]]@design@cohort_size <- CohortSizeConst(size = 1L)
  }

  testthat::local_mocked_bindings(
    mcmc = function(data, model, options) {
      if (is(data, "HierarchicalData")) {
        HierarchicalSamples(
          data = list(
            alpha0_arm_a = rep(-3, size(options)),
            alpha1_arm_a = rep(1, size(options)),
            alpha0_arm_b = rep(-3, size(options)),
            alpha1_arm_b = rep(1, size(options))
          ),
          options = options,
          arm_samples = list(
            arm_a = c(alpha0 = "alpha0_arm_a", alpha1 = "alpha1_arm_a"),
            arm_b = c(alpha0 = "alpha0_arm_b", alpha1 = "alpha1_arm_b")
          )
        )
      } else {
        Samples(
          data = list(
            alpha0 = rep(-3, size(options)),
            alpha1 = rep(1, size(options))
          ),
          options = options
        )
      }
    }
  )

  result <- simulate(
    design,
    truth = function(dose, ...) 0.1,
    truthResponse = function(dose, ...) 0.5,
    nsim = 1L,
    seed = 123L,
    mcmcOptions = h_get_mcmc_options(samples = 2L),
    parallel = FALSE
  )

  expect_valid(result, "HierarchicalSimulations")
  expect_equal(result@data[[1L]]@arms$arm_a@nObs, 1L)
  expect_equal(result@data[[1L]]@arms$arm_b@nObs, 1L)
  expect_identical(condition_env$calls, 3L)
})

test_that("ArmFinishedCondition opens an arm after the referenced arm stops", {
  design <- local_hierarchical_design()
  design@arms$arm_b@active <- TRUE
  design@arms$arm_b@open_when <- ArmFinishedCondition("arm_a")
  for (arm_name in names(design@arms)) {
    design@arms[[arm_name]]@borrow <- FALSE
    design@arms[[arm_name]]@design@stopping <-
      StoppingMinPatients(nPatients = 1L)
    design@arms[[arm_name]]@design@cohort_size <- CohortSizeConst(size = 1L)
  }

  testthat::local_mocked_bindings(
    mcmc = function(data, model, options) {
      if (is(data, "HierarchicalData")) {
        HierarchicalSamples(
          data = list(
            alpha0_arm_a = rep(-3, size(options)),
            alpha1_arm_a = rep(1, size(options)),
            alpha0_arm_b = rep(-3, size(options)),
            alpha1_arm_b = rep(1, size(options))
          ),
          options = options,
          arm_samples = list(
            arm_a = c(alpha0 = "alpha0_arm_a", alpha1 = "alpha1_arm_a"),
            arm_b = c(alpha0 = "alpha0_arm_b", alpha1 = "alpha1_arm_b")
          )
        )
      } else {
        Samples(
          data = list(
            alpha0 = rep(-3, size(options)),
            alpha1 = rep(1, size(options))
          ),
          options = options
        )
      }
    }
  )

  result <- simulate(
    design,
    truth = function(dose, ...) 0.1,
    truthResponse = function(dose, ...) 0.5,
    nsim = 1L,
    seed = 123L,
    mcmcOptions = h_get_mcmc_options(samples = 2L),
    parallel = FALSE
  )

  expect_valid(result, "HierarchicalSimulations")
  expect_equal(result@data[[1L]]@arms$arm_a@nObs, 1L)
  expect_equal(result@data[[1L]]@arms$arm_b@nObs, 1L)
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

  expect_true(
    "get_arm_simulations" %in% getNamespaceExports("crmPack")
  )

  arm_sims <- get_arm_simulations(sims, "arm_a")

  expect_s4_class(arm_sims, "Simulations")
  expect_equal(arm_sims@doses, c(20, 10))
  expect_equal(length(arm_sims@data), 2L)
  expect_equal(arm_sims@stop_report[, "Minimum patients"], c(TRUE, FALSE))
})

test_that("hierarchical summary helpers rebuild combo arm simulations", {
  combo_data <- DataCombo(
    x = cbind(drug1 = c(10, 20), drug2 = c(20, 40)),
    y = c(0L, 1L),
    doseGrid = list(drug1 = c(10, 20), drug2 = c(20, 40)),
    ID = 1L:2L,
    cohort = 1L:2L
  )
  sims <- HierarchicalSimulations(
    data = list(HierarchicalData(arms = list(combo = combo_data))),
    doses = list(list(combo = c(drug1 = 20, drug2 = 40))),
    samples = list(HierarchicalSamples(
      data = list(alpha0_combo = c(-3, -2)),
      options = McmcOptions(burnin = 1L, step = 1L, samples = 2L),
      arm_samples = list(combo = c(alpha0 = "alpha0_combo"))
    )),
    fit = list(list(
      combo = data.frame(
        drug1 = c(10, 20),
        drug2 = c(20, 40),
        middle = c(0.1, 0.3),
        lower = c(0.05, 0.2),
        upper = c(0.2, 0.4)
      )
    )),
    stop_reasons = list(list(combo = "Stopped combo")),
    stop_report = list(list(combo = c(rule = TRUE))),
    additional_stats = list(list(combo = list())),
    seed = 123L
  )

  combo_sims <- get_arm_simulations(sims, "combo")

  expect_s4_class(combo_sims, "ComboSimulations")
  expect_equal(combo_sims@doses, cbind(drug1 = 20, drug2 = 40))
  expect_s4_class(combo_sims@data[[1L]], "DataCombo")
  expect_equal(unname(combo_sims@stop_report[, "rule"]), TRUE)
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
