local_hierarchical_mono_model <- function() {
  LogisticLogNormal(
    mean = c(-0.85, 1),
    cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
    ref_dose = 10
  )
}

local_hierarchical_combo_model <- function(log_normal_eta = FALSE) {
  TwoDrugsCombo(
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

local_hierarchical_no_alpha_combo_model <- function() {
  HierarchicalModel(
    raw_mono = h_get_general_single_agent_no_ref(beta_mean = c(-2, 0.02)),
    raw_combo = h_get_two_drugs_combo_no_alpha_no_ref(),
    exchangeable_parameters = list()
  )
}

local_hierarchical_no_alpha_combo_data <- function() {
  HierarchicalData(
    raw_mono = Data(
      x = c(10, 20),
      y = c(0L, 1L),
      doseGrid = c(10, 20),
      ID = 1L:2L,
      cohort = 1L:2L
    ),
    raw_combo = DataCombo(
      x = cbind(
        drug1 = c(10, 20),
        drug2 = c(20, 40)
      ),
      y = c(0L, 1L),
      doseGrid = list(
        drug1 = c(10, 20),
        drug2 = c(20, 40)
      ),
      ID = 1L:2L,
      cohort = 1L:2L
    )
  )
}

local_parallel_hierarchical_model <- function(
  log_normal_eta = FALSE,
  pool_correlations = list(),
  pool_priors = list()
) {
  combo_model <- local_hierarchical_combo_model(log_normal_eta = log_normal_eta)

  HierarchicalModel(
    mono_drug1 = combo_model@single_models$drug1,
    mono_drug2 = combo_model@single_models$drug2,
    combo = combo_model,
    exchangeable_parameters = list(
      drug1_intercept = list(
        mono_drug1 = "alpha0",
        combo = "alpha0[1]"
      ),
      drug1_slope = list(
        mono_drug1 = "alpha1",
        combo = "alpha1[1]"
      ),
      drug2_intercept = list(
        mono_drug2 = "alpha0",
        combo = "alpha0[2]"
      ),
      drug2_slope = list(
        mono_drug2 = "alpha1",
        combo = "alpha1[2]"
      )
    ),
    pool_correlations = pool_correlations,
    pool_priors = pool_priors
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
    ),
    HierarchicalData(
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
