local_scenario_mono_design <- function(data, n_patients = 10L) {
  Design(
    model = local_hierarchical_mono_model(),
    nextBest = NextBestNCRM(
      target = c(0.2, 0.35),
      overdose = c(0.35, 1),
      max_overdose_prob = 0.25
    ),
    stopping = StoppingMinPatients(nPatients = n_patients),
    increments = IncrementsRelative(intervals = c(0), increments = c(1)),
    cohort_size = CohortSizeConst(3),
    data = data,
    startingDose = min(data@doseGrid)
  )
}

local_scenario_combo_design <- function(data, n_patients = 10L) {
  DesignCombo(
    model = local_hierarchical_combo_model(),
    nextBest = NextBestNCRM(
      target = c(0.2, 0.35),
      overdose = c(0.35, 1),
      max_overdose_prob = 0.25
    ),
    stopping = StoppingMinPatients(nPatients = n_patients),
    increments = IncrementsComboCartesian(
      drug1 = IncrementsRelative(intervals = c(0), increments = c(1)),
      drug2 = IncrementsRelative(intervals = c(0), increments = c(1))
    ),
    cohort_size = CohortSizeConst(3),
    data = data,
    startingDose = vapply(data@doseGrid, min, numeric(1L))
  )
}

local_comparison_decider_hierarchical_design <- function() {
  d1 <- c(0.1, 0.2, 0.4, 0.8, 1.6, 2.4, 3.6, 5, 6)
  d2 <- c(8, 12)
  dose_ref1 <- 6
  dose_ref2 <- 12

  prior_mu <- list(
    mu_a1 = c(qlogis(0.33), 2),
    mu_b1 = c(0, 1),
    mu_a2 = c(qlogis(0.33), 2),
    mu_b2 = c(0, 1)
  )
  prior_tau <- list(
    tau_a1 = c(log(0.25), log(2) / 1.96),
    tau_b1 = c(log(0.125), log(2) / 1.96),
    tau_a2 = c(log(0.25), log(2) / 1.96),
    tau_b2 = c(log(0.125), log(2) / 1.96)
  )

  mono_model1 <- LogisticLogNormal(
    mean = c(qlogis(0.33), 0),
    cov = diag(c(2, 1)^2),
    ref_dose = dose_ref1
  )
  mono_model2 <- LogisticLogNormal(
    mean = c(qlogis(0.33), 0),
    cov = diag(c(2, 1)^2),
    ref_dose = dose_ref2
  )
  combo_model <- TwoDrugsCombo(
    list(
      compound1 = mono_model1,
      compound2 = mono_model2
    ),
    gamma = 0,
    tau = 1 / (1.121^2)
  )

  historical_data <- list(
    dose2 = c(2, 4, 8, 12, 16),
    n.pat = c(3, 3, 3, 9, 12),
    n.dlt = c(0, 0, 0, 1, 2)
  )
  hist_data_comp2 <- Data(
    x = rep(historical_data$dose2, historical_data$n.pat),
    y = c(
      rep(0, sum(historical_data$n.pat) - sum(historical_data$n.dlt)),
      rep(1, sum(historical_data$n.dlt))
    ),
    doseGrid = historical_data$dose2
  )

  my_stopping <- StoppingMinPatients(nPatients = 50)
  my_increments <- IncrementsRelative(0, 2)
  my_next_best <- NextBestNCRM(
    target = c(0.16, 0.33),
    overdose = c(0.33, 1),
    max_overdose_prob = 0.25
  )
  my_cohort_size <- CohortSizeConst(size = 3)
  my_increments_combo <- IncrementsComboOneDrugOnly()

  design_armA <- DesignArm(
    "A",
    active = TRUE,
    design = Design(
      data = Data(doseGrid = d1),
      startingDose = d1[1],
      model = mono_model1,
      stopping = my_stopping,
      increments = my_increments,
      nextBest = my_next_best,
      cohort_size = my_cohort_size
    )
  )

  design_armB <- DesignArm(
    "B",
    active = TRUE,
    design = DesignCombo(
      data = DataCombo(doseGrid = list(compound1 = d1, compound2 = c(0, d2))),
      startingDose = c(compound1 = d1[1], compound2 = 0),
      model = combo_model,
      stopping = my_stopping,
      increments = my_increments_combo,
      nextBest = my_next_best,
      cohort_size = my_cohort_size
    ),
    open_when = ArmMinDoseCondition("A", min_dose = d1[2])
  )

  design_armC <- DesignArm(
    "C",
    active = FALSE,
    data = hist_data_comp2,
    model = mono_model2
  )

  design <- HierarchicalDesign(
    design_armA,
    design_armB,
    design_armC,
    exchangeable_parameters = list(
      comp1_intercept = list(
        A = "alpha0",
        B = "alpha0[1]"
      ),
      comp1_slope = list(
        A = "alpha1",
        B = "alpha1[1]"
      ),
      comp2_intercept = list(
        B = "alpha0[2]",
        C = "alpha0"
      ),
      comp2_slope = list(
        B = "alpha1[2]",
        C = "alpha1"
      )
    ),
    pool_correlations = list(
      comp1 = c("comp1_intercept", "comp1_slope"),
      comp2 = c("comp2_intercept", "comp2_slope")
    ),
    pool_priors = list(
      comp1_intercept = list(
        mu = prior_mu$mu_a1,
        tau = prior_tau$tau_a1
      ),
      comp1_slope = list(
        mu = prior_mu$mu_b1,
        tau = prior_tau$tau_b1
      ),
      comp2_intercept = list(
        mu = prior_mu$mu_a2,
        tau = prior_tau$tau_a2
      ),
      comp2_slope = list(
        mu = prior_mu$mu_b2,
        tau = prior_tau$tau_b2
      )
    )
  )

  data <- HierarchicalData(
    A = Data(
      x = c(0.1, 0.1, 0.1, 0.2, 0.2, 0.2),
      y = c(0, 0, 0, 0, 0, 1),
      doseGrid = design_armA@design@data@doseGrid
    ),
    B = design_armB@design@data,
    C = design_armC@design@data
  )

  list(
    design = design,
    data = data
  )
}
