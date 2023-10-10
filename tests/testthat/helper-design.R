h_get_design_dualdata <- function(placebo = FALSE) {
  # Define the dose-grid
  emptydata <- DataDual(
    doseGrid = c(1, 3, 5, 10, 15, 20, 25, 40, 50, 80, 100),
    placebo= placebo
  )

  # Initialize the CRM model
  model <- DualEndpointRW(
    mean = c(0, 1),
    cov = matrix(c(1, 0, 0, 1), nrow = 2),
    sigma2betaW = 0.01,
    sigma2W = c(a = 0.1, b = 0.1),
    use_log_dose = TRUE,
    ref_dose = 2,
    rho = c(a = 1, b = 1),
    rw1 = TRUE
  )

  # Choose the rule for selecting the next dose
  myNextBest <- NextBestDualEndpoint(
    target = c(0.9, 1),
    overdose = c(0.35, 1),
    max_overdose_prob = 0.25
  )

  # Choose the rule for stopping
  myStopping4 <- StoppingTargetBiomarker(
    target = c(0.9, 1),
    prob = 0.5
  )
  myStopping <- myStopping4 | StoppingMinPatients(10)

  # Choose the rule for dose increments
  myIncrements <- IncrementsRelative(
    intervals = c(0, 20),
    increments = c(1, 0.33)
  )

  # Initialize the design
  design <- DualDesign(
    model = model,
    data = emptydata,
    nextBest = myNextBest,
    stopping = myStopping,
    increments = myIncrements,
    cohort_size = CohortSizeConst(3),
    startingDose = 3
  )
  if (placebo) {
    design@pl_cohort_size <- CohortSizeConst(1)
  }
  design
}


h_get_design_data <- function(placebo = FALSE) {
  # Define the dose-grid
  emptydata <- Data(
    doseGrid = c(0.1, 1, 3, 5, 10, 15, 20, 25, 40, 50, 80, 100),
    placebo = placebo)

  # Initialize the CRM model
  model <- LogisticLogNormal(
    mean = c(-0.85, 1),
    cov =
      matrix(c(1, -0.5, -0.5, 1),
             nrow = 2
      ),
    ref_dose = 56
  )

  # Choose the rule for selecting the next dose
  myNextBest <- NextBestNCRM(
    target = c(0.2, 0.35),
    overdose = c(0.35, 1),
    max_overdose_prob = 0.25
  )

  # Choose the rule for stopping
  myStopping1 <- StoppingMinCohorts(nCohorts = 3)
  myStopping2 <- StoppingTargetProb(
    target = c(0.2, 0.35),
    prob = 0.5
  )
  myStopping3 <- StoppingMinPatients(nPatients = 20)
  myStopping <- (myStopping1 & myStopping2) | myStopping3

  # Choose the rule for dose increments
  myIncrements <- IncrementsRelative(
    intervals = c(0, 20, 50),
    increments = c(1, 0.67, 0.33)
  )

  # Initialize the design
  design <- Design(
    model = model,
    nextBest = myNextBest,
    stopping = myStopping,
    increments = myIncrements,
    cohort_size = CohortSizeConst(3),
    data = emptydata,
    startingDose = 3
  )
  if (placebo) {
    design@pl_cohort_size <- CohortSizeConst(1)
  }
  design
}
