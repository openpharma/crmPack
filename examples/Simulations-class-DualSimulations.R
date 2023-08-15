data_list <- list(
  Data(
    x = 1:2,
    y = 0:1,
    doseGrid = 1:2,
    ID = 1L:2L,
    cohort = 1L:2L
  ),
  Data(
    x = 3:4,
    y = 0:1,
    doseGrid = 3:4,
    ID = 1L:2L,
    cohort = 1L:2L
  )
)

doses <- c(1, 2)
seed <- as.integer(123)

fit <- list(
  c(0.1, 0.2),
  c(0.3, 0.4)
)

stop_report <- matrix(c(TRUE, FALSE), nrow = 2)

stop_reasons <- list("A", "B")

dual_simulations_obj <- DualSimulations(
  rho_est = c(0.25, 0.35),
  sigma2w_est = c(0.15, 0.25),
  fit_biomarker = list(c(0.3, 0.4), c(0.4, 0.5)),
  fit = fit,
  stop_report = stop_report,
  stop_reasons = stop_reasons,
  data = data_list,
  doses = doses,
  seed = seed
)
