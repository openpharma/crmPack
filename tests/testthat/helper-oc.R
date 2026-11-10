#' Deterministic Simulation Scenarios for Registry Tests
#' @return A model-based simulation object with overlapping stopping outcomes.
#' @keywords internal
oc_test_sims <- function() {
  grid <- c(1, 2, 4)
  data <- list(
    Data(x = c(1, 1, 2), y = c(0, 1, 1), doseGrid = grid),
    Data(x = c(2, 2), y = c(0, 1), doseGrid = grid),
    Data(doseGrid = grid)
  )
  report <- cbind(
    unsafe = c(TRUE, TRUE, FALSE),
    unsafe2 = c(TRUE, FALSE, FALSE),
    size = c(FALSE, TRUE, TRUE),
    precision = c(FALSE, FALSE, TRUE)
  )
  attr(report, "stopping_classes") <- c(
    unsafe = "StoppingLowestDoseHSRBeta",
    unsafe2 = "StoppingLowestDoseHSRBeta",
    size = "StoppingMinPatients",
    precision = "StoppingTDCIRatio"
  )
  Simulations(
    data = data,
    doses = c(1, 2, NA_real_),
    seed = 1L,
    truth = function(dose) dose / 10,
    fit = rep(
      list(data.frame(
        dose = grid,
        middle = grid / 10,
        lower = grid / 20,
        upper = grid / 5
      )),
      3
    ),
    stop_report = report,
    stop_reasons = rep(list("test"), 3),
    additional_stats = list(
      list(score = 1, curve = 1:2),
      list(score = 2, curve = 1:2),
      list(score = NA_real_, curve = 1:2)
    ),
    overdose_prob = rep(0.1, 3)
  )
}
