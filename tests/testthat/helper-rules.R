h_next_best_mtd <- function(target = 0.33) {
  NextBestMTD(
    target = target,
    derive = function(mtd_samples) {
      quantile(mtd_samples, probs = 0.25)
    }
  )
}

h_next_best_ncrm <- function(edge_case = FALSE) {
  if (edge_case) {
    NextBestNCRM(
      target = c(0, 1),
      overdose = c(0, 1),
      max_overdose_prob = 0.25
    )
  } else {
    NextBestNCRM(
      target = c(0.2, 0.35),
      overdose = c(0.35, 0.9),
      max_overdose_prob = 0.25
    )
  }
}

h_next_best_ncrm_loss <- function(edge_case = 0L) {
  if (edge_case == 1L) {
    overdose <- c(0.35, 1)
    unacceptable <- c(1, 1)
    losses <- c(1, 0, 1)
  } else if (edge_case == 2L) {
    overdose <- c(0, 0)
    unacceptable <- c(0, 1)
    losses <- c(1, 0, 1, 2)
  } else {
    overdose <- c(0.35, 0.6)
    unacceptable <- c(0.6, 0.9)
    losses <- c(1, 0, 1, 2)
  }

  NextBestNCRMLoss(
    target = c(0.2, 0.35),
    overdose = overdose,
    unacceptable = unacceptable,
    max_overdose_prob = 0.25,
    losses = losses
  )
}

h_next_best_dual_endpoint <- function(target_relative = TRUE, edge_case = FALSE) {
  target <- if (target_relative) {
    if (edge_case) {
      c(0, 1)
    } else {
      c(0.9, 1)
    }
  } else {
    c(200, 300)
  }

  if (edge_case) {
    overdose <- c(0, 1)
    target_thresh <- 0
  } else {
    overdose <- c(0.35, 0.9)
    target_thresh <- 0.01
  }

  NextBestDualEndpoint(
    target = target,
    overdose = overdose,
    max_overdose_prob = 0.25,
    target_relative = target_relative,
    target_thresh = target_thresh
  )
}

h_next_best_tdsamples <- function(td = 0.45, te = 0.4, p = 0.3) {
  NextBestTDsamples(
    prob_target_drt = td,
    prob_target_eot = te,
    derive = function(samples) as.numeric(quantile(samples, probs = p))
  )
}

h_next_best_mgsamples <- function(td = 0.45, te = 0.4, p = 0.3, p_gstar = 0.5) {
  NextBestMaxGainSamples(
    prob_target_drt = td,
    prob_target_eot = te,
    derive = function(s) as.numeric(quantile(s, prob = p)),
    mg_derive = function(s) as.numeric(quantile(s, prob = p_gstar))
  )
}

h_stopping_specific_dose <- function(rule = StoppingTargetProb(target = c(0, 0.3), prob = 0.8),
                                     dose = 80) {
  StoppingSpecificDose(
    rule = rule,
    dose = dose
  )
}

h_stopping_list <- function() {
  StoppingList(
    stop_list = list(
      StoppingMinCohorts(nCohorts = 3),
      StoppingTargetProb(target = c(0.2, 0.35), prob = 0.5),
      StoppingMinPatients(nPatients = 20)
    ),
    summary = any
  )
}
