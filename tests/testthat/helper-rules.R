h_next_best_mtd <- function(target = 0.33) {
  NextBestMTD(
    target = target,
    derive = function(mtd_samples) {
      quantile(mtd_samples, probs = 0.25)
    }
  )
}

h_next_best_ncrm <- function() {
  NextBestNCRM(
    target = c(0.2, 0.35),
    overdose = c(0.35, 1),
    max_overdose_prob = 0.25
  )
}

h_next_best_ncrm_loss <- function() {
  NextBestNCRMLoss(
    target = c(0.2, 0.35),
    overdose = c(0.35, 0.6),
    unacceptable = c(0.6, 1),
    max_overdose_prob = 0.25,
    losses = c(1, 0, 1, 2)
  )
}

h_next_best_dual_endpoint <- function(target_relative = TRUE) {
  target <- if (target_relative) {
    c(0.9, 1)
  } else {
    c(200, 300)
  }

  NextBestDualEndpoint(
    target = target,
    target_relative = target_relative,
    overdose = c(0.35, 1),
    max_overdose_prob = 0.25
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
