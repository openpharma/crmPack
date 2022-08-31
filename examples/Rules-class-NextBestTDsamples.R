# Target probability of the occurrence of a DLT during the trial is set to 0.35.
# Target probability of the occurrence of a DLT at the end of the trial is set to 0.3.
# We want the use the 30% posterior quantile (the 30th percentile) of the TD35
# (the dose level with probability of the DLT equals 0.35) and TD30 samples.
my_next_best <- NextBestTDsamples(
  prob_target_drt = 0.35,
  prob_target_eot = 0.3,
  derive = function(dose_samples) {
    quantile(dose_samples, probs = 0.3)
  }
)
