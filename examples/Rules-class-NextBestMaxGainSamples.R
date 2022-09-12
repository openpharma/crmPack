# Target probability of the occurrence of a DLT during the trial is set to 0.35.
# Target probability of the occurrence of a DLT at the end of the trial is set to 0.3.
# We want the use the 30% posterior quantile (the 30th percentile) of the TD35
# (the dose level with probability of the DLT equals 0.35) and TD30 samples.
# For `mg_derive` function (which takes the sample of doses which give the maximum
# gain), we will use the 50% posterior quantile (the median or th 50th percentile)
# of the sample.
my_next_best <- NextBestMaxGainSamples(
  prob_target_drt = 0.35,
  prob_target_eot = 0.3,
  derive = function(samples) {
    as.numeric(quantile(samples, prob = 0.3))
  },
  mg_derive = function(mg_samples) {
    as.numeric(quantile(mg_samples, prob = 0.5))
  }
)
