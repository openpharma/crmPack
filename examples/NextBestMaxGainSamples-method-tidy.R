NextBestMaxGainSamples(
  prob_target_drt = 0.3,
  prob_target_eot = 0.4,
  derive = function(dose_samples) {
    as.numeric(quantile(dose_samples, prob = 0.3))
  },
  mg_derive = function(dose_samples) {
    as.numeric(quantile(dose_samples, prob = 0.5))
  }
) %>% tidy()
