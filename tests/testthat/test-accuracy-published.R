test_that("OneParLogNormalPrior reproduces same numbers as in paper by Neuenschwander et al.", {

  mcmc_options <- McmcOptions(
    burnin = 50000,
    step = 2,
    samples = 10000,
    rng_kind = "Wichmann-Hill",
    rng_seed = 1
  )

  # One-parameter model
  dose_grid <- c(
    1, 2.5, 5, 10, 15, 20, 25, 30,
    40, 50, 75, 100, 150, 200, 250
  )

  # (A) Posterior summaries (original skeleton)
  empty_data <- Data(dose_grid = dose_grid)

  data_obs_a <- Data(
    x = c(
      rep(c(1, 2.5, 5, 10, 25),
          times = c(3, 4, 5, 4, 2))
    ),
    y = c(
      rep(c(0, 1),
          times = c(16, 2))
    ),
    cohort = c(
      rep(c(1, 2, 3, 4, 7),
          times = c(3, 4, 5, 4, 2))
    ),
    doseGrid = dose_grid,
    ID = 1:18
  )

  model_power_a <- OneParLogNormalPrior(
    skel_probs = c(
      0.01, 0.015, 0.020, 0.025, 0.03,
      0.04, 0.05, 0.10, 0.17, 0.30,
      0.45, 0.70, 0.80, 0.90, 0.95
    ),
    dose_grid = dose_grid,
    sigma2 = 1.34^2
  )

  prior_samples <- mcmc(
    data = empty_data,
    model = model_power_a,
    options = mcmc_options
  )

  # NCRM rule with loss function
  ncrm_loss <- NextBestNCRMLoss(
    target = c(0.2, 0.35),
    overdose = c(0.35, 0.6),
    unacceptable = c(0.6, 1),
    max_overdose_prob = 0.25,
    losses = c(1, 0, 1, 2)
  )

  increments_no <- IncrementsRelative(
    intervals = c(0, 250),
    increments = c(2, 2)
  )

  post_samples_a <- mcmc(data_obs_a, model_power_a, mcmc_options)

  dose_rec_loss_a <- nextBest(ncrm_loss,
                              doselimit = maxDose(
                                increments_no,
                                data_obs_a
                              ),
                              samples = post_samples_a,
                              model = model_power_a,
                              data = data_obs_a
  )

  # (A) Actual table I
  tab1_a_act <- rbind(
    "Skeleton (CRM)" = c(
      0.01, 0.015, 0.020, 0.025, 0.03,
      0.04, 0.05, 0.10, 0.17, 0.30,
      0.45, 0.70, 0.80, 0.90, 0.95
    )[1:10],
    "Mean" = t(dose_rec_loss_a$probs[, 6])[, 1:10],
    "Std. dev." = t(dose_rec_loss_a$probs[, 7])[, 1:10]
  )


  # (B) Actual table I
  data_obs_b <- data_obs_a
  data_obs_b@doseGrid <- dose_grid[1:10]
  data_obs_b@nGrid <- length(data_obs_b@doseGrid)

  model_power_b <- OneParLogNormalPrior(
    skel_probs = c(
      0.063, 0.125, 0.188, 0.250, 0.313,
      0.375, 0.438, 0.500, 0.563, 0.625
    ),
    dose_grid = dose_grid[1:10],
    sigma2 = 1.34^2
  )

  post_samples_b <- mcmc(data_obs_b, model_power_b, mcmc_options)

  dose_rec_loss_b <- nextBest(ncrm_loss,
                              doselimit = maxDose(
                                increments_no,
                                data_obs_b
                              ),
                              samples = post_samples_b,
                              model = model_power_b,
                              data = data_obs_b
  )

  tab1_b_act <- rbind(
    "Skeleton (CRM)" = c(
      0.01, 0.015, 0.020, 0.025, 0.03,
      0.04, 0.05, 0.10, 0.17, 0.30,
      0.45, 0.70, 0.80, 0.90, 0.95
    )[1:10],
    "Mean" = t(dose_rec_loss_b$probs[, 6])[, 1:10],
    "Std. dev." = t(dose_rec_loss_b$probs[, 7])[, 1:10]
  )

  # (A)+ (B) Actual table I
  tab1_act <- list(
    "Posterior summaries (original skeleton)" = as.data.frame(tab1_a_act),
    "Posterior summaries (equidistant skeleton)" = as.data.frame(tab1_b_act)
  )

  # Expected table I (Neuenschwander et al.)
  tab1 <- structure(list(
    dose1 = c(
      3, 0, NA, 0.01, 0.069, 0.055, NA, 0.063,
      0.024, 0.03
    ),
    dose2.5 = c(
      4, 0, NA, 0.015, 0.085, 0.062, NA, 0.125,
      0.054, 0.051
    ),
    dose5 = c(
      5, 0, NA, 0.02, 0.099, 0.068, NA, 0.188,
      0.09, 0.069
    ),
    dose10 = c(
      4, 0, NA, 0.025, 0.111, 0.072, NA, 0.25,
      0.13, 0.084
    ),
    dose15 = c(
      NA, NA, NA, 0.03, 0.123, 0.076, NA, 0.313,
      0.176, 0.097
    ),
    dose20 = c(
      NA, NA, NA, 0.04, 0.144, 0.082, NA, 0.375,
      0.226, 0.107
    ),
    dose25 = c(
      2, 2, NA, 0.05, 0.163, 0.087, NA, 0.438,
      0.281, 0.115
    ),
    dose30 = c(
      NA, NA, NA, 0.1, 0.242, 0.101, NA, 0.5,
      0.341, 0.119
    ),
    dose40 = c(
      NA, NA, NA, 0.17, 0.33, 0.109, NA, 0.563,
      0.405, 0.12
    ),
    dose50 = c(
      NA, NA, NA, 0.3, 0.465, 0.108, NA, 0.625,
      0.475, 0.117
    )
  ),
  class = "data.frame",
  row.names = c(
    "No. of patients", "No. of DLTs",
    "A) Posterior summaries (original skeleton)",
    "Skeleton (CRM)", "Mean", "Std. dev.",
    "B) Posterior summaries (equidistant skeleton)",
    "Skeleton (CRM)", "Mean", "Std. dev."
  )
  )

  tab1_a_exp <- rbind(
    "Skeleton (CRM)" = tab1[4, ],
    "Mean" = tab1[5, ],
    "Std. dev." = tab1[6, ]
  )

  names(tab1_a_exp) <- colnames(tab1_a_act)

  tab1_b_exp <- rbind(
    "Skeleton (CRM)" = tab1[8, ],
    "Mean" = tab1[9, ],
    "Std. dev." = tab1[10, ]
  )

  names(tab1_b_exp) <- colnames(tab1_b_act)

  # (A)+ (B) Expected table I
  tab1_exp <- list(
    "Posterior summaries (original skeleton)" = tab1_a_exp,
    "Posterior summaries (equidistant skeleton)" = tab1_b_exp
  )

  # test Posterior summaries for probabilities of DLT (CRM)
  # check whether absolute differences between published results and computed
  # results are smaller than chosen tolerance
  tolerance <- 0.01
  # original skeleton
  diff_org_mean <- abs(tab1_act$"Posterior summaries (original skeleton)"[2, ] -
                         tab1_exp$"Posterior summaries (original skeleton)"[2, ]) <
    tolerance

  diff_org_sd <- abs(tab1_act$"Posterior summaries (original skeleton)"[3, ] -
                       tab1_exp$"Posterior summaries (original skeleton)"[3, ]) <
    tolerance

  # if at least one computed result (mean or sd) has deviation larger than
  # tolerance from corresponding published result, set result as FALSE
  if ((FALSE %in% diff_org_mean) == TRUE |
      (FALSE %in% diff_org_sd) == TRUE) {
    result_org <- FALSE
  } else {
    result_org <- TRUE
  }

  # silent if all absolute differences are < tolerance
  expect_true(result_org)

  # equidistant skeleton
  diff_equi_mean <- abs(tab1_act$"Posterior summaries (equidistant skeleton)"[2, ] -
                          tab1_exp$"Posterior summaries (equidistant skeleton)"[2, ]) <
    tolerance

  diff_equi_sd <- abs(tab1_act$"Posterior summaries (equidistant skeleton)"[3, ] -
                        tab1_exp$"Posterior summaries (equidistant skeleton)"[3, ]) <
    tolerance

  if ((FALSE %in% diff_equi_mean) == TRUE |
      (FALSE %in% diff_equi_sd) == TRUE) {
    result_equi <- FALSE
  } else {
    result_equi <- TRUE
  }
  # silent if all absolute differences are < tolerance
  expect_true(result_equi)
})
