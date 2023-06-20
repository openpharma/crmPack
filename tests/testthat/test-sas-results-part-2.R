test_that("Posterior summaries for probabilities of
          DLT (2-parameter logistic model) and recommended doses (NCRMLoss):
          crmPack vs. SAS - Example 1", {
  skip_on_cran()

  set.seed(0304191413)
  mcmc_options <- McmcOptions(
    burnin = 5000,
    step = 2,
    samples = 100000,
    rng_kind = "Wichmann-Hill",
    rng_seed = 1
  )

  dose_grid_sc3 <- c(10, 15, 30, 45, 60, 90, 120, 150, 180, 240)

  data <- Data(
    x = c(rep(15, 3)), y = c(rep(0, 2), 1),
    cohort = c(rep(1, 3)),
    doseGrid = dose_grid_sc3,
    ID = 1:3
  )

  model_bcrm_sc3 <- LogisticLogNormal(
    mean = c(-0.8473, -0.0935),
    cov = matrix(c(1.489^2, 0, 0, 1.275^2), nrow = 2),
    ref_dose = 150
  )

  increment1 <- IncrementsRelative(
    intervals = c(0, 60),
    increments = c(1, 0.67)
  )

  increment2 <- IncrementsRelativeDLT(
    dlt_intervals = c(0, 1),
    increments = c(1, 0.50)
  )

  combIncrement <- IncrementsMin(increments_list = list(increment1, increment2))

  ncrm_loss_sc3 <- NextBestNCRMLoss(
    target = c(0.2, 0.35),
    overdose = c(0.35, 0.6),
    unacceptable = c(0.6, 1),
    max_overdose_prob = 0.25,
    losses = c(1, 0, 1, 2)
  )

  ncrm_sc3 <- NextBestNCRM(
    target = c(0.2, 0.35),
    overdose = c(0.35, 1),
    max_overdose_prob = 0.25
  )

  postSamples <- mcmc(data, model_bcrm_sc3, mcmc_options)

  dose_rec_loss <- expect_silent(nextBest(ncrm_loss_sc3,
    doselimit = maxDose(combIncrement, data),
    postSamples, model_bcrm_sc3, data
  ))

  dose_rec <- expect_silent(nextBest(ncrm_sc3,
    doselimit = maxDose(combIncrement, data),
    postSamples, model_bcrm_sc3, data
  ))

  rec_dose_loss_sc3 <- dose_rec_loss$value
  rec_dose_sc3 <- dose_rec$value

  prob_samples_mat <- matrix(
    nrow = size(postSamples@options),
    ncol = data@nGrid
  )

  # evaluate the probs, for all samples
  for (i in seq_len(data@nGrid)) {
    prob_samples_mat[, i] <- prob(
      dose = data@doseGrid[i],
      model_bcrm_sc3,
      postSamples
    )
  }
  pq75 <- apply(prob_samples_mat, 2, function(x) quantile(x, 0.75))

  res_sc3 <- cbind(
    LOSS = dose_rec_loss$probs[, "posterior_loss"],
    PTARGET = dose_rec_loss$probs[, "target"],
    POVEREX = dose_rec_loss$probs[, "excessive"],
    POVERUN = dose_rec_loss$probs[, "unacceptable"],
    POVER = rowSums(dose_rec_loss$probs[, c("excessive", "unacceptable")]),
    PMEAN = dose_rec_loss$probs[, "mean"],
    PQ75 = pq75
  )

  # Posterior summaries computed by SAS
  temp <- read.csv2(test_path("testdata/sc3_sit1.csv"),
    header = TRUE, dec = "."
  )
  sas_sc3 <- apply(as.matrix(temp[, -1]), 2, as.numeric)
  rownames(sas_sc3) <- temp[, 1]

  # Compare posterior summaries for probabilities of DLT: crmPack vs. SAS
  all_true <- c(FALSE)
  all_true <- all(abs(res_sc3 - sas_sc3) < 0.01)

  expect_true(all_true)

  # Recommended dose computed by SAS
  sas_dose_rec <- 15
  # compare the recommended doses: crmPack vs. SAS
  expect_equal(rec_dose_sc3, sas_dose_rec, tolerance = 0)
})

test_that("Posterior summaries for probabilities of
          DLT (2-parameter logistic model) and recommended doses (NCRMLoss):
          crmPack vs. SAS - Example 2", {
  skip_on_cran()

  set.seed(0304191413)
  mcmc_options <- McmcOptions(
    burnin = 5000,
    step = 2,
    samples = 100000,
    rng_kind = "Wichmann-Hill",
    rng_seed = 1
  )

  dose_grid_sc3 <- c(10, 15, 30, 45, 60, 90, 120, 150, 180, 240)

  data <- Data(
    x = c(rep(15, 3), rep(30, 3)),
    y = c(rep(0, 3), rep(0, 2), 1),
    cohort = c(rep(1, 3), rep(2, 3)),
    doseGrid = dose_grid_sc3,
    ID = 1:6
  )

  model_bcrm_sc3 <- LogisticLogNormal(
    mean = c(-0.8473, -0.0935),
    cov = matrix(c(1.489^2, 0, 0, 1.275^2), nrow = 2),
    ref_dose = 150
  )

  increment1 <- IncrementsRelative(
    intervals = c(0, 60),
    increments = c(1, 0.67)
  )

  increment2 <- IncrementsRelativeDLT(
    dlt_intervals = c(0, 1),
    increments = c(1, 0.50)
  )

  combIncrement <- IncrementsMin(increments_list = list(increment1, increment2))

  ncrm_loss_sc3 <- NextBestNCRMLoss(
    target = c(0.2, 0.35),
    overdose = c(0.35, 0.6),
    unacceptable = c(0.6, 1),
    max_overdose_prob = 0.25,
    losses = c(1, 0, 1, 2)
  )

  ncrm_sc3 <- NextBestNCRM(
    target = c(0.2, 0.35),
    overdose = c(0.35, 1),
    max_overdose_prob = 0.25
  )

  postSamples <- mcmc(data, model_bcrm_sc3, mcmc_options)

  dose_rec_loss <- expect_silent(nextBest(ncrm_loss_sc3,
    doselimit = maxDose(combIncrement, data),
    postSamples, model_bcrm_sc3, data
  ))

  dose_rec <- expect_silent(nextBest(ncrm_sc3,
    doselimit = maxDose(combIncrement, data),
    postSamples, model_bcrm_sc3, data
  ))

  rec_dose_loss_sc3 <- dose_rec_loss$value
  rec_dose_sc3 <- dose_rec$value

  prob_samples_mat <- matrix(
    nrow = size(postSamples@options),
    ncol = data@nGrid
  )

  # evaluate the probs, for all samples
  for (i in seq_len(data@nGrid)) {
    prob_samples_mat[, i] <- prob(
      dose = data@doseGrid[i],
      model_bcrm_sc3,
      postSamples
    )
  }
  pq75 <- apply(prob_samples_mat, 2, function(x) quantile(x, 0.75))

  res_sc3 <- cbind(
    LOSS = dose_rec_loss$probs[, "posterior_loss"],
    PTARGET = dose_rec_loss$probs[, "target"],
    POVEREX = dose_rec_loss$probs[, "excessive"],
    POVERUN = dose_rec_loss$probs[, "unacceptable"],
    POVER = rowSums(dose_rec_loss$probs[, c("excessive", "unacceptable")]),
    PMEAN = dose_rec_loss$probs[, "mean"],
    PQ75 = pq75
  )

  # Posterior summaries computed by SAS
  temp <- read.csv2(test_path("testdata/sc3_sit2.csv"),
    header = TRUE, dec = "."
  )
  sas_sc3 <- apply(as.matrix(temp[, -1]), 2, as.numeric)
  rownames(sas_sc3) <- temp[, 1]

  # compare posterior summaries for probabilities of DLT: crmPack vs. SAS
  all_true <- c(FALSE)
  all_true <- all(abs(res_sc3 - sas_sc3) < 0.01)

  expect_true(all_true)

  # Recommended dose computed by SAS
  sas_dose_rec <- 45
  # compare the recommended doses: crmPack vs. SAS
  expect_equal(rec_dose_sc3, sas_dose_rec, tolerance = 0)
})

test_that("Posterior summaries for probabilities of
          DLT (2-parameter logistic model) and recommended doses (NCRMLoss):
          crmPack vs. SAS - Example 3", {
  skip_on_cran()

  set.seed(0304191413)
  mcmc_options <- McmcOptions(
    burnin = 5000,
    step = 2,
    samples = 100000,
    rng_kind = "Wichmann-Hill",
    rng_seed = 1
  )

  dose_grid_sc3 <- c(10, 15, 30, 45, 60, 90, 120, 150, 180, 240)

  data <- Data(
    x = c(rep(15, 3), rep(30, 3), rep(60, 3), rep(90, 3)),
    y = c(rep(0, 3), rep(0, 3), rep(0, 2), 1, rep(0, 2), 1),
    cohort = c(rep(1, 3), rep(2, 3), rep(3, 3), rep(4, 3)),
    doseGrid = dose_grid_sc3,
    ID = 1:12
  )

  model_bcrm_sc3 <- LogisticLogNormal(
    mean = c(-0.8473, -0.0935),
    cov = matrix(c(1.489^2, 0, 0, 1.275^2), nrow = 2),
    ref_dose = 150
  )

  increment1 <- IncrementsRelative(
    intervals = c(0, 60),
    increments = c(1, 0.67)
  )

  increment2 <- IncrementsRelativeDLT(
    dlt_intervals = c(0, 1),
    increments = c(1, 0.50)
  )

  combIncrement <- IncrementsMin(increments_list = list(increment1, increment2))

  ncrm_loss_sc3 <- NextBestNCRMLoss(
    target = c(0.2, 0.35),
    overdose = c(0.35, 0.6),
    unacceptable = c(0.6, 1),
    max_overdose_prob = 0.25,
    losses = c(1, 0, 1, 2)
  )

  ncrm_sc3 <- NextBestNCRM(
    target = c(0.2, 0.35),
    overdose = c(0.35, 1),
    max_overdose_prob = 0.25
  )

  postSamples <- mcmc(data, model_bcrm_sc3, mcmc_options)

  dose_rec_loss <- expect_silent(nextBest(ncrm_loss_sc3,
    doselimit = maxDose(combIncrement, data),
    postSamples, model_bcrm_sc3, data
  ))

  dose_rec <- expect_silent(nextBest(ncrm_sc3,
    doselimit = maxDose(combIncrement, data),
    postSamples, model_bcrm_sc3, data
  ))

  rec_dose_loss_sc3 <- dose_rec_loss$value
  rec_dose_sc3 <- dose_rec$value

  prob_samples_mat <- matrix(
    nrow = size(postSamples@options),
    ncol = data@nGrid
  )

  # evaluate the probs, for all samples
  for (i in seq_len(data@nGrid)) {
    prob_samples_mat[, i] <- prob(
      dose = data@doseGrid[i],
      model_bcrm_sc3,
      postSamples
    )
  }
  pq75 <- apply(prob_samples_mat, 2, function(x) quantile(x, 0.75))

  res_sc3 <- cbind(
    LOSS = dose_rec_loss$probs[, "posterior_loss"],
    PTARGET = dose_rec_loss$probs[, "target"],
    POVEREX = dose_rec_loss$probs[, "excessive"],
    POVERUN = dose_rec_loss$probs[, "unacceptable"],
    POVER = rowSums(dose_rec_loss$probs[, c("excessive", "unacceptable")]),
    PMEAN = dose_rec_loss$probs[, "mean"],
    PQ75 = pq75
  )

  # Posterior summaries computed by SAS
  temp <- read.csv2(test_path("testdata/sc3_sit3.csv"),
    header = TRUE, dec = "."
  )
  sas_sc3 <- apply(as.matrix(temp[, -1]), 2, as.numeric)
  rownames(sas_sc3) <- temp[, 1]

  # compare posterior summaries for probabilities of DLT: crmPack vs. SAS
  all_true <- c(FALSE)
  all_true <- all(abs(res_sc3 - sas_sc3) < 0.01)

  expect_true(all_true)

  # Recommended dose computed by SAS
  sas_dose_rec <- 90
  # compare the recommended doses: crmPack vs. SAS
  expect_equal(rec_dose_sc3, sas_dose_rec, tolerance = 0)
})

test_that("Posterior summaries for probabilities of
          DLT (2-parameter logistic model) and recommended doses (NCRMLoss):
          crmPack vs. SAS - Example 4", {
  skip_on_cran()

  set.seed(0304191413)
  mcmc_options <- McmcOptions(
    burnin = 5000,
    step = 2,
    samples = 100000,
    rng_kind = "Wichmann-Hill",
    rng_seed = 1
  )

  dose_grid_sc3 <- c(10, 15, 30, 45, 60, 90, 120, 150, 180, 240)

  data <- Data(
    x = c(rep(15, 3), rep(30, 3), rep(60, 3)),
    y = c(rep(0, 3), rep(0, 3), rep(0, 3)),
    cohort = c(rep(1, 3), rep(2, 3), rep(3, 3)),
    doseGrid = dose_grid_sc3,
    ID = 1:9
  )

  model_bcrm_sc3 <- LogisticLogNormal(
    mean = c(-0.8473, -0.0935),
    cov = matrix(c(1.489^2, 0, 0, 1.275^2), nrow = 2),
    ref_dose = 150
  )

  increment1 <- IncrementsRelative(
    intervals = c(0, 60),
    increments = c(1, 0.67)
  )

  increment2 <- IncrementsRelativeDLT(
    dlt_intervals = c(0, 1),
    increments = c(1, 0.50)
  )

  combIncrement <- IncrementsMin(increments_list = list(increment1, increment2))

  ncrm_loss_sc3 <- NextBestNCRMLoss(
    target = c(0.2, 0.35),
    overdose = c(0.35, 0.6),
    unacceptable = c(0.6, 1),
    max_overdose_prob = 0.25,
    losses = c(1, 0, 1, 2)
  )

  ncrm_sc3 <- NextBestNCRM(
    target = c(0.2, 0.35),
    overdose = c(0.35, 1),
    max_overdose_prob = 0.25
  )

  postSamples <- mcmc(data, model_bcrm_sc3, mcmc_options)

  dose_rec_loss <- expect_silent(nextBest(ncrm_loss_sc3,
    doselimit = maxDose(combIncrement, data),
    postSamples, model_bcrm_sc3, data
  ))

  dose_rec <- expect_silent(nextBest(ncrm_sc3,
    doselimit = maxDose(combIncrement, data),
    postSamples, model_bcrm_sc3, data
  ))

  rec_dose_loss_sc3 <- dose_rec_loss$value
  rec_dose_sc3 <- dose_rec$value

  prob_samples_mat <- matrix(
    nrow = size(postSamples@options),
    ncol = data@nGrid
  )

  # evaluate the probs, for all samples
  for (i in seq_len(data@nGrid)) {
    prob_samples_mat[, i] <- prob(
      dose = data@doseGrid[i],
      model_bcrm_sc3,
      postSamples
    )
  }
  pq75 <- apply(prob_samples_mat, 2, function(x) quantile(x, 0.75))

  res_sc3 <- cbind(
    LOSS = dose_rec_loss$probs[, "posterior_loss"],
    PTARGET = dose_rec_loss$probs[, "target"],
    POVEREX = dose_rec_loss$probs[, "excessive"],
    POVERUN = dose_rec_loss$probs[, "unacceptable"],
    POVER = rowSums(dose_rec_loss$probs[, c("excessive", "unacceptable")]),
    PMEAN = dose_rec_loss$probs[, "mean"],
    PQ75 = pq75
  )

  # Posterior summaries computed by SAS
  temp <- read.csv2(test_path("testdata/sc3_sit4.csv"),
    header = TRUE, dec = "."
  )
  sas_sc3 <- apply(as.matrix(temp[, -1]), 2, as.numeric)
  rownames(sas_sc3) <- temp[, 1]

  # Compare posterior summaries for probabilities of DLT: crmPack vs. SAS
  all_true <- c(FALSE)
  all_true <- all(abs(res_sc3 - sas_sc3) < 0.01)

  expect_true(all_true)

  # Recommended dose computed by SAS
  sas_dose_rec <- 90
  # compare the recommended doses: crmPack vs. SAS
  expect_equal(rec_dose_sc3, sas_dose_rec, tolerance = 0)
})
