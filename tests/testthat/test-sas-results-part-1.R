test_that("Posterior summaries for probabilities of
          DLT (2-parameter logistic model) and recommended doses (NCRMLoss):
          crmPack vs. SAS - Example 1", {

  skip_on_cran()

  set.seed(0504201914)
  mcmc_options <- McmcOptions(
    burnin = 5000,
    step = 2,
    samples = 200000,
    rng_kind = "Wichmann-Hill",
    rng_seed = 1
  )

  dose_grid_sc1 <- c(10, 20, 35, 50, 65, 80, 90, 100)

  data <- Data(
    x = c(rep(10, 3)),
    y = c(rep(0, 3)),
    cohort = c(rep(1, 3)),
    doseGrid = dose_grid_sc1,
    ID = 1:3
  )

  model_bcrm_sc1 <- LogisticLogNormal(
    mean = c(-0.708, -0.389),
    cov = matrix(c(1.2^2, 0, 0, 0.9^2), nrow = 2),
    ref_dose = 90
  )

  ncrm_loss_sc1 <- NextBestNCRMLoss(
    target = c(0.2, 0.35),
    overdose = c(0.35, 0.6),
    unacceptable = c(0.6, 1),
    max_overdose_prob = 0.9999,
    losses = c(1, 0, 2, 3)
  )

  postSamples <- mcmc(data, model_bcrm_sc1, mcmc_options)

  dose_rec_loss <- expect_silent(nextBest(ncrm_loss_sc1,
    doselimit = Inf,
    postSamples, model_bcrm_sc1, data
  ))

  rec_dose_sc1 <- dose_rec_loss$value

  prob_samples_mat <- matrix(
    nrow = size(postSamples@options),
    ncol = data@nGrid
  )

  # evaluate the probs, for all samples
  for (i in seq_len(data@nGrid)) {
    prob_samples_mat[, i] <- prob(
      dose = data@doseGrid[i],
      model_bcrm_sc1,
      postSamples
    )
  }
  pq75 <- apply(prob_samples_mat, 2, function(x) quantile(x, 0.75))

  res_sc1 <- cbind(
    LOSS = dose_rec_loss$probs[, "posterior_loss"],
    PTARGET = dose_rec_loss$probs[, "target"],
    POVEREX = dose_rec_loss$probs[, "excessive"],
    POVERUN = dose_rec_loss$probs[, "unacceptable"],
    POVER = rowSums(dose_rec_loss$probs[, c("excessive", "unacceptable")]),
    PMEAN = dose_rec_loss$probs[, "mean"],
    PQ75 = pq75
  )

  # Posterior summaries computed by SAS
  temp <- read.csv2(test_path("testdata/sc1_sit1.csv"),
    header = TRUE, dec = "."
  )
  sas_sc1 <- apply(as.matrix(temp[, -1]), 2, as.numeric)
  rownames(sas_sc1) <- temp[, 1]

  # Compare posterior summaries for probabilities of DLT: crmPack vs. SAS
  all_true <- c(FALSE)
  all_true <- all(abs(res_sc1 - sas_sc1) < 0.01)

  expect_true(all_true)

  # Recommended dose computed by SAS
  sas_dose_rec <- 35
  # compare recommended doses: crmPack vs. SAS
  expect_equal(rec_dose_sc1, sas_dose_rec, tolerance = 0)
})

test_that("Posterior summaries for probabilities of
          DLT (2-parameter logistic model) and recommended doses (NCRMLoss):
          crmPack vs. SAS - Example 2", {
  set.seed(0504201914)
  mcmc_options <- McmcOptions(
    burnin = 5000,
    step = 2,
    samples = 600000,
    rng_kind = "Wichmann-Hill",
    rng_seed = 1
  )

  dose_grid_sc1 <- c(10, 20, 35, 50, 65, 80, 90, 100)

  data <- Data(
    x = c(rep(10, 3), rep(20, 3)),
    y = c(rep(0, 3), rep(0, 2), 1),
    cohort = c(rep(1, 3), rep(2, 3)),
    doseGrid = dose_grid_sc1,
    ID = 1:6
  )

  model_bcrm_sc1 <- LogisticLogNormal(
    mean = c(-0.708, -0.389),
    cov = matrix(c(1.2^2, 0, 0, 0.9^2), nrow = 2),
    ref_dose = 90
  )

  ncrm_loss_sc1 <- NextBestNCRMLoss(
    target = c(0.2, 0.35),
    overdose = c(0.35, 0.6),
    unacceptable = c(0.6, 1),
    max_overdose_prob = 0.9999,
    losses = c(1, 0, 2, 3)
  )

  postSamples <- mcmc(data, model_bcrm_sc1, mcmc_options)

  dose_rec_loss <- expect_silent(nextBest(ncrm_loss_sc1,
    doselimit = Inf,
    postSamples, model_bcrm_sc1, data
  ))

  rec_dose_sc1 <- dose_rec_loss$value

  prob_samples_mat <- matrix(
    nrow = size(postSamples@options),
    ncol = data@nGrid
  )

  # evaluate the probs, for all samples
  for (i in seq_len(data@nGrid)) {
    prob_samples_mat[, i] <- prob(
      dose = data@doseGrid[i],
      model_bcrm_sc1,
      postSamples
    )
  }
  pq75 <- apply(prob_samples_mat, 2, function(x) quantile(x, 0.75))

  res_sc1 <- cbind(
    LOSS = dose_rec_loss$probs[, "posterior_loss"],
    PTARGET = dose_rec_loss$probs[, "target"],
    POVEREX = dose_rec_loss$probs[, "excessive"],
    POVERUN = dose_rec_loss$probs[, "unacceptable"],
    POVER = rowSums(dose_rec_loss$probs[, c("excessive", "unacceptable")]),
    PMEAN = dose_rec_loss$probs[, "mean"],
    PQ75 = pq75
  )

  # Posterior summaries computed by SAS
  temp <- read.csv2(test_path("testdata/sc1_sit2.csv"),
    header = TRUE, dec = "."
  )
  sas_sc1 <- apply(as.matrix(temp[, -1]), 2, as.numeric)
  rownames(sas_sc1) <- temp[, 1]

  # compare posterior summaries for probabilities of DLT: crmPack vs. SAS
  all_true <- c(FALSE)
  all_true <- all(abs(res_sc1 - sas_sc1) < 0.01)

  expect_true(all_true)

  # Recommended dose computed by SAS
  sas_dose_rec <- 20
  # compare recommended doses: crmPack vs. SAS
  expect_equal(rec_dose_sc1, sas_dose_rec, tolerance = 0)
})

test_that("Posterior summaries for probabilities of
          DLT (2-parameter logistic model) and recommended doses (NCRMLoss):
          crmPack vs. SAS - Example 3", {
  set.seed(0504201914)
  mcmc_options <- McmcOptions(
    burnin = 5000,
    step = 2,
    samples = 200000,
    rng_kind = "Wichmann-Hill",
    rng_seed = 1
  )

  dose_grid_sc1 <- c(10, 20, 35, 50, 65, 80, 90, 100)

  data <- Data(
    x = c(
      rep(10, 3), rep(20, 3),
      rep(35, 3), rep(50, 3)
    ),
    y = c(rep(0, 3 * 4)),
    cohort = c(
      rep(1, 3), rep(2, 3),
      rep(3, 3), rep(4, 3)
    ),
    doseGrid = dose_grid_sc1,
    ID = 1:12
  )

  model_bcrm_sc1 <- LogisticLogNormal(
    mean = c(-0.708, -0.389),
    cov = matrix(c(1.2^2, 0, 0, 0.9^2), nrow = 2),
    ref_dose = 90
  )

  ncrm_loss_sc1 <- NextBestNCRMLoss(
    target = c(0.2, 0.35),
    overdose = c(0.35, 0.6),
    unacceptable = c(0.6, 1),
    max_overdose_prob = 0.9999,
    losses = c(1, 0, 2, 3)
  )

  postSamples <- mcmc(data, model_bcrm_sc1, mcmc_options)

  dose_rec_loss <- expect_silent(nextBest(ncrm_loss_sc1,
    doselimit = Inf,
    postSamples, model_bcrm_sc1, data
  ))

  rec_dose_sc1 <- dose_rec_loss$value

  prob_samples_mat <- matrix(
    nrow = size(postSamples@options),
    ncol = data@nGrid
  )

  # evaluate the probs, for all samples
  for (i in seq_len(data@nGrid)) {
    prob_samples_mat[, i] <- prob(
      dose = data@doseGrid[i],
      model_bcrm_sc1,
      postSamples
    )
  }
  pq75 <- apply(prob_samples_mat, 2, function(x) quantile(x, 0.75))

  res_sc1 <- cbind(
    LOSS = dose_rec_loss$probs[, "posterior_loss"],
    PTARGET = dose_rec_loss$probs[, "target"],
    POVEREX = dose_rec_loss$probs[, "excessive"],
    POVERUN = dose_rec_loss$probs[, "unacceptable"],
    POVER = rowSums(dose_rec_loss$probs[, c("excessive", "unacceptable")]),
    PMEAN = dose_rec_loss$probs[, "mean"],
    PQ75 = pq75
  )

  # Posterior summaries computed by SAS
  temp <- read.csv2(test_path("testdata/sc1_sit3.csv"),
    header = TRUE, dec = "."
  )
  sas_sc1 <- apply(as.matrix(temp[, -1]), 2, as.numeric)
  rownames(sas_sc1) <- temp[, 1]

  # compare posterior summaries for probabilities of DLT: crmPack vs. SAS
  all_true <- c(FALSE)
  all_true <- all(abs(res_sc1 - sas_sc1) < 0.01)

  expect_true(all_true)

  # Recommended dose computed by SAS
  sas_dose_rec <- 65
  # compare recommended doses: crmPack vs. SAS
  expect_equal(rec_dose_sc1, sas_dose_rec, tolerance = 0)
})

test_that("Posterior summaries for probabilities of
          DLT (2-parameter logistic model) and recommended doses (NCRMLoss):
          crmPack vs. SAS - Example 4", {
  set.seed(0504201914)
  mcmc_options <- McmcOptions(
    burnin = 5000,
    step = 2,
    samples = 200000,
    rng_kind = "Wichmann-Hill",
    rng_seed = 1
  )

  dose_grid_sc1 <- c(10, 20, 35, 50, 65, 80, 90, 100)

  data <- Data(
    x = c(
      rep(10, 3), rep(20, 3),
      rep(35, 3), rep(50, 3)
    ),
    y = c(rep(0, 3 * 3), rep(0, 2), 1),
    cohort = c(
      rep(1, 3), rep(2, 3),
      rep(3, 3), rep(4, 3)
    ),
    doseGrid = dose_grid_sc1,
    ID = 1:12
  )

  model_bcrm_sc1 <- LogisticLogNormal(
    mean = c(-0.708, -0.389),
    cov = matrix(c(1.2^2, 0, 0, 0.9^2), nrow = 2),
    ref_dose = 90
  )

  ncrm_loss_sc1 <- NextBestNCRMLoss(
    target = c(0.2, 0.35),
    overdose = c(0.35, 0.6),
    unacceptable = c(0.6, 1),
    max_overdose_prob = 0.9999,
    losses = c(1, 0, 2, 3)
  )

  postSamples <- mcmc(data, model_bcrm_sc1, mcmc_options)

  dose_rec_loss <- expect_silent(nextBest(ncrm_loss_sc1,
    doselimit = Inf,
    postSamples, model_bcrm_sc1, data
  ))

  rec_dose_sc1 <- dose_rec_loss$value

  prob_samples_mat <- matrix(
    nrow = size(postSamples@options),
    ncol = data@nGrid
  )

  # evaluate the probs, for all samples
  for (i in seq_len(data@nGrid)) {
    prob_samples_mat[, i] <- prob(
      dose = data@doseGrid[i],
      model_bcrm_sc1,
      postSamples
    )
  }
  pq75 <- apply(prob_samples_mat, 2, function(x) quantile(x, 0.75))

  res_sc1 <- cbind(
    LOSS = dose_rec_loss$probs[, "posterior_loss"],
    PTARGET = dose_rec_loss$probs[, "target"],
    POVEREX = dose_rec_loss$probs[, "excessive"],
    POVERUN = dose_rec_loss$probs[, "unacceptable"],
    POVER = rowSums(dose_rec_loss$probs[, c("excessive", "unacceptable")]),
    PMEAN = dose_rec_loss$probs[, "mean"],
    PQ75 = pq75
  )

  # Posterior summaries computed by SAS
  temp <- read.csv2(test_path("testdata/sc1_sit4.csv"),
    header = TRUE, dec = "."
  )
  sas_sc1 <- apply(as.matrix(temp[, -1]), 2, as.numeric)
  rownames(sas_sc1) <- temp[, 1]

  # Compare posterior summaries for probabilities of DLT: crmPack vs. SAS
  all_true <- c(FALSE)
  all_true <- all(abs(res_sc1 - sas_sc1) < 0.01)

  expect_true(all_true)

  # Recommended dose computed by SAS
  sas_dose_rec <- 50
  # compare recommended doses: crmPack vs. SAS
  expect_equal(rec_dose_sc1, sas_dose_rec, tolerance = 0)
})
