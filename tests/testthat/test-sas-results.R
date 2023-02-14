test_that("Sc1: Posterior summaries for probabilities of
          DLT (2-parameter logistic model): crmPack vs. SAS", {
  ## Data examples
  ## Scenario 1
  set.seed(0504201914)
  mcmc_options <- McmcOptions(
    burnin = 5000,
    step = 2,
    samples = 1000000,
    rng_kind = "Wichmann-Hill",
    rng_seed = 1
  )

  doseGrid.sc1 <- c(10, 20, 35, 50, 65, 80, 90, 100)
  tab.sc1 <- vector("list")
  tab.sc1$"Data example 1 (DLTs: 0)" <- Data(
    x = c(rep(10, 3)),
    y = c(rep(0, 3)),
    cohort = c(rep(1, 3)),
    doseGrid = doseGrid.sc1,
    ID = 1:3
  )

  tab.sc1$"Data example 2 (DLTs: 0,1)" <- Data(
    x = c(rep(10, 3), rep(20, 3)),
    y = c(rep(0, 3), rep(0, 2), 1),
    cohort = c(rep(1, 3), rep(2, 3)),
    doseGrid = doseGrid.sc1,
    ID = 1:6
  )

  tab.sc1$"Data example 3 (DLTs: 0,0,0,0)" <- Data(
    x = c(
      rep(10, 3), rep(20, 3),
      rep(35, 3), rep(50, 3)
    ),
    y = c(rep(0, 3 * 4)),
    cohort = c(
      rep(1, 3), rep(2, 3),
      rep(3, 3), rep(4, 3)
    ),
    doseGrid = doseGrid.sc1,
    ID = 1:12
  )

  tab.sc1$"Data example 4 (DLTs: 0,0,0,1)" <- Data(
    x = c(
      rep(10, 3), rep(20, 3),
      rep(35, 3), rep(50, 3)
    ),
    y = c(rep(0, 3 * 3), rep(0, 2), 1),
    cohort = c(
      rep(1, 3), rep(2, 3),
      rep(3, 3), rep(4, 3)
    ),
    doseGrid = doseGrid.sc1,
    ID = 1:12
  )

  ## Look at single trials
  for (i in 1:length(tab.sc1)) {
    date_time <- Sys.time()
    while ((as.numeric(Sys.time()) - as.numeric(date_time)) < 1) {}
  }

  model_bcrm_sc1 <- LogisticLogNormal(
    mean = c(-0.708, -0.389),
    cov = matrix(c(1.2^2, 0, 0, 0.9^2), nrow = 2),
    ref_dose = 90
  )

  ncrm_loss_sc1 <- NextBestNCRMLoss(
    target = c(0.2, 0.35),
    overdose = c(0.35, 0.6),
    unacceptable = c(0.6, 1),
    max_overdose_prob = 0.9999, # changed from 1 to 0.99
    losses = c(1, 0, 2, 3)
  )

  rec.dose.sc1 <- NA
  res.sc1 <- vector("list", 4)
  names(res.sc1) <- names(tab.sc1)

  for (j in 1:length(tab.sc1)) {
    data.obs <- tab.sc1[[j]]
    postSamples <- mcmc(data.obs, model_bcrm_sc1, mcmc_options)

    doseRec_loss <- expect_silent(nextBest(ncrm_loss_sc1,
      doselimit = Inf,
      postSamples, model_bcrm_sc1, data.obs
    ))

    rec.dose.sc1[j] <- doseRec_loss$value

    probSamples.mat <- matrix(
      nrow = size(postSamples@options),
      ncol = data.obs@nGrid
    )

    ## evaluate the probs, for all samples.
    for (i in seq_len(data.obs@nGrid))
    {
      probSamples.mat[, i] <- prob(
        dose = data.obs@doseGrid[i],
        model_bcrm_sc1,
        postSamples
      )
    }
    pq75 <- apply(probSamples.mat, 2, function(x) quantile(x, 0.75))

    res.sc1[[j]] <- cbind(
      LOSS = doseRec_loss$probs[, "posterior_loss"],
      PTARGET = doseRec_loss$probs[, "target"],
      POVEREX = doseRec_loss$probs[, "excessive"],
      POVERUN = doseRec_loss$probs[, "unacceptable"],
      POVER = rowSums(doseRec_loss$probs[, c("excessive", "unacceptable")]),
      PMEAN = doseRec_loss$probs[, "mean"],
      PQ75 = pq75
    )
  }

  ## Posterior summaries computed by SAS
  SAS.sc1 <- vector("list", 4)
  names(SAS.sc1) <- names(tab.sc1)

  for (i in 1:4) {
    temp <- read.csv2(paste0(getwd(), "/testdata/sc1_sit", i, ".csv"), header = TRUE, dec = ".")
    SAS.sc1[[i]] <- apply(as.matrix(temp[, -1]), 2, as.numeric)
    rownames(SAS.sc1[[i]]) <- temp[, 1]
  }

  ## compare posterior summaries for probabilities of DLT (2-parameter logistic model): crmPack vs. SAS
  all_true <- c(F, F, F, F)
  for (i in 1:4) {
    all_true[i] <- all(abs(res.sc1[[i]] - SAS.sc1[[i]]) < 0.01)
  }

  expect_true(all(all_true))

  ## Recommended doses computed by SAS
  SAS.dose.Rec <- t(read.csv2(paste0(getwd(), "/testdata/sc1_recdose.csv")))[-1, ]
  ## compare recommended doses: crmPack vs. SAS
  expect_equal(rec.dose.sc1, SAS.dose.Rec, tolerance = 0)
})
