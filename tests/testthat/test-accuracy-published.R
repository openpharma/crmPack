

test_that(".PowerNormal works as expected", {
  .PowerNormal <- setClass(
    Class = "PowerNormal", contains = "Model",
    slots = c(
      skel_fun = "function",
      skel_probs = "numeric",
      mean = "numeric",
      variance = "numeric"
    )
  )

  # create a convenient initialization function
  PowerNormal <- function(skel_probs, dose_grid, mean, variance) {
    skel_fun <- approxfun(x = dose_grid, y = skel_probs, rule = 2)
    inv_skel_fun <- approxfun(x = skel_probs, y = dose_grid, rule = 1)

    .PowerNormal(
      skel_fun = skel_fun, skel_probs = skel_probs,
      mean = mean,
      variance = variance,
      datamodel = function() {
        for (i in 1:nObs) {
          y[i] ~ dbern(p[i])
          p[i] <- skel_probs[xLevel[i]]^exp(param1)
        }
      },
      datanames = c("nObs", "y", "xLevel"),
      prob = function(dose, param1) {
        skel_fun(dose)^exp(param1)
      },
      dose = function(x, param1) {
        inv_skel_fun(x^ (1 / exp(param1)))
      },
      priormodel = function() {
        param1 ~ dnorm(mean, 1 / variance)
      },
      modelspecs = function() {
        list(
          skel_probs = skel_probs,
          mean = mean, variance = variance
        )
      },
      init = function() {
        list(param1 = 1)
      }, sample = "param1"
    )
  }

  mcmc_options <- crmPack::McmcOptions(
    burnin = 50000, step = 2,
    samples = 1000000
  )


  ## One-paramter model
  ## (A) Posterior summaries (original skeleton)
  empty_data <- crmPack::Data(dose_grid = c(
    1, 2.5, 5, 10, 15, 20, 25, 30,
    40, 50, 75, 100, 150, 200, 250
  ))

  data_obs <- crmPack::Data(
    x = c(
      rep(1, 3), rep(2.5, 4), rep(5, 5),
      rep(10, 4), rep(25, 2)
    ),
    y = c(
      rep(0, 3), rep(0, 4), rep(0, 5),
      rep(0, 4), rep(1, 2)
    ),
    cohort = c(
      rep(1, 3), rep(2, 4), rep(3, 5),
      rep(4, 4), rep(7, 2)
    ),
    doseGrid = c(
      1, 2.5, 5, 10, 15, 20, 25, 30,
      40, 50, 75, 100, 150, 200, 250
    ),
    ID = 1:18
  )

  model_power_a <- PowerNormal(
    skel_probs = c(
      0.01, 0.015, 0.020, 0.025, 0.03,
      0.04, 0.05, 0.10, 0.17, 0.30,
      0.45, 0.70, 0.80, 0.90, 0.95
    ),
    dose_grid = c(
      1, 2.5, 5, 10, 15, 20, 25, 30,
      40, 50, 75, 100, 150, 200, 250
    ),
    mean = 0,
    variance = 1.34^2
  )

  expect_warning(prior_samples <- crmPack::mcmc(
    data = empty_data,
    model = model_power_a,
    options = mcmc_options
  ))

  ## NCRM rule with loss function
  ncrm_loss <- crmPack::NextBestNCRMLoss(
    target = c(0.2, 0.35),
    overdose = c(0.35, 0.6),
    unacceptable = c(0.6, 1),
    max_overdose_prob = 0.25,
    losses = c(1, 0, 1, 2)
  )

  increments_no <- crmPack::IncrementsRelative(
    intervals = 250,
    increments = 2
  )

  post_samples_a <- crmPack::mcmc(data_obs, model_power_a, mcmc_options)

  dose_rec_loss_a <- crmPack::nextBest(ncrm_loss,
    doselimit = crmPack::maxDose(
      increments_no,
      data_obs
    ),
    samples = post_samples_a,
    model = model_power_a,
    data = data_obs
  )

  ## (A) Actual table I
  pat_info <- rbind(
    "No. of patients" = c(3, 4, 5, 4, "-", "-", 2, "-", "-", "-"),
    "No. of DLTs" = c(0, 0, 0, 0, "-", "-", 2, "-", "-", "-")
  )
  colnames(pat_info) <- c(1, 2.5, 5, 10, 15, 20, 25, 30, 40, 50)

  tab1_a_act <- rbind(
    "Skeleton (CRM)" = c(
      0.01, 0.015, 0.020, 0.025, 0.03,
      0.04, 0.05, 0.10, 0.17, 0.30,
      0.45, 0.70, 0.80, 0.90, 0.95
    )[1:10],
    t(dose_rec_loss_a$probs[, c(6:7)])[, 1:10]
  )


  ## (B) Actual table I
  model_power_b <- PowerNormal(
    skel_probs = c(
      0.063, 0.125, 0.188, 0.250, 0.313,
      0.375, 0.438, 0.500, 0.563, 0.625
    ),
    dose_grid = c(
      1, 2.5, 5, 10, 15, 20, 25, 30,
      40, 50
    ),
    mean = 0,
    variance = 1.34^2
  )

  post_samples_b <- crmPack::mcmc(data_obs, model_power_b, mcmc_options)

  dose_rec_loss_b <- crmPack::nextBest(ncrm_loss,
    doselimit = crmPack::maxDose(
      increments_no,
      data_obs
    ),
    post_samples_b, model_power_b, data_obs
  )

  tab1_b_act <- rbind(
    "Skeleton (CRM)" = c(
      0.01, 0.015, 0.020, 0.025, 0.03,
      0.04, 0.05, 0.10, 0.17, 0.30,
      0.45, 0.70, 0.80, 0.90, 0.95
    )[1:10],
    # t(doseRec_loss_A[[4]])[c(7:8),1:10]),3)
    t(dose_rec_loss_b$probs[, c(6:7)])[, 1:10]
  )

  ## (A)+ (B) Actual table I
  tab1_act <- list(
    "Posterior summaries (original skeleton)" = tab1_a_act,
    "Posterior summaries (equidistant skeleton)" = tab1_b_act
  )


  ## Expected table I (Neuenschwander et al.)
  tab_1_exp <- list()
  tab1_names <- list(
    c("Skeleton (CRM)", "Mean", "Std. dev."),
    c(1, 2.5, 5, 10, 15, 20, 25, 30, 40, 50)
  )

  tab1 <- read.table("NeuenschwanderTable1.txt",
    sep = "\t",
    header = TRUE
  )
  tab_1_exp$"Posterior summaries (original skeleton)" <- apply(as.matrix(tab1[4:6, -1]), 2, as.numeric)
  tab_1_exp$"Posterior summaries (equidistant skeleton)" <- apply(as.matrix(tab1[8:10, -1]), 2, as.numeric)
  colnames(tab_1_exp[[1]]) <- colnames(tab1_exp[[2]]) <- tab1_names[[2]]
  rownames(tab1_exp[[1]]) <- rownames(tab1_exp[[2]]) <- tab1_names[[1]]

  ## test Posterior summaries for probabilities of DLT (CRM)
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
