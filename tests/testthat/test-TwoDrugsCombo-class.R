# TwoDrugsCombo ----

## constructor ----

test_that("TwoDrugsCombo object can be created with user constructor", {
  result <- expect_silent(h_get_two_drugs_combo())
  expect_valid(result, "TwoDrugsCombo")
})

test_that("TwoDrugsCombo accepts LogisticNormalMixture agents", {
  result_one <- expect_silent(
    h_get_two_drugs_combo_with_normal_mix(two_mixtures = FALSE)
  )
  result_two <- expect_silent(
    h_get_two_drugs_combo_with_normal_mix(two_mixtures = TRUE)
  )

  expect_valid(result_one, "TwoDrugsCombo")
  expect_valid(result_two, "TwoDrugsCombo")
  expect_subset(
    c(
      "weightpar_drug1",
      "mean_drug1",
      "prec_drug1",
      "mean_drug2",
      "prec_drug2"
    ),
    names(result_one@modelspecs(TRUE))
  )
  expect_subset(
    c(
      "weightpar_drug1",
      "weightpar_drug2",
      "mean_drug1",
      "prec_drug1",
      "mean_drug2",
      "prec_drug2"
    ),
    names(result_two@modelspecs(TRUE))
  )
})

test_that("TwoDrugsCombo does not require alpha or ref_dose", {
  result <- expect_silent(h_get_two_drugs_combo_no_alpha_no_ref())

  expect_valid(result, "TwoDrugsCombo")
  expect_equal(result@sample, c("beta0", "beta1", "eta"))
  expect_true(all(is.na(result@ref_dose)))
  expect_subset(
    c("beta_mean_drug1", "beta_mean_drug2", "gamma", "tau"),
    names(result@modelspecs(TRUE))
  )
})

test_that("TwoDrugsCombo uses single-agent dose normalization", {
  log_model <- h_get_two_drugs_combo()
  raw_model <- h_get_two_drugs_combo_no_alpha_no_ref()
  sub_model <- h_get_two_drugs_combo_sub()

  log_model_file <- h_jags_write_model(log_model@datamodel)
  raw_model_file <- h_jags_write_model(raw_model@datamodel)
  sub_model_file <- h_jags_write_model(sub_model@datamodel)
  on.exit(unlink(c(log_model_file, raw_model_file, sub_model_file)))

  read_model <- function(file) {
    gsub("\\s+", " ", paste(readLines(file), collapse = " "))
  }
  log_model_text <- read_model(log_model_file)
  raw_model_text <- read_model(raw_model_file)
  sub_model_text <- read_model(sub_model_file)

  expect_match(
    log_model_text,
    "combo_interaction\\[i\\] <- x_drug1\\[i\\]/ref_dose_drug1 \\* \\(x_drug2\\[i\\]/ref_dose_drug2\\)" # nolint
  )
  expect_match(
    raw_model_text,
    "combo_interaction\\[i\\] <- x_drug1\\[i\\] \\* x_drug2\\[i\\]"
  )
  expect_match(
    sub_model_text,
    "combo_interaction\\[i\\] <- \\(x_drug1\\[i\\] - ref_dose_drug1\\) \\* \\(x_drug2\\[i\\] - ref_dose_drug2\\)" # nolint
  )
})

test_that(".DefaultTwoDrugsCombo works as expected", {
  expect_valid(
    .DefaultTwoDrugsCombo(),
    "TwoDrugsCombo"
  )
})

## mcmc ----

test_that("MCMC runs for TwoDrugsCombo model", {
  data <- h_get_data_combo()
  model <- h_get_two_drugs_combo()
  options <- h_get_mcmc_options(samples = 10, burnin = 20)

  result <- mcmc(data = data, model = model, options = options)
  expect_s4_class(result, "Samples")
  expect_subset(c("alpha0", "alpha1", "eta"), names(result@data))
  expect_equal(ncol(result@data$alpha0), 2L)
  expect_equal(ncol(result@data$alpha1), 2L)
})

test_that("MCMC runs for TwoDrugsCombo with mixture agents", {
  data <- h_get_data_combo()
  options <- h_get_mcmc_options(samples = 10, burnin = 20)

  for (two_mixtures in c(FALSE, TRUE)) {
    model <- h_get_two_drugs_combo_with_normal_mix(
      two_mixtures = two_mixtures
    )
    result <- mcmc(data = data, model = model, options = options)
    expect_s4_class(result, "Samples")
    expect_subset(c("alpha0", "alpha1", "eta"), names(result@data))
    expect_equal(ncol(result@data$alpha0), 2L)
    expect_equal(ncol(result@data$alpha1), 2L)
  }
})

test_that("MCMC runs for TwoDrugsCombo without alpha or ref_dose", {
  data <- h_get_data_combo()
  model <- h_get_two_drugs_combo_no_alpha_no_ref()
  options <- h_get_mcmc_options(samples = 10, burnin = 20)

  result <- mcmc(data = data, model = model, options = options)
  expect_s4_class(result, "Samples")
  expect_subset(c("beta0", "beta1", "eta"), names(result@data))
  expect_equal(ncol(result@data$beta0), 2L)
  expect_equal(ncol(result@data$beta1), 2L)
})

test_that("MCMC runs for TwoDrugsCombo model with empty data (i.e. prior)", {
  data <- h_get_data_combo(empty = TRUE)
  model <- h_get_two_drugs_combo(log_normal_eta = TRUE)
  options <- h_get_mcmc_options(samples = 10, burnin = 20)

  result <- mcmc(data = data, model = model, options = options)
  expect_s4_class(result, "Samples")
  expect_subset(c("alpha0", "alpha1", "eta"), names(result@data))
  expect_equal(ncol(result@data$alpha0), 2L)
  expect_equal(ncol(result@data$alpha1), 2L)
})

test_that("MCMC runs also when LogisticLogNormalSub models are used inside", {
  data <- h_get_data_combo()
  model <- h_get_two_drugs_combo_sub()
  options <- h_get_mcmc_options(samples = 10, burnin = 20)

  result <- mcmc(data = data, model = model, options = options)
  expect_s4_class(result, "Samples")
  expect_subset(c("alpha0", "alpha1", "eta"), names(result@data))
  expect_equal(ncol(result@data$alpha0), 2L)
  expect_equal(ncol(result@data$alpha1), 2L)
})

test_that("MCMC also works when a models with different parameters are combined", {
  data <- h_get_data_combo()
  model <- h_get_two_drugs_combo_diff_pars()
  options <- h_get_mcmc_options(samples = 10, burnin = 20)

  result <- mcmc(data = data, model = model, options = options)
  expect_s4_class(result, "Samples")
  expect_subset(
    c("alpha0", "alpha1", "beta0", "beta1", "eta"),
    names(result@data)
  )
})
