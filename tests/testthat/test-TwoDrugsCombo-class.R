# TwoDrugsCombo ----

## internal helpers ----

test_that("TwoDrugsCombo expression helpers replace and discover symbols", {
  expr <- quote(logit(p[i]) <- alpha0 + alpha1 * log(x[i] / ref_dose))
  replacements <- list(
    p = as.name("p_drug1"),
    alpha0 = as.name("alpha0_drug1"),
    alpha1 = as.name("alpha1_drug1"),
    x = as.name("x_drug1"),
    ref_dose = as.name("ref_dose_drug1")
  )

  replaced <- h_two_drugs_combo_replace_symbols(expr, replacements)
  expect_equal(
    deparse1(replaced),
    "logit(p_drug1[i]) <- alpha0_drug1 + alpha1_drug1 * log(x_drug1[i]/ref_dose_drug1)" # nolint
  )
  expect_equal(
    h_two_drugs_combo_indexed_call("p_single", as.name("i"), 2),
    quote(p_single[i, 2])
  )
  expect_equal(h_two_drugs_combo_lhs_symbols(quote(logit(p[i]))), "p")
  expect_setequal(
    h_two_drugs_combo_assigned_nodes(body(h_get_two_drugs_combo()@datamodel)),
    c(
      "x_drug1",
      "p_drug1",
      "p_single",
      "x_drug2",
      "p_drug2",
      "combo_interaction",
      "p0",
      "p",
      "y"
    )
  )
})

test_that("TwoDrugsCombo specification helpers handle optional ref_dose", {
  log_model <- h_get_logistic_log_normal()
  raw_model <- h_get_general_single_agent_no_ref()
  specs <- h_two_drugs_combo_single_model_specs(log_model, from_prior = FALSE)

  expect_named(specs, c("mean", "prec", "ref_dose"))
  expect_equal(h_two_drugs_combo_single_model_ref_dose(log_model), 50)
  expect_true(is.na(h_two_drugs_combo_single_model_ref_dose(raw_model)))
  expect_named(
    h_two_drugs_combo_suffix_names(list(mean = 1, prec = 2), "_drug1"),
    c("mean_drug1", "prec_drug1")
  )
})

test_that("TwoDrugsCombo dose-normalization helpers infer dose covariates", {
  expect_true(h_two_drugs_combo_contains_symbol(quote(log(x / ref_dose)), "x"))
  expect_false(h_two_drugs_combo_contains_symbol(quote(alpha0 + alpha1), "x"))
  expect_true(h_two_drugs_combo_is_x_term(quote(x[i])))
  expect_false(h_two_drugs_combo_is_x_term(quote(x[i] / ref_dose)))

  expect_equal(
    h_two_drugs_combo_normalized_dose_from_expr(quote(log(x[i] / ref_dose))),
    quote(x[i] / ref_dose)
  )
  expect_equal(
    h_two_drugs_combo_normalized_dose_from_expr(quote(alpha0 + alpha1 * x[i])),
    quote(x[i])
  )
  expect_equal(
    h_two_drugs_combo_rhs_expressions(quote({
      a <- b + x
      y[i] ~ dbern(p[i])
    })),
    list(quote(b + x), quote(dbern(p[i])))
  )
  expect_equal(
    h_two_drugs_combo_normalized_dose_expr(
      quote({
        logit(p[i]) <- alpha0 + alpha1 * log(x[i] / ref_dose)
        y[i] ~ dbern(p[i])
      }),
      list(x = as.name("x_drug1"), ref_dose = as.name("ref_dose_drug1"))
    ),
    quote(x_drug1[i] / ref_dose_drug1)
  )
})

test_that("TwoDrugsCombo likelihood helpers rewrite Bernoulli contribution", {
  replacements <- list(
    p = as.name("p_drug2"),
    x = as.name("x_drug2"),
    alpha0 = as.name("alpha0_drug2")
  )
  replacement <- h_two_drugs_combo_likelihood_replacement(
    quote(y[k] ~ dbern(p[k])),
    replacements = replacements,
    index = 2
  )
  transformed <- h_two_drugs_combo_replace_bernoulli_likelihood(
    quote({
      logit(p[k]) <- alpha0 + x[k]
      y[k] ~ dbern(p[k])
    }),
    replacements = replacements,
    index = 2
  )

  expect_equal(replacement, quote(p_single[k, 2] <- p_drug2[k]))
  expect_true(transformed$found)
  expect_match(deparse1(transformed$expr), "p_single\\[k, 2\\] <- p_drug2\\[k\\]")
  expect_match(deparse1(transformed$expr), "logit\\(p_drug2\\[k\\]\\)")
})

test_that("TwoDrugsCombo model-fragment helpers generate expected JAGS", {
  model <- h_get_two_drugs_combo_diff_pars()
  x_mapping <- h_two_drugs_combo_x_mapping_model(2)
  sample_alias <- h_two_drugs_combo_sample_alias_model(model@sample, model@single_models)
  interaction <- h_two_drugs_combo_interaction_model(
    list(quote(x_drug1[i]), quote(x_drug2[i] / ref_dose_drug2))
  )

  expect_match(deparse1(body(x_mapping)), "x_drug2\\[i\\] <- x\\[i, 2\\]")
  expect_match(deparse1(body(sample_alias)), "beta0\\[1L\\] <- beta0_drug1")
  expect_match(deparse1(body(sample_alias)), "alpha0\\[1L\\] <- alpha0_drug2")
  expect_match(
    deparse1(body(interaction)),
    "combo_interaction\\[i\\] <- x_drug1\\[i\\] \\* \\(x_drug2\\[i\\]/ref_dose_drug2\\)" # nolint
  )
})

test_that("TwoDrugsCombo single-model part helper namespaces a model", {
  part <- h_two_drugs_combo_single_model_part(h_get_logistic_log_normal(), 2)
  prior_file <- h_jags_write_model(part$priormodel)
  data_file <- h_jags_write_model(part$datamodel)
  on.exit(unlink(c(prior_file, data_file)))
  prior_text <- gsub("\\s+", " ", paste(readLines(prior_file), collapse = " "))
  data_text <- gsub("\\s+", " ", paste(readLines(data_file), collapse = " "))

  expect_named(part$prior_specs, c("mean_drug2", "prec_drug2"))
  expect_named(part$full_specs, c("mean_drug2", "prec_drug2", "ref_dose_drug2"))
  expect_named(part$inits, "theta_drug2")
  expect_equal(part$normalized_dose, quote(x_drug2[i] / ref_dose_drug2))
  expect_match(prior_text, "theta_drug2 ~ dmnorm\\(mean_drug2, prec_drug2\\)")
  expect_match(data_text, "x_drug2\\[i\\] <- x\\[i, 2\\]")
  expect_match(data_text, "p_single\\[i, 2\\] <- p_drug2\\[i\\]")
})

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

  expect_equal(model@sample, c("beta0", "beta1", "alpha0", "alpha1", "eta"))

  result <- mcmc(data = data, model = model, options = options)
  expect_s4_class(result, "Samples")
  expect_subset(
    c("alpha0", "alpha1", "beta0", "beta1", "eta"),
    names(result@data)
  )
  expect_length(result@data$alpha0, 10L)
  expect_length(result@data$alpha1, 10L)
  expect_length(result@data$beta0, 10L)
  expect_length(result@data$beta1, 10L)
})
