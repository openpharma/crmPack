# Prototype: mono + fixed combo + hierarchical MCMC wiring
#
# This script is intentionally outside the package API and is meant as a
# design prototype to exercise the data/model/sample flow before full
# implementation.

# nolint start

## First try: Hardcoded mcmc code and function

build_mono_prototype <- function() {
  mono_model <- crmPack::LogisticLogNormal(
    mean = c(-0.85, 1.0),
    cov = matrix(c(1.0, -0.5, -0.5, 1.0), nrow = 2),
    ref_dose = 10
  )

  mono_data <- crmPack::Data(
    x = c(10, 10, 20, 20, 30, 30),
    y = c(0L, 0L, 0L, 1L, 1L, 0L),
    doseGrid = c(10, 20, 30, 40),
    ID = 1:6,
    cohort = c(1L, 1L, 2L, 2L, 3L, 3L)
  )

  mono_options <- crmPack::McmcOptions(burnin = 100L, step = 1L, samples = 300L)
  mono_samples <- crmPack::mcmc(mono_data, mono_model, mono_options)

  list(
    model = mono_model,
    data = mono_data,
    options = mono_options,
    samples = mono_samples
  )
}

build_fixed_combo_prototype <- function() {
  combo_model <- crmPack:::LogisticLogNormalCombo(
    single_models = list(
      drug1 = crmPack::LogisticLogNormal(
        mean = c(-0.85, 1.0),
        cov = matrix(c(1.0, -0.5, -0.5, 1.0), nrow = 2),
        ref_dose = 10
      ),
      drug2 = crmPack::LogisticLogNormal(
        mean = c(-0.70, 0.8),
        cov = matrix(c(1.1, -0.3, -0.3, 0.9), nrow = 2),
        ref_dose = 20
      )
    ),
    gamma = 0,
    tau = 1
  )

  combo_data <- crmPack::DataCombo(
    x = cbind(
      drug1 = c(10, 10, 20, 20, 30, 30),
      drug2 = c(20, 40, 20, 40, 20, 40)
    ),
    y = c(0L, 0L, 0L, 1L, 1L, 1L),
    doseGrid = list(
      drug1 = c(10, 20, 30),
      drug2 = c(20, 40)
    ),
    ID = 1:6,
    cohort = 1:6
  )

  combo_options <- crmPack::McmcOptions(
    burnin = 100L,
    step = 1L,
    samples = 300L
  )
  combo_samples <- crmPack::mcmc(combo_data, combo_model, combo_options)

  list(
    model = combo_model,
    data = combo_data,
    options = combo_options,
    samples = combo_samples
  )
}

HierarchicalDataPrototype <- function(mono, combo) {
  stopifnot(methods::is(mono, "Data"))
  stopifnot(methods::is(combo, "DataCombo"))

  structure(
    list(arms = list(mono = mono, combo = combo)),
    class = "HierarchicalDataPrototype"
  )
}

HierarchicalModelPrototype <- function(mono_model, combo_model) {
  stopifnot(methods::is(mono_model, "LogisticLogNormal"))
  stopifnot(methods::is(combo_model, "LogisticLogNormalCombo"))

  structure(
    list(
      # Matches the design sketch: arm graph + parameter pools.
      arm_map = list(
        mono = list(
          type = "mono1",
          active = TRUE,
          use_for_recommendation = TRUE
        ),
        combo = list(
          type = "combo",
          active = TRUE,
          use_for_recommendation = TRUE
        )
      ),
      parameter_pools = list(
        drug1 = list(
          parameters = c("alpha0", "alpha1"),
          members = c("mono", "combo"),
          prior = "exchangeable"
        ),
        drug2 = list(
          parameters = c("alpha0", "alpha1"),
          members = c("combo"),
          prior = "fixed"
        ),
        interaction = list(
          parameters = c("eta"),
          members = c("combo"),
          prior = "fixed"
        )
      ),
      mono_model = mono_model,
      combo_model = combo_model
    ),
    class = "HierarchicalModelPrototype"
  )
}

# Simplest version with hardcoded JAGS model and no dynamic code generation yet.
mcmc_hierarchical_prototype <- function(data, model, options) {
  stopifnot(inherits(data, "HierarchicalDataPrototype"))
  stopifnot(inherits(model, "HierarchicalModelPrototype"))
  stopifnot(methods::is(options, "McmcOptions"))

  mono <- data$arms$mono
  combo <- data$arms$combo

  stopifnot(identical(combo@drugNames, model$combo_model@drug_names))

  prior_mean_drug2 <- model$combo_model@single_models[[2]]@params@mean
  prior_prec_drug2 <- model$combo_model@single_models[[2]]@params@prec

  ref1 <- as.numeric(model$combo_model@ref_dose[1])
  ref2 <- as.numeric(model$combo_model@ref_dose[2])

  kappa <- log(2) / 1.96

  model_file <- crmPack:::h_jags_write_model(function() {
    for (i in 1:nObsMono) {
      logit(p_mono[i]) <- alpha0_mono + alpha1_mono * log(xMono[i] / ref1)
      yMono[i] ~ dbern(p_mono[i])
    }

    for (i in 1:nObsCombo) {
      logit(p1[i]) <- alpha0_combo1 + alpha1_combo1 * log(xCombo[i, 1] / ref1)
      logit(p2[i]) <- alpha0_combo2 + alpha1_combo2 * log(xCombo[i, 2] / ref2)
      p0[i] <- p1[i] + p2[i] - p1[i] * p2[i]
      logit(p_combo[i]) <- log(p0[i] / (1 - p0[i])) +
        eta * (xCombo[i, 1] / ref1) * (xCombo[i, 2] / ref2)
      yCombo[i] ~ dbern(p_combo[i])
    }

    # Exchangeable pool for drug1 parameters across mono and combo arms.
    theta_mono_1 ~ dnorm(mu_alpha0_1, pow(tau_alpha0_1, -2))
    theta_mono_2 ~ dnorm(mu_log_alpha1_1, pow(tau_log_alpha1_1, -2))
    theta_combo1_1 ~ dnorm(mu_alpha0_1, pow(tau_alpha0_1, -2))
    theta_combo1_2 ~ dnorm(mu_log_alpha1_1, pow(tau_log_alpha1_1, -2))

    alpha0_mono <- theta_mono_1
    alpha1_mono <- exp(theta_mono_2)
    alpha0_combo1 <- theta_combo1_1
    alpha1_combo1 <- exp(theta_combo1_2)

    # Fixed prior for combo drug2 from existing fixed combo model.
    theta_combo2[1:2] ~ dmnorm(
      prior_mean_drug2[1:2],
      prior_prec_drug2[1:2, 1:2]
    )
    alpha0_combo2 <- theta_combo2[1]
    alpha1_combo2 <- exp(theta_combo2[2])

    # Fixed prior for interaction eta from existing fixed combo model.
    eta ~ dnorm(gamma_eta, tau_eta)

    # Hyperpriors for drug1 pool (prototype simplification: independent blocks).
    mu_alpha0_1 ~ dnorm(logit(0.25), pow(2.5, -2))
    mu_log_alpha1_1 ~ dnorm(0, pow(0.7, -2))
    tau_alpha0_1 ~ dlnorm(log(0.5), pow(kappa, -2))
    tau_log_alpha1_1 ~ dlnorm(log(0.25), pow(kappa, -2))
  })

  jags_data <- list(
    nObsMono = mono@nObs,
    yMono = as.integer(mono@y),
    xMono = as.numeric(mono@x),
    nObsCombo = combo@nObs,
    yCombo = as.integer(combo@y),
    xCombo = combo@x,
    ref1 = ref1,
    ref2 = ref2,
    gamma_eta = model$combo_model@gamma,
    tau_eta = model$combo_model@tau,
    prior_mean_drug2 = prior_mean_drug2,
    prior_prec_drug2 = prior_prec_drug2,
    kappa = kappa
  )

  jags_model <- rjags::jags.model(
    file = model_file,
    data = jags_data,
    quiet = TRUE,
    n.adapt = 0
  )

  update(jags_model, n.iter = options@burnin, progress.bar = "none")

  vars <- c(
    "alpha0_mono",
    "alpha1_mono",
    "alpha0_combo1",
    "alpha1_combo1",
    "alpha0_combo2",
    "alpha1_combo2",
    "eta",
    "mu_alpha0_1",
    "mu_log_alpha1_1",
    "tau_alpha0_1",
    "tau_log_alpha1_1"
  )

  samples_coda <- rjags::coda.samples(
    model = jags_model,
    variable.names = vars,
    n.iter = options@iterations - options@burnin,
    thin = options@step,
    progress.bar = "none"
  )

  smat <- as.matrix(samples_coda)

  as_col <- function(name) {
    as.numeric(smat[, name])
  }

  crmPack::Samples(
    data = list(
      alpha0_mono = as_col("alpha0_mono"),
      alpha1_mono = as_col("alpha1_mono"),
      alpha0_combo1 = as_col("alpha0_combo1"),
      alpha1_combo1 = as_col("alpha1_combo1"),
      alpha0_combo2 = as_col("alpha0_combo2"),
      alpha1_combo2 = as_col("alpha1_combo2"),
      eta = as_col("eta"),
      mu_alpha0_1 = as_col("mu_alpha0_1"),
      mu_log_alpha1_1 = as_col("mu_log_alpha1_1"),
      tau_alpha0_1 = as_col("tau_alpha0_1"),
      tau_log_alpha1_1 = as_col("tau_log_alpha1_1")
    ),
    options = options
  )
}

arm_samples_from_hierarchical <- function(h_samples, arm = c("mono", "combo")) {
  arm <- match.arg(arm)
  stopifnot(methods::is(h_samples, "Samples"))

  if (arm == "mono") {
    return(crmPack::Samples(
      data = list(
        alpha0 = h_samples@data$alpha0_mono,
        alpha1 = h_samples@data$alpha1_mono
      ),
      options = h_samples@options
    ))
  }

  crmPack::Samples(
    data = list(
      alpha0 = cbind(
        h_samples@data$alpha0_combo1,
        h_samples@data$alpha0_combo2
      ),
      alpha1 = cbind(
        h_samples@data$alpha1_combo1,
        h_samples@data$alpha1_combo2
      ),
      eta = h_samples@data$eta
    ),
    options = h_samples@options
  )
}

# Running example.
run_hierarchical_combo_prototype <- function() {
  mono <- build_mono_prototype()
  combo <- build_fixed_combo_prototype()

  h_data <- HierarchicalDataPrototype(mono = mono$data, combo = combo$data)
  h_model <- HierarchicalModelPrototype(
    mono_model = mono$model,
    combo_model = combo$model
  )

  h_options <- crmPack::McmcOptions(burnin = 300L, step = 1L, samples = 500L)
  h_samples <- mcmc_hierarchical_prototype(
    data = h_data,
    model = h_model,
    options = h_options
  )

  mono_from_h <- arm_samples_from_hierarchical(h_samples, arm = "mono")
  combo_from_h <- arm_samples_from_hierarchical(h_samples, arm = "combo")

  mono_prob_20 <- crmPack::prob(20, mono$model, mono_from_h)
  combo_prob_20_20 <- crmPack::prob(
    c(drug1 = 20, drug2 = 20),
    combo$model,
    combo_from_h
  )

  list(
    mono = mono,
    combo = combo,
    hierarchical = list(
      data = h_data,
      model = h_model,
      options = h_options,
      samples = h_samples,
      arm_samples = list(mono = mono_from_h, combo = combo_from_h),
      checks = list(
        mean_prob_mono_at_20 = mean(mono_prob_20),
        mean_prob_combo_at_20_20 = mean(combo_prob_20_20)
      )
    )
  )
}

result1 <- run_hierarchical_combo_prototype()
str(result1, 2)

## Second try: Flexible mcmc function and dynamically generated jags code

# Helper functions for dynamic JAGS code generation. In a real implementation, these
# would be more robust and handle edge cases, but for the prototype we can keep
# them simple and focused on the expected input structures.
h_get_arm_models_prototype <- function(model) {
  if (!is.null(model$arm_models)) {
    return(model$arm_models)
  }

  arm_models <- list()
  if (!is.null(model$mono_model)) {
    arm_models$mono <- model$mono_model
  }
  if (!is.null(model$combo_model)) {
    arm_models$combo <- model$combo_model
  }
  arm_models
}

h_pool_exchangeable_members <- function(parameter_pools, pool_name) {
  pool <- parameter_pools[[pool_name]]
  if (is.null(pool)) {
    return(character())
  }
  if (!identical(pool$prior, "exchangeable")) {
    return(character())
  }
  as.character(pool$members)
}

# Extract the lines of code from a function body, removing the enclosing braces if present.
h_model_block_lines <- function(fun) {
  txt <- deparse(body(fun), control = NULL)
  txt <- trimws(txt)
  if (length(txt) == 0L) {
    return(character())
  }
  if (identical(txt[1], "{")) {
    txt <- txt[-1]
  }
  if (length(txt) > 0L && identical(txt[length(txt)], "}")) {
    txt <- txt[-length(txt)]
  }
  txt
}

# Replace tokens in the lines of code with the provided replacements. Tokens are replaced
# only when they appear as standalone identifiers (not part of other words). Longer tokens
# are replaced first to avoid partial replacements.
h_replace_tokens <- function(lines, replacements) {
  if (length(lines) == 0L || length(replacements) == 0L) {
    return(lines)
  }

  keys <- names(replacements)
  keys <- keys[order(nchar(keys), decreasing = TRUE)]

  out <- lines
  for (key in keys) {
    out <- gsub(
      pattern = sprintf("(?<![A-Za-z0-9_])%s(?![A-Za-z0-9_])", key),
      replacement = replacements[[key]],
      x = out,
      perl = TRUE
    )
  }
  out
}

# Render the lines of code from a model slot function, replacing tokens with the provided replacements.
# This allows us to reuse the same model code structure for different arms and parameter names by defining
# the model code with generic tokens and then substituting them as needed.
h_render_model_slot_lines <- function(fun, replacements = character()) {
  h_replace_tokens(
    lines = h_model_block_lines(fun),
    replacements = replacements
  )
}

# Add the JAGS code and data for a mono arm, handling exchangeable vs fixed priors
# and updating the lines, prior_lines, hyper_lines, jags_data, sample_vars, and arm_param_map
# accordingly. This function is called from the main hierarchical JAGS code generation function
# for each mono arm in the data/model, allowing us to build up the full JAGS model
# dynamically based on the structure of the data/model and the specified parameter pools.
h_add_mono_arm_dynamic <- function(
  arm,
  arm_data,
  arm_model,
  mono_pool,
  exchangeable_drug1,
  exchangeable_drug2,
  lines,
  prior_lines,
  hyper_lines,
  jags_data,
  sample_vars,
  arm_param_map
) {
  n_obs <- paste0("nObs_", arm)
  y <- paste0("y_", arm)
  x <- paste0("x_", arm)
  ref <- paste0("ref_dose_", arm)
  alpha0 <- paste0("alpha0_", arm)
  alpha1 <- paste0("alpha1_", arm)

  datamodel_lines <- h_render_model_slot_lines(
    fun = arm_model@datamodel,
    replacements = c(
      nObs = n_obs,
      y = y,
      x = x,
      ref_dose = ref,
      alpha0 = alpha0,
      alpha1 = alpha1,
      p = paste0("p_", arm)
    )
  )

  lines <- c(lines, datamodel_lines)

  jags_data[[n_obs]] <- arm_data@nObs
  jags_data[[y]] <- as.integer(arm_data@y)
  jags_data[[x]] <- as.numeric(arm_data@x)
  jags_data[[ref]] <- as.numeric(arm_model@ref_dose)

  exchangeable_members <- if (identical(mono_pool, "drug2")) {
    exchangeable_drug2
  } else {
    exchangeable_drug1
  }

  pool_suffix <- if (identical(mono_pool, "drug2")) "drug2" else "drug1"

  if (arm %in% exchangeable_members) {
    tvec <- paste0("theta_", arm, "_", pool_suffix)

    prior_lines <- c(
      prior_lines,
      sprintf(
        "%s[1:2] ~ dmnorm(mu_%s[1:2], prec_%s[1:2, 1:2])",
        tvec,
        pool_suffix,
        pool_suffix
      ),
      sprintf("%s <- %s[1]", alpha0, tvec),
      sprintf("%s <- exp(%s[2])", alpha1, tvec)
    )
  } else {
    mean_name <- paste0("prior_mean_", arm)
    prec_name <- paste0("prior_prec_", arm)
    theta_name <- paste0("theta_", arm, "_fixed")

    jags_data[[mean_name]] <- arm_model@params@mean
    jags_data[[prec_name]] <- arm_model@params@prec

    prior_lines <- c(
      prior_lines,
      h_render_model_slot_lines(
        fun = arm_model@priormodel,
        replacements = c(
          theta = theta_name,
          mean = mean_name,
          prec = prec_name,
          alpha0 = alpha0,
          alpha1 = alpha1
        )
      )
    )
  }

  sample_vars <- c(sample_vars, alpha0, alpha1)
  arm_param_map[[arm]] <- list(type = "mono", alpha0 = alpha0, alpha1 = alpha1)

  list(
    lines = lines,
    prior_lines = prior_lines,
    hyper_lines = hyper_lines,
    jags_data = jags_data,
    sample_vars = sample_vars,
    arm_param_map = arm_param_map
  )
}

# Now the same for the combo arm.
h_add_combo_arm_dynamic <- function(
  arm,
  arm_data,
  arm_model,
  exchangeable_drug1,
  exchangeable_drug2,
  exchangeable_eta,
  lines,
  prior_lines,
  hyper_lines,
  jags_data,
  sample_vars,
  arm_param_map
) {
  n_obs <- paste0("nObs_", arm)
  y <- paste0("y_", arm)
  x <- paste0("x_", arm)
  ref <- paste0("ref_dose_", arm)

  alpha0_1 <- paste0("alpha0_", arm, "_1")
  alpha1_1 <- paste0("alpha1_", arm, "_1")
  alpha0_2 <- paste0("alpha0_", arm, "_2")
  alpha1_2 <- paste0("alpha1_", arm, "_2")
  eta <- paste0("eta_", arm)

  datamodel_lines <- h_render_model_slot_lines(
    fun = arm_model@datamodel,
    replacements = c(
      nObs = n_obs,
      y = y,
      x = x,
      ref_dose = ref,
      alpha0 = paste0("alpha0_", arm),
      alpha1 = paste0("alpha1_", arm),
      eta = eta,
      p_single = paste0("p_single_", arm),
      p0 = paste0("p0_", arm),
      p = paste0("p_", arm)
    )
  )

  lines <- c(lines, datamodel_lines)

  jags_data[[n_obs]] <- arm_data@nObs
  jags_data[[y]] <- as.integer(arm_data@y)
  jags_data[[x]] <- arm_data@x
  jags_data[[ref]] <- as.numeric(arm_model@ref_dose)

  has_exchangeable <-
    (arm %in% exchangeable_drug1) ||
    (arm %in% exchangeable_drug2) ||
    (arm %in% exchangeable_eta)

  if (!has_exchangeable) {
    prior_mean_name <- paste0("prior_mean_", arm)
    prior_prec_name <- paste0("prior_prec_", arm)
    theta_name <- paste0("theta_", arm)
    gamma_eta <- paste0("gamma_eta_", arm)
    tau_eta <- paste0("tau_eta_", arm)

    prior_mean <- do.call(
      cbind,
      lapply(arm_model@single_models, function(sm) sm@params@mean)
    )
    prior_prec <- array(
      data = do.call(
        c,
        lapply(arm_model@single_models, function(sm) sm@params@prec)
      ),
      dim = c(2, 2, 2)
    )

    jags_data[[prior_mean_name]] <- prior_mean
    jags_data[[prior_prec_name]] <- prior_prec
    jags_data[[gamma_eta]] <- arm_model@gamma
    jags_data[[tau_eta]] <- arm_model@tau

    prior_lines <- c(
      prior_lines,
      h_render_model_slot_lines(
        fun = arm_model@priormodel,
        replacements = c(
          theta = theta_name,
          prior_mean = prior_mean_name,
          prior_prec = prior_prec_name,
          alpha0 = paste0("alpha0_", arm),
          alpha1 = paste0("alpha1_", arm),
          eta = eta,
          gamma = gamma_eta,
          tau = tau_eta,
          log_eta = paste0("log_eta_", arm)
        )
      ),
      sprintf("%s <- alpha0_%s[1]", alpha0_1, arm),
      sprintf("%s <- alpha1_%s[1]", alpha1_1, arm),
      sprintf("%s <- alpha0_%s[2]", alpha0_2, arm),
      sprintf("%s <- alpha1_%s[2]", alpha1_2, arm)
    )

    sample_vars <- c(sample_vars, alpha0_1, alpha1_1, alpha0_2, alpha1_2, eta)
    arm_param_map[[arm]] <- list(
      type = "combo",
      alpha0 = c(alpha0_1, alpha0_2),
      alpha1 = c(alpha1_1, alpha1_2),
      eta = eta
    )

    return(list(
      lines = lines,
      prior_lines = prior_lines,
      hyper_lines = hyper_lines,
      jags_data = jags_data,
      sample_vars = sample_vars,
      arm_param_map = arm_param_map
    ))
  }

  if (arm %in% exchangeable_drug1) {
    tvec <- paste0("theta_", arm, "_drug1")
    prior_lines <- c(
      prior_lines,
      sprintf(
        "%s[1:2] ~ dmnorm(mu_drug1[1:2], prec_drug1[1:2, 1:2])",
        tvec
      ),
      sprintf("%s <- %s[1]", alpha0_1, tvec),
      sprintf("%s <- exp(%s[2])", alpha1_1, tvec)
    )
  } else {
    mean_1 <- paste0("prior_mean_", arm, "_1")
    prec_1 <- paste0("prior_prec_", arm, "_1")
    theta_1 <- paste0("theta_", arm, "_drug1_fixed")
    jags_data[[mean_1]] <- arm_model@single_models[[1]]@params@mean
    jags_data[[prec_1]] <- arm_model@single_models[[1]]@params@prec
    prior_lines <- c(
      prior_lines,
      sprintf(
        "%s[1:2] ~ dmnorm(%s[1:2], %s[1:2, 1:2])",
        theta_1,
        mean_1,
        prec_1
      ),
      sprintf("%s <- %s[1]", alpha0_1, theta_1),
      sprintf("%s <- exp(%s[2])", alpha1_1, theta_1)
    )
  }

  if (arm %in% exchangeable_drug2) {
    tvec <- paste0("theta_", arm, "_drug2")
    prior_lines <- c(
      prior_lines,
      sprintf(
        "%s[1:2] ~ dmnorm(mu_drug2[1:2], prec_drug2[1:2, 1:2])",
        tvec
      ),
      sprintf("%s <- %s[1]", alpha0_2, tvec),
      sprintf("%s <- exp(%s[2])", alpha1_2, tvec)
    )
  } else {
    mean_2 <- paste0("prior_mean_", arm, "_2")
    prec_2 <- paste0("prior_prec_", arm, "_2")
    theta_2 <- paste0("theta_", arm, "_drug2_fixed")
    jags_data[[mean_2]] <- arm_model@single_models[[2]]@params@mean
    jags_data[[prec_2]] <- arm_model@single_models[[2]]@params@prec
    prior_lines <- c(
      prior_lines,
      sprintf(
        "%s[1:2] ~ dmnorm(%s[1:2], %s[1:2, 1:2])",
        theta_2,
        mean_2,
        prec_2
      ),
      sprintf("%s <- %s[1]", alpha0_2, theta_2),
      sprintf("%s <- exp(%s[2])", alpha1_2, theta_2)
    )
  }

  if (arm %in% exchangeable_eta) {
    prior_lines <- c(
      prior_lines,
      sprintf("%s ~ dnorm(mu_eta, pow(tau_eta_pool, -2))", eta)
    )
  } else {
    gamma_eta <- paste0("gamma_eta_", arm)
    tau_eta <- paste0("tau_eta_", arm)
    jags_data[[gamma_eta]] <- arm_model@gamma
    jags_data[[tau_eta]] <- arm_model@tau
    prior_lines <- c(
      prior_lines,
      sprintf("%s ~ dnorm(%s, %s)", eta, gamma_eta, tau_eta)
    )
  }

  # Reused combo datamodel expects alpha0[j] and alpha1[j] style indexing.
  prior_lines <- c(
    prior_lines,
    sprintf("alpha0_%s[1] <- %s", arm, alpha0_1),
    sprintf("alpha0_%s[2] <- %s", arm, alpha0_2),
    sprintf("alpha1_%s[1] <- %s", arm, alpha1_1),
    sprintf("alpha1_%s[2] <- %s", arm, alpha1_2)
  )

  sample_vars <- c(sample_vars, alpha0_1, alpha1_1, alpha0_2, alpha1_2, eta)
  arm_param_map[[arm]] <- list(
    type = "combo",
    alpha0 = c(alpha0_1, alpha0_2),
    alpha1 = c(alpha1_1, alpha1_2),
    eta = eta
  )

  list(
    lines = lines,
    prior_lines = prior_lines,
    hyper_lines = hyper_lines,
    jags_data = jags_data,
    sample_vars = sample_vars,
    arm_param_map = arm_param_map
  )
}

# Main function to compile the JAGS model for the hierarchical mono + combo prototype.
h_compile_hierarchical_jags_dynamic <- function(data, model) {
  stopifnot(inherits(data, "HierarchicalDataPrototype"))
  stopifnot(inherits(model, "HierarchicalModelPrototype"))

  arm_models <- h_get_arm_models_prototype(model)
  arm_names <- names(data$arms)
  stopifnot(length(arm_names) > 0L)
  stopifnot(all(arm_names %in% names(arm_models)))

  exchangeable_drug1 <- h_pool_exchangeable_members(
    model$parameter_pools,
    "drug1"
  )
  exchangeable_drug2 <- h_pool_exchangeable_members(
    model$parameter_pools,
    "drug2"
  )
  exchangeable_eta <- h_pool_exchangeable_members(
    model$parameter_pools,
    "interaction"
  )

  lines <- c("model {")
  prior_lines <- character()
  hyper_lines <- character()
  jags_data <- list()
  sample_vars <- character()
  arm_param_map <- list()

  kappa <- log(2) / 1.96
  jags_data$kappa <- kappa

  if (length(exchangeable_drug1) > 0L) {
    hyper_lines <- c(
      hyper_lines,
      "mu_alpha0_drug1 ~ dnorm(logit(0.25), pow(2.5, -2))",
      "mu_log_alpha1_drug1 ~ dnorm(0, pow(0.7, -2))",
      "tau_alpha0_drug1 ~ dlnorm(log(0.5), pow(kappa, -2))",
      "tau_log_alpha1_drug1 ~ dlnorm(log(0.25), pow(kappa, -2))",
      "rho_drug1 ~ dunif(-1, 1)",
      "mu_drug1[1] <- mu_alpha0_drug1",
      "mu_drug1[2] <- mu_log_alpha1_drug1",
      "cov_drug1[1, 1] <- pow(tau_alpha0_drug1, 2)",
      "cov_drug1[2, 2] <- pow(tau_log_alpha1_drug1, 2)",
      "cov_drug1[1, 2] <- rho_drug1 * tau_alpha0_drug1 * tau_log_alpha1_drug1",
      "cov_drug1[2, 1] <- cov_drug1[1, 2]",
      "prec_drug1[1:2, 1:2] <- inverse(cov_drug1[1:2, 1:2])"
    )
    sample_vars <- c(
      sample_vars,
      "mu_alpha0_drug1",
      "mu_log_alpha1_drug1",
      "tau_alpha0_drug1",
      "tau_log_alpha1_drug1",
      "rho_drug1"
    )
  }

  if (length(exchangeable_drug2) > 0L) {
    hyper_lines <- c(
      hyper_lines,
      "mu_alpha0_drug2 ~ dnorm(logit(0.25), pow(2.5, -2))",
      "mu_log_alpha1_drug2 ~ dnorm(0, 1)",
      "tau_alpha0_drug2 ~ dlnorm(log(0.75), pow(kappa, -2))",
      "tau_log_alpha1_drug2 ~ dlnorm(log(0.25), pow(kappa, -2))",
      "rho_drug2 ~ dunif(-1, 1)",
      "mu_drug2[1] <- mu_alpha0_drug2",
      "mu_drug2[2] <- mu_log_alpha1_drug2",
      "cov_drug2[1, 1] <- pow(tau_alpha0_drug2, 2)",
      "cov_drug2[2, 2] <- pow(tau_log_alpha1_drug2, 2)",
      "cov_drug2[1, 2] <- rho_drug2 * tau_alpha0_drug2 * tau_log_alpha1_drug2",
      "cov_drug2[2, 1] <- cov_drug2[1, 2]",
      "prec_drug2[1:2, 1:2] <- inverse(cov_drug2[1:2, 1:2])"
    )
    sample_vars <- c(
      sample_vars,
      "mu_alpha0_drug2",
      "mu_log_alpha1_drug2",
      "tau_alpha0_drug2",
      "tau_log_alpha1_drug2",
      "rho_drug2"
    )
  }

  if (length(exchangeable_eta) > 0L) {
    hyper_lines <- c(
      hyper_lines,
      "mu_eta ~ dnorm(0, 1)",
      "tau_eta_pool ~ dlnorm(log(0.125), pow(kappa, -2))"
    )
    sample_vars <- c(sample_vars, "mu_eta", "tau_eta_pool")
  }

  for (arm in arm_names) {
    arm_data <- data$arms[[arm]]
    arm_model <- arm_models[[arm]]
    arm_type <- model$arm_map[[arm]]$type

    mono_pool <- if (identical(arm_type, "mono2")) "drug2" else "drug1"

    if (
      methods::is(arm_model, "LogisticLogNormal") &&
        methods::is(arm_data, "Data")
    ) {
      out <- h_add_mono_arm_dynamic(
        arm = arm,
        arm_data = arm_data,
        arm_model = arm_model,
        mono_pool = mono_pool,
        exchangeable_drug1 = exchangeable_drug1,
        exchangeable_drug2 = exchangeable_drug2,
        lines = lines,
        prior_lines = prior_lines,
        hyper_lines = hyper_lines,
        jags_data = jags_data,
        sample_vars = sample_vars,
        arm_param_map = arm_param_map
      )
    } else if (
      methods::is(arm_model, "LogisticLogNormalCombo") &&
        methods::is(arm_data, "DataCombo")
    ) {
      out <- h_add_combo_arm_dynamic(
        arm = arm,
        arm_data = arm_data,
        arm_model = arm_model,
        exchangeable_drug1 = exchangeable_drug1,
        exchangeable_drug2 = exchangeable_drug2,
        exchangeable_eta = exchangeable_eta,
        lines = lines,
        prior_lines = prior_lines,
        hyper_lines = hyper_lines,
        jags_data = jags_data,
        sample_vars = sample_vars,
        arm_param_map = arm_param_map
      )
    } else {
      stop(
        sprintf(
          "Unsupported arm combination for arm '%s': data class '%s', model class '%s'",
          arm,
          class(arm_data)[1],
          class(arm_model)[1]
        ),
        call. = FALSE
      )
    }

    lines <- out$lines
    prior_lines <- out$prior_lines
    hyper_lines <- out$hyper_lines
    jags_data <- out$jags_data
    sample_vars <- out$sample_vars
    arm_param_map <- out$arm_param_map
  }

  all_lines <- c(lines, prior_lines, hyper_lines, "}")

  model_file <- tempfile(pattern = "hierarchical_dynamic_", fileext = ".jags")
  writeLines(all_lines, con = model_file)

  list(
    model_file = model_file,
    jags_data = jags_data,
    sample_vars = unique(sample_vars),
    arm_param_map = arm_param_map,
    arm_models = arm_models,
    jags_lines = all_lines
  )
}

# Main function to run MCMC for the hierarchical mono + combo prototype.
mcmc_hierarchical_dynamic_prototype <- function(data, model, options) {
  stopifnot(methods::is(options, "McmcOptions"))

  compiled <- h_compile_hierarchical_jags_dynamic(data = data, model = model)

  jags_model <- rjags::jags.model(
    file = compiled$model_file,
    data = compiled$jags_data,
    quiet = TRUE,
    n.adapt = 0
  )

  update(jags_model, n.iter = options@burnin, progress.bar = "none")

  samples_coda <- rjags::coda.samples(
    model = jags_model,
    variable.names = compiled$sample_vars,
    n.iter = options@iterations - options@burnin,
    thin = options@step,
    progress.bar = "none"
  )

  smat <- as.matrix(samples_coda)
  as_col <- function(name) {
    as.numeric(smat[, name])
  }

  sample_data <- setNames(
    lapply(compiled$sample_vars, as_col),
    compiled$sample_vars
  )

  list(
    samples = crmPack::Samples(data = sample_data, options = options),
    arm_param_map = compiled$arm_param_map,
    arm_models = compiled$arm_models,
    jags_model_file = compiled$model_file,
    jags_code = compiled$jags_lines
  )
}

# Helper function to extract the samples for a specific arm from the hierarchical MCMC fit.
arm_samples_from_hierarchical_dynamic <- function(h_fit, arm) {
  stopifnot(
    is.list(h_fit),
    !is.null(h_fit$samples),
    !is.null(h_fit$arm_param_map)
  )
  stopifnot(methods::is(h_fit$samples, "Samples"))
  stopifnot(arm %in% names(h_fit$arm_param_map))

  map <- h_fit$arm_param_map[[arm]]
  src <- h_fit$samples@data

  if (identical(map$type, "mono")) {
    return(crmPack::Samples(
      data = list(alpha0 = src[[map$alpha0]], alpha1 = src[[map$alpha1]]),
      options = h_fit$samples@options
    ))
  }

  if (identical(map$type, "combo")) {
    return(crmPack::Samples(
      data = list(
        alpha0 = cbind(src[[map$alpha0[1]]], src[[map$alpha0[2]]]),
        alpha1 = cbind(src[[map$alpha1[1]]], src[[map$alpha1[2]]]),
        eta = src[[map$eta]]
      ),
      options = h_fit$samples@options
    ))
  }

  stop(sprintf("Unsupported arm type '%s'.", map$type), call. = FALSE)
}

# Running example for the dynamic JAGS code generation and MCMC sampling.
run_hierarchical_combo_dynamic_prototype <- function() {
  mono <- build_mono_prototype()
  combo <- build_fixed_combo_prototype()

  h_data <- HierarchicalDataPrototype(mono = mono$data, combo = combo$data)
  h_model <- HierarchicalModelPrototype(
    mono_model = mono$model,
    combo_model = combo$model
  )

  # In the dynamic path, we make arm-model mapping explicit.
  h_model$arm_models <- list(
    mono = mono$model,
    combo = combo$model
  )

  h_options <- crmPack::McmcOptions(burnin = 300L, step = 1L, samples = 500L)
  h_fit <- mcmc_hierarchical_dynamic_prototype(
    data = h_data,
    model = h_model,
    options = h_options
  )

  mono_from_h <- arm_samples_from_hierarchical_dynamic(h_fit, arm = "mono")
  combo_from_h <- arm_samples_from_hierarchical_dynamic(h_fit, arm = "combo")

  mono_prob_20 <- crmPack::prob(20, mono$model, mono_from_h)
  combo_prob_20_20 <- crmPack::prob(
    c(drug1 = 20, drug2 = 20),
    combo$model,
    combo_from_h
  )

  list(
    mono = mono,
    combo = combo,
    hierarchical_dynamic = list(
      data = h_data,
      model = h_model,
      options = h_options,
      fit = h_fit,
      arm_samples = list(mono = mono_from_h, combo = combo_from_h),
      checks = list(
        mean_prob_mono_at_20 = mean(mono_prob_20),
        mean_prob_combo_at_20_20 = mean(combo_prob_20_20)
      )
    )
  )
}
result3 <- run_hierarchical_combo_dynamic_prototype()
str(result3, 2)
result3$hierarchical_dynamic$model

## Third try: Now also with a mono arm for the other drug, to see if the dynamic code
## above can handle that too
build_mono2_prototype <- function() {
  mono2_model <- crmPack::LogisticLogNormal(
    mean = c(-0.70, 0.8),
    cov = matrix(c(1.1, -0.3, -0.3, 0.9), nrow = 2),
    ref_dose = 20
  )

  mono2_data <- crmPack::Data(
    x = c(20, 20, 40, 40, 60, 60),
    y = c(0L, 0L, 0L, 1L, 1L, 0L),
    doseGrid = c(20, 40, 60),
    ID = 1:6,
    cohort = c(1L, 1L, 2L, 2L, 3L, 3L)
  )

  mono2_options <- crmPack::McmcOptions(
    burnin = 100L,
    step = 1L,
    samples = 300L
  )
  mono2_samples <- crmPack::mcmc(mono2_data, mono2_model, mono2_options)

  list(
    model = mono2_model,
    data = mono2_data,
    options = mono2_options,
    samples = mono2_samples
  )
}

run_hierarchical_combo_dynamic_three_arms_prototype <- function() {
  mono1 <- build_mono_prototype()
  mono2 <- build_mono2_prototype()
  combo <- build_fixed_combo_prototype()

  h_data <- structure(
    list(
      arms = list(mono1 = mono1$data, mono2 = mono2$data, combo = combo$data)
    ),
    class = "HierarchicalDataPrototype"
  )

  h_model <- structure(
    list(
      arm_map = list(
        mono1 = list(
          type = "mono1",
          active = TRUE,
          use_for_recommendation = TRUE
        ),
        mono2 = list(
          type = "mono2",
          active = TRUE,
          use_for_recommendation = TRUE
        ),
        combo = list(
          type = "combo",
          active = TRUE,
          use_for_recommendation = TRUE
        )
      ),
      parameter_pools = list(
        drug1 = list(
          parameters = c("alpha0", "alpha1"),
          members = c("mono1", "combo"),
          prior = "exchangeable"
        ),
        drug2 = list(
          parameters = c("alpha0", "alpha1"),
          members = c("mono2", "combo"),
          prior = "exchangeable"
        ),
        interaction = list(
          parameters = c("eta"),
          members = c("combo"),
          prior = "fixed"
        )
      ),
      arm_models = list(
        mono1 = mono1$model,
        mono2 = mono2$model,
        combo = combo$model
      ),
      mono_model = mono1$model,
      combo_model = combo$model
    ),
    class = "HierarchicalModelPrototype"
  )

  h_options <- crmPack::McmcOptions(burnin = 300L, step = 1L, samples = 500L)
  h_fit <- mcmc_hierarchical_dynamic_prototype(
    data = h_data,
    model = h_model,
    options = h_options
  )

  mono1_from_h <- arm_samples_from_hierarchical_dynamic(h_fit, arm = "mono1")
  mono2_from_h <- arm_samples_from_hierarchical_dynamic(h_fit, arm = "mono2")
  combo_from_h <- arm_samples_from_hierarchical_dynamic(h_fit, arm = "combo")

  mono1_prob_20 <- crmPack::prob(20, mono1$model, mono1_from_h)
  mono2_prob_40 <- crmPack::prob(40, mono2$model, mono2_from_h)
  combo_prob_20_40 <- crmPack::prob(
    c(drug1 = 20, drug2 = 40),
    combo$model,
    combo_from_h
  )

  list(
    mono1 = mono1,
    mono2 = mono2,
    combo = combo,
    hierarchical_dynamic = list(
      data = h_data,
      model = h_model,
      options = h_options,
      fit = h_fit,
      arm_samples = list(
        mono1 = mono1_from_h,
        mono2 = mono2_from_h,
        combo = combo_from_h
      ),
      checks = list(
        mean_prob_mono1_at_20 = mean(mono1_prob_20),
        mean_prob_mono2_at_40 = mean(mono2_prob_40),
        mean_prob_combo_at_20_40 = mean(combo_prob_20_40)
      )
    )
  )
}
result4 <- run_hierarchical_combo_dynamic_three_arms_prototype()
str(result4, 2)

## Fourth try: add a historical mono arm (non-enrolling) that still contributes
## to the joint MCMC fit.

build_historical_mono1_prototype <- function() {
  hist_model <- crmPack::LogisticLogNormal(
    mean = c(-0.85, 1.0),
    cov = matrix(c(1.0, -0.5, -0.5, 1.0), nrow = 2),
    ref_dose = 10
  )

  hist_data <- crmPack::Data(
    x = c(10, 10, 20, 20, 30, 30, 40, 40),
    y = c(0L, 0L, 0L, 1L, 1L, 1L, 1L, 0L),
    doseGrid = c(10, 20, 30, 40),
    ID = 101:108,
    cohort = c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L)
  )

  hist_options <- crmPack::McmcOptions(burnin = 100L, step = 1L, samples = 300L)
  hist_samples <- crmPack::mcmc(hist_data, hist_model, hist_options)

  list(
    model = hist_model,
    data = hist_data,
    options = hist_options,
    samples = hist_samples
  )
}

run_hierarchical_combo_dynamic_four_arms_with_hist_prototype <- function() {
  mono1 <- build_mono_prototype()
  mono2 <- build_mono2_prototype()
  combo <- build_fixed_combo_prototype()
  hist_mono1 <- build_historical_mono1_prototype()

  h_data <- structure(
    list(
      arms = list(
        mono1 = mono1$data,
        mono2 = mono2$data,
        combo = combo$data,
        hist_mono1 = hist_mono1$data
      )
    ),
    class = "HierarchicalDataPrototype"
  )

  h_model <- structure(
    list(
      arm_map = list(
        mono1 = list(
          type = "mono1",
          active = TRUE,
          use_for_recommendation = TRUE
        ),
        mono2 = list(
          type = "mono2",
          active = TRUE,
          use_for_recommendation = TRUE
        ),
        combo = list(
          type = "combo",
          active = TRUE,
          use_for_recommendation = TRUE
        ),
        hist_mono1 = list(
          type = "historical_mono1",
          active = FALSE,
          use_for_recommendation = FALSE
        )
      ),
      parameter_pools = list(
        drug1 = list(
          parameters = c("alpha0", "alpha1"),
          members = c("hist_mono1", "mono1", "combo"),
          prior = "exchangeable"
        ),
        drug2 = list(
          parameters = c("alpha0", "alpha1"),
          members = c("mono2", "combo"),
          prior = "exchangeable"
        ),
        interaction = list(
          parameters = c("eta"),
          members = c("combo"),
          prior = "fixed"
        )
      ),
      arm_models = list(
        mono1 = mono1$model,
        mono2 = mono2$model,
        combo = combo$model,
        hist_mono1 = hist_mono1$model
      ),
      mono_model = mono1$model,
      combo_model = combo$model
    ),
    class = "HierarchicalModelPrototype"
  )

  h_options <- crmPack::McmcOptions(burnin = 300L, step = 1L, samples = 500L)
  h_fit <- mcmc_hierarchical_dynamic_prototype(
    data = h_data,
    model = h_model,
    options = h_options
  )
  # cat(h_fit$jags_code, sep = "\n")

  mono1_from_h <- arm_samples_from_hierarchical_dynamic(h_fit, arm = "mono1")
  mono2_from_h <- arm_samples_from_hierarchical_dynamic(h_fit, arm = "mono2")
  combo_from_h <- arm_samples_from_hierarchical_dynamic(h_fit, arm = "combo")
  hist_mono1_from_h <- arm_samples_from_hierarchical_dynamic(
    h_fit,
    arm = "hist_mono1"
  )

  mono1_prob_20 <- crmPack::prob(20, mono1$model, mono1_from_h)
  mono2_prob_40 <- crmPack::prob(40, mono2$model, mono2_from_h)
  combo_prob_20_40 <- crmPack::prob(
    c(drug1 = 20, drug2 = 40),
    combo$model,
    combo_from_h
  )
  hist_mono1_prob_20 <- crmPack::prob(20, hist_mono1$model, hist_mono1_from_h)

  list(
    mono1 = mono1,
    mono2 = mono2,
    combo = combo,
    hist_mono1 = hist_mono1,
    hierarchical_dynamic = list(
      data = h_data,
      model = h_model,
      options = h_options,
      fit = h_fit,
      arm_samples = list(
        mono1 = mono1_from_h,
        mono2 = mono2_from_h,
        combo = combo_from_h,
        hist_mono1 = hist_mono1_from_h
      ),
      checks = list(
        mean_prob_mono1_at_20 = mean(mono1_prob_20),
        mean_prob_mono2_at_40 = mean(mono2_prob_40),
        mean_prob_combo_at_20_40 = mean(combo_prob_20_40),
        mean_prob_hist_mono1_at_20 = mean(hist_mono1_prob_20)
      ),
      active_arms = names(which(vapply(
        h_model$arm_map,
        function(x) isTRUE(x$active),
        logical(1L)
      )))
    )
  )
}

result5 <- run_hierarchical_combo_dynamic_four_arms_with_hist_prototype()
str(result5, 2)


## Fifth try: full 5D joint source-level prior
## theta_j = (alpha1, log(beta1), alpha2, log(beta2), eta)'
## with theta_j ~ Normal(mu, Sigma) and block-structured Sigma.

mcmc_hierarchical_joint5d_prior_prototype <- function(data, options) {
  stopifnot(inherits(data, "HierarchicalDataPrototype"))
  stopifnot(methods::is(options, "McmcOptions"))

  stopifnot(all(c("mono1", "mono2", "combo") %in% names(data$arms)))

  mono1 <- data$arms$mono1
  mono2 <- data$arms$mono2
  combo <- data$arms$combo
  has_hist <- "hist_mono1" %in% names(data$arms)
  if (has_hist) {
    hist_mono1 <- data$arms$hist_mono1
  }

  ref1 <- 10
  ref2 <- 20
  kappa <- log(2) / 1.96

  model_core <- function() {
    for (i in 1:nObs_mono1) {
      logit(p_mono1[i]) <- theta_mono1[1] +
        exp(theta_mono1[2]) *
          log(x_mono1[i] / ref1)
      y_mono1[i] ~ dbern(p_mono1[i])
    }

    for (i in 1:nObs_mono2) {
      logit(p_mono2[i]) <- theta_mono2[3] +
        exp(theta_mono2[4]) *
          log(x_mono2[i] / ref2)
      y_mono2[i] ~ dbern(p_mono2[i])
    }

    for (i in 1:nObs_combo) {
      logit(p_combo_1[i]) <- theta_combo[1] +
        exp(theta_combo[2]) *
          log(x_combo[i, 1] / ref1)
      logit(p_combo_2[i]) <- theta_combo[3] +
        exp(theta_combo[4]) *
          log(x_combo[i, 2] / ref2)
      p0_combo[i] <- p_combo_1[i] + p_combo_2[i] - p_combo_1[i] * p_combo_2[i]
      logit(p_combo[i]) <- log(p0_combo[i] / (1 - p0_combo[i])) +
        theta_combo[5] * (x_combo[i, 1] / ref1) * (x_combo[i, 2] / ref2)
      y_combo[i] ~ dbern(p_combo[i])
    }

    theta_mono1[1:5] ~ dmnorm(mu[1:5], prec[1:5, 1:5])
    theta_mono2[1:5] ~ dmnorm(mu[1:5], prec[1:5, 1:5])
    theta_combo[1:5] ~ dmnorm(mu[1:5], prec[1:5, 1:5])

    mu[1] ~ dnorm(logit(0.25), pow(2.5, -2))
    mu[2] ~ dnorm(0, pow(0.7, -2))
    mu[3] ~ dnorm(logit(0.25), pow(2.5, -2))
    mu[4] ~ dnorm(0, 1)
    mu[5] ~ dnorm(0, 1)

    tau_alpha1 ~ dlnorm(log(0.5), pow(kappa, -2))
    tau_beta1 ~ dlnorm(log(0.25), pow(kappa, -2))
    tau_alpha2 ~ dlnorm(log(0.75), pow(kappa, -2))
    tau_beta2 ~ dlnorm(log(0.25), pow(kappa, -2))
    tau_eta ~ dlnorm(log(0.125), pow(kappa, -2))

    rho1 ~ dunif(-0.95, 0.95)
    rho2 ~ dunif(-0.95, 0.95)

    cov[1, 1] <- pow(tau_alpha1, 2)
    cov[2, 2] <- pow(tau_beta1, 2)
    cov[1, 2] <- rho1 * tau_alpha1 * tau_beta1
    cov[2, 1] <- cov[1, 2]

    cov[3, 3] <- pow(tau_alpha2, 2)
    cov[4, 4] <- pow(tau_beta2, 2)
    cov[3, 4] <- rho2 * tau_alpha2 * tau_beta2
    cov[4, 3] <- cov[3, 4]

    cov[5, 5] <- pow(tau_eta, 2)

    cov[1, 3] <- 0
    cov[1, 4] <- 0
    cov[1, 5] <- 0
    cov[2, 3] <- 0
    cov[2, 4] <- 0
    cov[2, 5] <- 0
    cov[3, 1] <- 0
    cov[3, 2] <- 0
    cov[3, 5] <- 0
    cov[4, 1] <- 0
    cov[4, 2] <- 0
    cov[4, 5] <- 0
    cov[5, 1] <- 0
    cov[5, 2] <- 0
    cov[5, 3] <- 0
    cov[5, 4] <- 0

    prec[1:5, 1:5] <- inverse(cov[1:5, 1:5])
  }

  model_fun <- model_core
  if (has_hist) {
    model_fun <- crmPack:::h_jags_join_models(
      model_fun,
      function() {
        for (i in 1:nObs_hist_mono1) {
          logit(p_hist_mono1[i]) <- theta_hist_mono1[1] +
            exp(theta_hist_mono1[2]) *
              log(x_hist_mono1[i] / ref1)
          y_hist_mono1[i] ~ dbern(p_hist_mono1[i])
        }
        theta_hist_mono1[1:5] ~ dmnorm(mu[1:5], prec[1:5, 1:5])
      }
    )
  }

  model_file <- crmPack:::h_jags_write_model(model_fun)

  jags_data <- list(
    nObs_mono1 = mono1@nObs,
    y_mono1 = as.integer(mono1@y),
    x_mono1 = as.numeric(mono1@x),
    nObs_mono2 = mono2@nObs,
    y_mono2 = as.integer(mono2@y),
    x_mono2 = as.numeric(mono2@x),
    nObs_combo = combo@nObs,
    y_combo = as.integer(combo@y),
    x_combo = combo@x,
    ref1 = ref1,
    ref2 = ref2,
    kappa = kappa
  )

  if (has_hist) {
    jags_data$nObs_hist_mono1 <- hist_mono1@nObs
    jags_data$y_hist_mono1 <- as.integer(hist_mono1@y)
    jags_data$x_hist_mono1 <- as.numeric(hist_mono1@x)
  } else {
    jags_data$nObs_hist_mono1 <- 0L
    jags_data$y_hist_mono1 <- integer()
    jags_data$x_hist_mono1 <- numeric()
  }

  jags_model <- rjags::jags.model(
    file = model_file,
    data = jags_data,
    quiet = TRUE,
    n.adapt = 0
  )

  update(jags_model, n.iter = options@burnin, progress.bar = "none")

  sample_vars <- c(
    "theta_mono1",
    "theta_mono2",
    "theta_combo",
    "mu",
    "tau_alpha1",
    "tau_beta1",
    "tau_alpha2",
    "tau_beta2",
    "tau_eta",
    "rho1",
    "rho2"
  )
  if (has_hist) {
    sample_vars <- c(sample_vars, "theta_hist_mono1")
  }

  samples_coda <- rjags::coda.samples(
    model = jags_model,
    variable.names = sample_vars,
    n.iter = options@iterations - options@burnin,
    thin = options@step,
    progress.bar = "none"
  )

  smat <- as.matrix(samples_coda)
  as_col <- function(name) as.numeric(smat[, name])
  as_vec5 <- function(base) {
    cbind(
      as_col(paste0(base, "[1]")),
      as_col(paste0(base, "[2]")),
      as_col(paste0(base, "[3]")),
      as_col(paste0(base, "[4]")),
      as_col(paste0(base, "[5]"))
    )
  }

  out <- list(
    samples = crmPack::Samples(
      data = list(
        theta_mono1 = as_vec5("theta_mono1"),
        theta_mono2 = as_vec5("theta_mono2"),
        theta_combo = as_vec5("theta_combo"),
        mu = as_vec5("mu"),
        tau_alpha1 = as_col("tau_alpha1"),
        tau_beta1 = as_col("tau_beta1"),
        tau_alpha2 = as_col("tau_alpha2"),
        tau_beta2 = as_col("tau_beta2"),
        tau_eta = as_col("tau_eta"),
        rho1 = as_col("rho1"),
        rho2 = as_col("rho2")
      ),
      options = options
    ),
    model_file = model_file
  )

  if (has_hist) {
    out$samples@data$theta_hist_mono1 <- as_vec5("theta_hist_mono1")
  }

  out
}

arm_samples_from_joint5d_prototype <- function(
  joint_fit,
  arm = c("mono1", "mono2", "combo", "hist_mono1")
) {
  arm <- match.arg(arm)
  src <- joint_fit$samples@data

  if (identical(arm, "mono1")) {
    th <- src$theta_mono1
    return(crmPack::Samples(
      data = list(alpha0 = th[, 1], alpha1 = exp(th[, 2])),
      options = joint_fit$samples@options
    ))
  }
  if (identical(arm, "mono2")) {
    th <- src$theta_mono2
    return(crmPack::Samples(
      data = list(alpha0 = th[, 3], alpha1 = exp(th[, 4])),
      options = joint_fit$samples@options
    ))
  }
  if (identical(arm, "combo")) {
    th <- src$theta_combo
    return(crmPack::Samples(
      data = list(
        alpha0 = cbind(th[, 1], th[, 3]),
        alpha1 = cbind(exp(th[, 2]), exp(th[, 4])),
        eta = th[, 5]
      ),
      options = joint_fit$samples@options
    ))
  }

  if (is.null(src$theta_hist_mono1)) {
    stop("No historical source in joint 5D fit.", call. = FALSE)
  }
  th <- src$theta_hist_mono1
  crmPack::Samples(
    data = list(alpha0 = th[, 1], alpha1 = exp(th[, 2])),
    options = joint_fit$samples@options
  )
}

run_hierarchical_joint5d_prior_prototype <- function(include_hist = TRUE) {
  mono1 <- build_mono_prototype()
  mono2 <- build_mono2_prototype()
  combo <- build_fixed_combo_prototype()

  arms <- list(mono1 = mono1$data, mono2 = mono2$data, combo = combo$data)
  if (isTRUE(include_hist)) {
    hist_mono1 <- build_historical_mono1_prototype()
    arms$hist_mono1 <- hist_mono1$data
  }

  h_data <- structure(list(arms = arms), class = "HierarchicalDataPrototype")
  h_options <- crmPack::McmcOptions(burnin = 300L, step = 1L, samples = 500L)
  joint_fit <- mcmc_hierarchical_joint5d_prior_prototype(h_data, h_options)

  mono1_s <- arm_samples_from_joint5d_prototype(joint_fit, "mono1")
  mono2_s <- arm_samples_from_joint5d_prototype(joint_fit, "mono2")
  combo_s <- arm_samples_from_joint5d_prototype(joint_fit, "combo")

  checks <- list(
    mean_prob_mono1_at_20 = mean(crmPack::prob(20, mono1$model, mono1_s)),
    mean_prob_mono2_at_40 = mean(crmPack::prob(40, mono2$model, mono2_s)),
    mean_prob_combo_at_20_40 = mean(crmPack::prob(
      c(drug1 = 20, drug2 = 40),
      combo$model,
      combo_s
    )),
    mean_rho1 = mean(joint_fit$samples@data$rho1),
    mean_rho2 = mean(joint_fit$samples@data$rho2)
  )

  if (isTRUE(include_hist)) {
    hist_s <- arm_samples_from_joint5d_prototype(joint_fit, "hist_mono1")
    checks$mean_prob_hist_mono1_at_20 <- mean(crmPack::prob(
      20,
      hist_mono1$model,
      hist_s
    ))
  }

  list(
    data = h_data,
    options = h_options,
    joint5d = joint_fit,
    checks = checks
  )
}

result6 <- run_hierarchical_joint5d_prior_prototype(include_hist = TRUE)
str(result6, 2)

## Sixth try: dynamic generation for full 5D joint source-level prior
## (no hardcoded source names).

# Helper function to create safe variable names for JAGS from arm names.
h_joint5d_safe_name <- function(x) {
  gsub("[^A-Za-z0-9_]", "_", x)
}

# Helper function to determine the source type for the joint 5D model based on arm type, data, and model.
h_joint5d_type <- function(arm_type, arm_data, arm_model) {
  if (
    methods::is(arm_model, "LogisticLogNormalCombo") &&
      methods::is(arm_data, "DataCombo")
  ) {
    return("combo")
  }
  if (
    methods::is(arm_model, "LogisticLogNormal") && methods::is(arm_data, "Data")
  ) {
    if (
      identical(arm_type, "mono2") || identical(arm_type, "historical_mono2")
    ) {
      return("mono2")
    }
    return("mono1")
  }
  stop(
    sprintf(
      "Unsupported source arm type for dynamic joint5d: arm_type=%s, data=%s, model=%s",
      arm_type,
      class(arm_data)[1],
      class(arm_model)[1]
    ),
    call. = FALSE
  )
}

# Main function to dynamically generate the JAGS model code for the hierarchical joint 5D prior.
h_compile_hierarchical_joint5d_dynamic <- function(data, model) {
  stopifnot(inherits(data, "HierarchicalDataPrototype"))
  stopifnot(inherits(model, "HierarchicalModelPrototype"))

  arm_models <- h_get_arm_models_prototype(model)
  arm_names <- names(data$arms)
  stopifnot(length(arm_names) > 0L)
  stopifnot(all(arm_names %in% names(arm_models)))
  stopifnot(all(arm_names %in% names(model$arm_map)))

  lines <- c("model {")
  prior_lines <- character()
  hyper_lines <- character(
    0L
  )
  jags_data <- list()
  sample_vars <- c(
    "mu",
    "tau_alpha1",
    "tau_beta1",
    "tau_alpha2",
    "tau_beta2",
    "tau_eta",
    "rho1",
    "rho2"
  )
  arm_param_map <- list()

  ref1 <- 10
  ref2 <- 20
  kappa <- log(2) / 1.96
  jags_data$kappa <- kappa

  for (arm in arm_names) {
    arm_data <- data$arms[[arm]]
    arm_model <- arm_models[[arm]]
    arm_type <- model$arm_map[[arm]]$type
    source_type <- h_joint5d_type(arm_type, arm_data, arm_model)
    src <- h_joint5d_safe_name(arm)
    theta_var <- paste0("theta_", src)

    if (identical(source_type, "combo")) {
      n_obs <- paste0("nObs_", src)
      y <- paste0("y_", src)
      x <- paste0("x_", src)
      ref <- paste0("ref_dose_", src)
      a0 <- paste0("alpha0_", src)
      a1 <- paste0("alpha1_", src)
      e <- paste0("eta_", src)

      jags_data[[n_obs]] <- arm_data@nObs
      jags_data[[y]] <- as.integer(arm_data@y)
      jags_data[[x]] <- arm_data@x
      jags_data[[ref]] <- c(ref1, ref2)

      prior_lines <- c(
        prior_lines,
        sprintf("%s[1:5] ~ dmnorm(mu[1:5], prec[1:5, 1:5])", theta_var),
        sprintf("%s[1] <- %s[1]", a0, theta_var),
        sprintf("%s[2] <- %s[3]", a0, theta_var),
        sprintf("%s[1] <- exp(%s[2])", a1, theta_var),
        sprintf("%s[2] <- exp(%s[4])", a1, theta_var),
        sprintf("%s <- %s[5]", e, theta_var)
      )

      lines <- c(
        lines,
        h_render_model_slot_lines(
          fun = arm_model@datamodel,
          replacements = c(
            nObs = n_obs,
            y = y,
            x = x,
            ref_dose = ref,
            alpha0 = a0,
            alpha1 = a1,
            eta = e,
            p_single = paste0("p_single_", src),
            p0 = paste0("p0_", src),
            p = paste0("p_", src)
          )
        )
      )

      sample_vars <- c(sample_vars, theta_var)
      arm_param_map[[arm]] <- list(type = "combo", theta = theta_var)
    } else {
      n_obs <- paste0("nObs_", src)
      y <- paste0("y_", src)
      x <- paste0("x_", src)
      ref <- paste0("ref_dose_", src)
      alpha0 <- paste0("alpha0_", src)
      alpha1 <- paste0("alpha1_", src)
      idx <- if (identical(source_type, "mono2")) c(3L, 4L) else c(1L, 2L)

      jags_data[[n_obs]] <- arm_data@nObs
      jags_data[[y]] <- as.integer(arm_data@y)
      jags_data[[x]] <- as.numeric(arm_data@x)
      jags_data[[ref]] <- if (identical(source_type, "mono2")) ref2 else ref1

      prior_lines <- c(
        prior_lines,
        sprintf("%s[1:5] ~ dmnorm(mu[1:5], prec[1:5, 1:5])", theta_var),
        sprintf("%s <- %s[%d]", alpha0, theta_var, idx[1]),
        sprintf("%s <- exp(%s[%d])", alpha1, theta_var, idx[2])
      )

      lines <- c(
        lines,
        h_render_model_slot_lines(
          fun = arm_model@datamodel,
          replacements = c(
            nObs = n_obs,
            y = y,
            x = x,
            ref_dose = ref,
            alpha0 = alpha0,
            alpha1 = alpha1,
            p = paste0("p_", src)
          )
        )
      )

      sample_vars <- c(sample_vars, theta_var)
      arm_param_map[[arm]] <- list(type = source_type, theta = theta_var)
    }
  }

  hyper_lines <- c(
    hyper_lines,
    "mu[1] ~ dnorm(logit(0.25), pow(2.5, -2))",
    "mu[2] ~ dnorm(0, pow(0.7, -2))",
    "mu[3] ~ dnorm(logit(0.25), pow(2.5, -2))",
    "mu[4] ~ dnorm(0, 1)",
    "mu[5] ~ dnorm(0, 1)",
    "tau_alpha1 ~ dlnorm(log(0.5), pow(kappa, -2))",
    "tau_beta1 ~ dlnorm(log(0.25), pow(kappa, -2))",
    "tau_alpha2 ~ dlnorm(log(0.75), pow(kappa, -2))",
    "tau_beta2 ~ dlnorm(log(0.25), pow(kappa, -2))",
    "tau_eta ~ dlnorm(log(0.125), pow(kappa, -2))",
    "rho1 ~ dunif(-0.95, 0.95)",
    "rho2 ~ dunif(-0.95, 0.95)",
    "cov[1, 1] <- pow(tau_alpha1, 2)",
    "cov[2, 2] <- pow(tau_beta1, 2)",
    "cov[1, 2] <- rho1 * tau_alpha1 * tau_beta1",
    "cov[2, 1] <- cov[1, 2]",
    "cov[3, 3] <- pow(tau_alpha2, 2)",
    "cov[4, 4] <- pow(tau_beta2, 2)",
    "cov[3, 4] <- rho2 * tau_alpha2 * tau_beta2",
    "cov[4, 3] <- cov[3, 4]",
    "cov[5, 5] <- pow(tau_eta, 2)",
    "cov[1, 3] <- 0",
    "cov[1, 4] <- 0",
    "cov[1, 5] <- 0",
    "cov[2, 3] <- 0",
    "cov[2, 4] <- 0",
    "cov[2, 5] <- 0",
    "cov[3, 1] <- 0",
    "cov[3, 2] <- 0",
    "cov[3, 5] <- 0",
    "cov[4, 1] <- 0",
    "cov[4, 2] <- 0",
    "cov[4, 5] <- 0",
    "cov[5, 1] <- 0",
    "cov[5, 2] <- 0",
    "cov[5, 3] <- 0",
    "cov[5, 4] <- 0",
    "prec[1:5, 1:5] <- inverse(cov[1:5, 1:5])"
  )

  all_lines <- c(lines, prior_lines, hyper_lines, "}")
  model_file <- tempfile(
    pattern = "hierarchical_joint5d_dynamic_",
    fileext = ".jags"
  )
  writeLines(all_lines, con = model_file)

  list(
    model_file = model_file,
    jags_data = jags_data,
    sample_vars = unique(sample_vars),
    arm_param_map = arm_param_map,
    jags_lines = all_lines
  )
}

# Main function to run MCMC for the hierarchical joint 5D model using the dynamically generated JAGS code.
mcmc_hierarchical_joint5d_dynamic_prototype <- function(data, model, options) {
  stopifnot(methods::is(options, "McmcOptions"))

  compiled <- h_compile_hierarchical_joint5d_dynamic(data = data, model = model)

  jags_model <- rjags::jags.model(
    file = compiled$model_file,
    data = compiled$jags_data,
    quiet = TRUE,
    n.adapt = 0
  )
  update(jags_model, n.iter = options@burnin, progress.bar = "none")

  samples_coda <- rjags::coda.samples(
    model = jags_model,
    variable.names = compiled$sample_vars,
    n.iter = options@iterations - options@burnin,
    thin = options@step,
    progress.bar = "none"
  )

  smat <- as.matrix(samples_coda)
  as_col <- function(name) as.numeric(smat[, name])
  as_vec5 <- function(base) {
    cbind(
      as_col(paste0(base, "[1]")),
      as_col(paste0(base, "[2]")),
      as_col(paste0(base, "[3]")),
      as_col(paste0(base, "[4]")),
      as_col(paste0(base, "[5]"))
    )
  }

  sample_data <- list(
    mu = as_vec5("mu"),
    tau_alpha1 = as_col("tau_alpha1"),
    tau_beta1 = as_col("tau_beta1"),
    tau_alpha2 = as_col("tau_alpha2"),
    tau_beta2 = as_col("tau_beta2"),
    tau_eta = as_col("tau_eta"),
    rho1 = as_col("rho1"),
    rho2 = as_col("rho2")
  )

  theta_vars <- names(compiled$arm_param_map)
  for (arm in theta_vars) {
    theta_name <- compiled$arm_param_map[[arm]]$theta
    sample_data[[theta_name]] <- as_vec5(theta_name)
  }

  list(
    samples = crmPack::Samples(data = sample_data, options = options),
    arm_param_map = compiled$arm_param_map,
    jags_model_file = compiled$model_file,
    jags_code = compiled$jags_lines
  )
}

# Helper function to extract arm-specific samples from the joint 5D fit based on the arm parameter map.
arm_samples_from_joint5d_dynamic_prototype <- function(joint_fit, arm) {
  stopifnot(
    is.list(joint_fit),
    !is.null(joint_fit$samples),
    !is.null(joint_fit$arm_param_map)
  )
  stopifnot(arm %in% names(joint_fit$arm_param_map))

  map <- joint_fit$arm_param_map[[arm]]
  th <- joint_fit$samples@data[[map$theta]]

  if (identical(map$type, "combo")) {
    return(crmPack::Samples(
      data = list(
        alpha0 = cbind(th[, 1], th[, 3]),
        alpha1 = cbind(exp(th[, 2]), exp(th[, 4])),
        eta = th[, 5]
      ),
      options = joint_fit$samples@options
    ))
  }

  if (identical(map$type, "mono2")) {
    return(crmPack::Samples(
      data = list(alpha0 = th[, 3], alpha1 = exp(th[, 4])),
      options = joint_fit$samples@options
    ))
  }

  crmPack::Samples(
    data = list(alpha0 = th[, 1], alpha1 = exp(th[, 2])),
    options = joint_fit$samples@options
  )
}

# Main function to run the hierarchical joint 5D model with dynamic JAGS code generation
# and extract samples for checks.
run_hierarchical_joint5d_dynamic_prototype <- function(include_hist = TRUE) {
  mono1 <- build_mono_prototype()
  mono2 <- build_mono2_prototype()
  combo <- build_fixed_combo_prototype()

  arms <- list(mono1 = mono1$data, mono2 = mono2$data, combo = combo$data)
  arm_models <- list(
    mono1 = mono1$model,
    mono2 = mono2$model,
    combo = combo$model
  )
  arm_map <- list(
    mono1 = list(type = "mono1", active = TRUE, use_for_recommendation = TRUE),
    mono2 = list(type = "mono2", active = TRUE, use_for_recommendation = TRUE),
    combo = list(type = "combo", active = TRUE, use_for_recommendation = TRUE)
  )

  if (isTRUE(include_hist)) {
    hist_mono1 <- build_historical_mono1_prototype()
    arms$hist_mono1 <- hist_mono1$data
    arm_models$hist_mono1 <- hist_mono1$model
    arm_map$hist_mono1 <- list(
      type = "historical_mono1",
      active = FALSE,
      use_for_recommendation = FALSE
    )
  }

  h_data <- structure(list(arms = arms), class = "HierarchicalDataPrototype")
  h_model <- structure(
    list(
      arm_map = arm_map,
      arm_models = arm_models,
      mono_model = mono1$model,
      combo_model = combo$model
    ),
    class = "HierarchicalModelPrototype"
  )

  h_options <- crmPack::McmcOptions(burnin = 300L, step = 1L, samples = 500L)
  joint_fit <- mcmc_hierarchical_joint5d_dynamic_prototype(
    h_data,
    h_model,
    h_options
  )
  cat(joint_fit$jags_code, sep = "\n")

  mono1_s <- arm_samples_from_joint5d_dynamic_prototype(joint_fit, "mono1")
  mono2_s <- arm_samples_from_joint5d_dynamic_prototype(joint_fit, "mono2")
  combo_s <- arm_samples_from_joint5d_dynamic_prototype(joint_fit, "combo")

  checks <- list(
    mean_prob_mono1_at_20 = mean(crmPack::prob(20, mono1$model, mono1_s)),
    mean_prob_mono2_at_40 = mean(crmPack::prob(40, mono2$model, mono2_s)),
    mean_prob_combo_at_20_40 = mean(crmPack::prob(
      c(drug1 = 20, drug2 = 40),
      combo$model,
      combo_s
    )),
    mean_rho1 = mean(joint_fit$samples@data$rho1),
    mean_rho2 = mean(joint_fit$samples@data$rho2)
  )

  if (isTRUE(include_hist)) {
    hist_s <- arm_samples_from_joint5d_dynamic_prototype(
      joint_fit,
      "hist_mono1"
    )
    checks$mean_prob_hist_mono1_at_20 <- mean(crmPack::prob(
      20,
      hist_mono1$model,
      hist_s
    ))
  }

  list(
    data = h_data,
    model = h_model,
    options = h_options,
    joint5d_dynamic = joint_fit,
    checks = checks
  )
}

result7 <- run_hierarchical_joint5d_dynamic_prototype(include_hist = TRUE)
str(result7, 2)

# Let's compare with the model without using historical data
# to see if we get different results.
result8 <- run_hierarchical_joint5d_dynamic_prototype(include_hist = FALSE)
str(result8, 2)

result7$checks
result8$checks

# nolint end
