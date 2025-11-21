#' @include helpers.R
#' @include helpers_covr.R
#' @include logger.R
#' @include Samples-class.R
NULL

# mcmc ----

#' Obtaining Posterior Samples for all Model Parameters
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This is the function that actually runs the `JAGS` MCMC machinery to produce
#' posterior samples from all model parameters and required derived values.
#' It is a generic function, so that customized versions may be conveniently
#' defined for specific subclasses of [`GeneralData`], [`GeneralModel`], and
#' [`McmcOptions`] input.
#'
#' @note The type of Random Number Generator (RNG) and its initial seed used by
#'   `JAGS` are taken from the `options` argument. If no initial values are
#'   supplied (i.e RNG kind or seed slot in `options` has `NA`), then they will
#'   be generated automatically by `JAGS`.
#'
#' @param data (`GeneralData`)\cr an input data.
#' @param model (`GeneralModel`)\cr an input model.
#' @param options (`McmcOptions`)\cr MCMC options.
#' @param ... not used.
#'
#' @return The posterior samples, an object of class [`Samples`].
#' @export
#'
setGeneric(
  name = "mcmc",
  def = function(data, model, options, ...) {
    standardGeneric("mcmc")
  },
  valueClass = "Samples"
)

# mcmc-GeneralData ----

#' @describeIn mcmc Standard method which uses JAGS.
#'
#' @param from_prior (`flag`)\cr sample from the prior only? Default to `TRUE`
#'   when number of observations in `data` is `0`. For some models it might be
#'   necessary to specify it manually here though.
#'
#' @aliases mcmc-GeneralData
#' @example examples/mcmc.R
#'
setMethod(
  f = "mcmc",
  signature = signature(
    data = "GeneralData",
    model = "GeneralModel",
    options = "McmcOptions"
  ),
  def = function(data, model, options, from_prior = data@nObs == 0L, ...) {
    assert_flag(from_prior)

    model_fun <- if (from_prior) {
      model@priormodel
    } else {
      h_jags_join_models(model@datamodel, model@priormodel)
    }

    model_file <- h_jags_write_model(model_fun)
    model_inits <- h_jags_get_model_inits(model, data)
    model_data <- h_jags_get_data(model, data, from_prior)

    jags_model <- rjags::jags.model(
      file = model_file,
      data = model_data,
      inits = c(
        model_inits,
        .RNG.name = h_null_if_na(options@rng_kind),
        .RNG.seed = h_null_if_na(options@rng_seed)
      ),
      quiet = !is_logging_enabled(),
      n.adapt = 0 # No adaptation. Important for reproducibility.
    )
    update(jags_model, n.iter = options@burnin, progress.bar = "none")

    # This is necessary as some outputs are written directly from the JAGS
    # compiled code to the outstream.
    log_trace("Running rjags::jags.samples")
    if (is_logging_enabled()) {
      jags_samples <- rjags::jags.samples(
        model = jags_model,
        variable.names = model@sample,
        n.iter = (options@iterations - options@burnin),
        thin = options@step
      )
    } else {
      invisible(
        capture.output(
          jags_samples <- rjags::jags.samples(
            model = jags_model,
            variable.names = model@sample,
            n.iter = (options@iterations - options@burnin),
            thin = options@step,
            progress.bar = "none"
          )
        )
      )
    }
    log_trace("JAGS samples: ", jags_samples, capture = TRUE)
    samples <- lapply(jags_samples, h_jags_extract_samples)

    Samples(data = samples, options = options)
  }
)

# mcmc-GeneralData-DualEndpointRW ----

#' @describeIn mcmc Standard method which uses JAGS. For the
#'   [`DualEndpointRW`] model, it is required that there are at least two (in
#'   case of random walk prior of the first order on the biomarker level) or
#'   three doses in the grid.
#'
#' @param from_prior (`flag`)\cr sample from the prior only? Default to `TRUE`
#'   when number of observations in `data` is `0`. For some models it might be
#'   necessary to specify it manually here though.
#'
#' @aliases mcmc-GeneralData-DualEndpointRW
#' @example examples/mcmc-DualEndpointRW.R
#'
setMethod(
  f = "mcmc",
  signature = signature(
    data = "GeneralData",
    model = "DualEndpointRW",
    options = "McmcOptions"
  ),
  def = function(data, model, options, from_prior = data@nObs == 0L, ...) {
    if (model@rw1) {
      assert_true(data@nGrid >= 2)
    } else {
      assert_true(data@nGrid >= 3)
    }

    callNextMethod(
      data = data,
      model = model,
      options = options,
      from_prior = from_prior,
      ...
    )
  }
)

# mcmc-GeneralData-DualEndpointBeta ----

#' @describeIn mcmc Standard method which uses JAGS. For the
#'   [`DualEndpointBeta`] model, it is required that the value of `ref_dose_beta`
#'   slot is greater than the maximum dose in a grid. This requirement comes from
#'   definition of the beta function that is used to model dose-biomarker
#'   relationship in [`DualEndpointBeta`] model. The other requirement is that
#'   there must be at least one dose in the grid.
#'
#' @param from_prior (`flag`)\cr sample from the prior only? Default to `TRUE`
#'   when number of observations in `data` is `0`. For some models it might be
#'   necessary to specify it manually here though.
#'
#' @aliases mcmc-GeneralData-DualEndpointBeta
#' @example examples/mcmc-DualEndpointBeta.R
#'
setMethod(
  f = "mcmc",
  signature = signature(
    data = "GeneralData",
    model = "DualEndpointBeta",
    options = "McmcOptions"
  ),
  def = function(data, model, options, from_prior = data@nObs == 0L, ...) {
    assert_true(data@nGrid >= 1)
    assert_true(model@ref_dose_beta > data@doseGrid[data@nGrid])

    callNextMethod(
      data = data,
      model = model,
      options = options,
      from_prior = from_prior,
      ...
    )
  }
)

# mcmc-GeneralData-DualEndpointEmax ----

#' @describeIn mcmc Standard method which uses JAGS. For the
#'   [`DualEndpointEmax`] model, it is required that there is at least one dose
#'   in the grid.
#'
#' @param from_prior (`flag`)\cr sample from the prior only? Default to `TRUE`
#'   when number of observations in `data` is `0`. For some models it might be
#'   necessary to specify it manually here though.
#'
#' @aliases mcmc-GeneralData-DualEndpointEmax
#'
setMethod(
  f = "mcmc",
  signature = signature(
    data = "GeneralData",
    model = "DualEndpointEmax",
    options = "McmcOptions"
  ),
  def = function(data, model, options, from_prior = data@nObs == 0L, ...) {
    assert_true(data@nGrid >= 1)

    callNextMethod(
      data = data,
      model = model,
      options = options,
      from_prior = from_prior,
      ...
    )
  }
)

# mcmc-GeneralData-OneParLogNormalPrior ----

#' @describeIn mcmc Standard method which uses JAGS. For the
#'   [`OneParLogNormalPrior`] model, it is required that the length of
#'   skeleton prior probabilities vector should be equal to the length of the
#'   number of doses.
#'
#' @param from_prior (`flag`)\cr sample from the prior only? Default to `TRUE`
#'   when number of observations in `data` is `0`. For some models it might be
#'   necessary to specify it manually here though.
#'
#' @aliases mcmc-GeneralData-OneParLogNormalPrior
#'
setMethod(
  f = "mcmc",
  signature = signature(
    data = "GeneralData",
    model = "OneParLogNormalPrior",
    options = "McmcOptions"
  ),
  def = function(data, model, options, from_prior = data@nObs == 0L, ...) {
    if (!from_prior) {
      assert_true(length(model@skel_probs) == data@nGrid)
    }

    callNextMethod(
      data = data,
      model = model,
      options = options,
      from_prior = from_prior,
      ...
    )
  }
)

# mcmc-GeneralData-OneParExpPrior ----

#' @describeIn mcmc Standard method which uses JAGS. For the
#'   [`OneParExpPrior`] model, it is required that the length of
#'   skeleton prior probabilities vector should be equal to the length of the
#'   number of doses.
#'
#' @param from_prior (`flag`)\cr sample from the prior only? Default to `TRUE`
#'   when number of observations in `data` is `0`. For some models it might be
#'   necessary to specify it manually here though.
#'
#' @aliases mcmc-GeneralData-OneParExpPrior
#'
setMethod(
  f = "mcmc",
  signature = signature(
    data = "GeneralData",
    model = "OneParExpPrior",
    options = "McmcOptions"
  ),
  def = function(data, model, options, from_prior = data@nObs == 0L, ...) {
    if (!from_prior) {
      assert_true(length(model@skel_probs) == data@nGrid)
    }

    callNextMethod(
      data = data,
      model = model,
      options = options,
      from_prior = from_prior,
      ...
    )
  }
)

# mcmc-DataMixture ----

#' @describeIn mcmc Method for [`DataMixture`] with different `from_prior` default.
#'   Samples from the prior only when both the number of observations and the
#'   number of shared observations are zero.
#'
#' @aliases mcmc-DataMixture
#'
setMethod(
  f = "mcmc",
  signature = signature(
    data = "DataMixture",
    model = "GeneralModel",
    options = "McmcOptions"
  ),
  def = function(
    data,
    model,
    options,
    from_prior = data@nObs == 0L & data@nObsshare == 0L,
    ...
  ) {
    callNextMethod(data, model, options, from_prior = from_prior, ...)
  }
)

# myBayesLogit ----

#' MCMC Sampling for Bayesian Logistic Regression Model
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Replacement for `BayesLogit::logit`. Performs MCMC sampling for Bayesian
#' logistic regression using JAGS.
#'
#' @param y (`integer`)\cr 0/1 vector of responses.
#' @param X (`matrix`)\cr design matrix.
#' @param m0 (`numeric`)\cr prior mean vector.
#' @param P0 (`matrix`)\cr precision matrix.
#' @param options (`McmcOptions`)\cr MCMC options.
#'
#' @return The matrix of samples (samples x parameters).
#'
#' @keywords internal
myBayesLogit <- function(y, X, m0, P0, options) {
  # Assertions.
  p <- length(m0)
  n_obs <- length(y)
  assert_integerish(y, lower = 0, upper = 1, any.missing = FALSE, len = n_obs)
  assert_numeric(m0, any.missing = FALSE, len = p)
  assert_matrix(P0, mode = "numeric", any.missing = FALSE, nrows = p, ncols = p)
  assert_matrix(
    X,
    mode = "numeric",
    any.missing = FALSE,
    nrows = n_obs,
    ncols = p
  )
  assert_class(options, "McmcOptions")

  # Get or set the seed.
  r_seed <- try(get(".Random.seed", envir = .GlobalEnv), silent = TRUE)
  if (is(r_seed, "try-error")) {
    set.seed(floor(runif(n = 1, min = 0, max = 1e4)))
    r_seed <- get(".Random.seed", envir = .GlobalEnv)
  }
  # .Random.seed contains two leading integers where the second
  # gives the position in the following 624 long vector (see
  # ?set.seed). Take the current position and ensure positivity.
  r_seed <- abs(r_seed[-c(1:2)][r_seed[2]])

  # Build the JAGS model.
  bugs_model <- function() {
    for (i in 1:nObs) {
      y[i] ~ dbern(p[i])
      logit(p[i]) <- mu[i]
    }

    mu <- X[,] %*% beta

    ## the multivariate normal prior on the coefficients
    beta ~ dmnorm(priorMean[], priorPrec[,])
  }

  # Write the model file.
  model_file_name <- h_jags_write_model(bugs_model)

  jags_model <- rjags::jags.model(
    model_file_name,
    data = list(
      "X" = X,
      "y" = y,
      "nObs" = n_obs,
      priorMean = m0,
      priorPrec = P0
    ),
    quiet = TRUE,
    # Add the RNG seed to the inits list (use Mersenne Twister as per R default).
    inits = list(
      .RNG.name = "base::Mersenne-Twister",
      .RNG.seed = r_seed
    ),
    n.chains = 1,
    n.adapt = 0
  )

  # Burn in.
  update(jags_model, n.iter = options@burnin, progress.bar = "none")

  # Generate samples.
  # This is necessary because some outputs are written directly from the JAGS
  # compiled code to the outstream.
  samples <- NULL
  capture.output(
    samples <- rjags::jags.samples(
      model = jags_model,
      variable.names = "beta",
      n.iter = (options@iterations - options@burnin),
      thin = options@step,
      progress.bar = "none"
    )
  )

  t(samples$beta[,, 1L])
}


# mcmc-Data-LogisticIndepBeta ----

#' @describeIn mcmc Obtain posterior samples for the model parameters based on
#'   the pseudo [`LogisticIndepBeta`] DLE model. The joint prior and posterior
#'   probability density function of the intercept \eqn{\phi_1} (`phi1`) and the
#'   slope \eqn{\phi_2} (`phi2`) are given in
#'   \insertCite{WhiteheadWilliamson1998;textual}{crmPack}. However, since
#'   asymptotically, the joint posterior probability density will be bivariate
#'   normal, we use the bivariate normal distribution to generate posterior
#'   samples of the intercept and the slope parameters. For the prior samples of
#'   the intercept and the slope, a bivariate normal distribution with mean and
#'   the covariance matrix given in
#'   \insertCite{WhiteheadWilliamson1998;textual}{crmPack} is used.
#'
#' @aliases mcmc-Data-LogisticIndepBeta
#' @example examples/mcmc-LogisticIndepBeta.R
#' @references
#'   \insertAllCited{}
#'
setMethod(
  f = "mcmc",
  signature = signature(
    data = "Data",
    model = "LogisticIndepBeta",
    options = "McmcOptions"
  ),
  def = function(data, model, options, ...) {
    # Update the DLE model first.
    this_model <- update(object = model, data = data)

    # Decide whether we sample from the prior or not.
    from_prior <- data@nObs == 0L

    # Probabilities of risk of DLE at all dose levels.
    pi <- (this_model@binDLE) / (this_model@DLEweights)
    # Scalar term for the covariance matrix.
    scalar_i <- this_model@DLEweights * pi * (1 - pi)

    precision <- matrix(rep(0, 4), nrow = 2, ncol = 2)

    for (i in seq_len(length(this_model@binDLE))) {
      precision_mat <- scalar_i[i] *
        matrix(
          c(
            1,
            log(this_model@DLEdose[i]),
            log(this_model@DLEdose[i]),
            (log(this_model@DLEdose[i]))^2
          ),
          2,
          2
        )
      precision <- precision + precision_mat
    }

    if (from_prior) {
      # Sample from the (asymptotic) bivariate normal prior for theta.
      tmp <- mvtnorm::rmvnorm(
        n = size(options),
        mean = c(slot(this_model, "phi1"), slot(this_model, "phi2")),
        sigma = solve(precision)
      )

      samples <- list(
        phi1 = tmp[, 1],
        phi2 = tmp[, 2]
      )
    } else {
      weights <- rep(1, length(data@y))
      # Probabilities of risk of DLE at all dose levels.
      pi <- (data@y) / weights
      # Scalar term for the covariance matrix.
      scalar_i <- weights * pi * (1 - pi)

      prior_dle <- this_model@binDLE
      prior_w1 <- this_model@DLEweights
      prior_dose <- this_model@DLEdose

      fit_dle <- suppressWarnings(glm(
        prior_dle / prior_w1 ~ log(prior_dose),
        family = binomial(link = "logit"),
        weights = prior_w1
      ))
      s_fit_dle <- summary(fit_dle)

      # Obtain parameter estimates for dose-DLE curve.
      prior_phi1 <- coef(s_fit_dle)[1, 1]
      prior_phi2 <- coef(s_fit_dle)[2, 1]

      # Use fast special sampler here.
      # Set up design matrix.
      X <- cbind(1, log(data@x))
      init_res <- myBayesLogit(
        y = data@y,
        X = X,
        m0 = c(prior_phi1, prior_phi2),
        P0 = precision,
        options = options
      )

      # Form the samples list.
      samples <- list(
        phi1 = init_res[, 1],
        phi2 = init_res[, 2]
      )
    }

    # Form a Samples object for return.
    Samples(
      data = samples,
      options = options
    )
  }
)

# mcmc-DataDual-Effloglog ----

#' @describeIn mcmc Obtain the posterior samples for the model parameters in the
#'   [`Effloglog`] model. Given the value of \eqn{\nu}, the precision of the
#'   efficacy responses, the joint prior or the posterior probability of the
#'   intercept \eqn{\theta_1} (`theta1`) and the slope \eqn{\theta_2} (`theta2`)
#'   is a bivariate normal distribution. The \eqn{\nu} (`nu`), the precision of
#'   the efficacy responses is either a fixed value or has a gamma distribution.
#'   If a gamma distribution is used, the samples of `nu` will be first generated.
#'   Then the mean of the `nu` samples will be used to generate samples of the
#'   intercept and slope parameters of the model.
#'
#' @aliases mcmc-DataDual-Effloglog
#' @example examples/mcmc-Effloglog.R
#'
setMethod(
  f = "mcmc",
  signature = signature(
    data = "DataDual",
    model = "Effloglog",
    options = "McmcOptions"
  ),
  def = function(data, model, options, ...) {
    model <- update(object = model, data = data)
    sample_size <- size(options)

    if (model@use_fixed) {
      nu <- model@nu
      nu_samples <- rep(nu, sample_size)
    } else {
      nu_samples <- rgamma(
        sample_size,
        shape = model@nu["a"],
        rate = model@nu["b"]
      )
      nu <- mean(nu_samples)
    }

    # Sample from the (asymptotic) bivariate normal prior for theta1 and theta2.
    tmp <- mvtnorm::rmvnorm(
      n = sample_size,
      mean = model@mu,
      sigma = solve(nu * model@Q)
    )

    samples <- list(
      theta1 = tmp[, 1],
      theta2 = tmp[, 2],
      nu = nu_samples
    )

    Samples(
      data = samples,
      options = options
    )
  }
)

# mcmc-DataDual-EffFlexi ----

#' @describeIn mcmc Obtain the posterior samples for the estimates in the
#'   [`EffFlexi`] model. This is the MCMC procedure based on what is described
#'   in \insertCite{LangBrezger2004;textual}{crmPack} such that samples of the
#'   mean efficacy responses at all dose levels, samples of `sigma2`
#'   \eqn{\sigma^2}, the variance of the efficacy response and samples of
#'   `sigma2betaW` \eqn{\sigma^2_{\beta_W}}, the variance of the random walk
#'   model will be generated. Please refer to
#'   \insertCite{LangBrezger2004;textual}{crmPack} for the procedures and the
#'   form of the joint prior and posterior probability density for the mean
#'   efficacy responses. In addition, both `sigma2` and `sigma2betaW` can be
#'   fixed or have an inverse-gamma prior and posterior distribution. Therefore,
#'   if the inverse gamma distribution(s) are used, the parameters in the
#'   distribution will be first updated and then samples of `sigma2` and
#'   `sigma2betaW` will be generated using the updated parameters.
#'
#' @aliases mcmc-DataDual-EffFlexi
#' @example examples/mcmc-EffFlexi.R
#'
setMethod(
  f = "mcmc",
  signature = signature(
    data = "DataDual",
    model = "EffFlexi",
    options = "McmcOptions"
  ),
  def = function(data, model, options, ...) {
    # Update the model.
    this_model <- update(object = model, data = data)

    n_samples <- size(options)

    # Prepare samples container.
    samples <- list(
      ExpEff = matrix(ncol = data@nGrid, nrow = n_samples),
      sigma2W = matrix(nrow = n_samples),
      sigma2betaW = matrix(nrow = n_samples)
    )

    # Index of the next sample to be saved.
    iter_save <- 1L

    # Monitoring the Metropolis-Hastings update for sigma2.
    accept_history <- list(sigma2W = logical(options@iterations))

    # Current parameter values and starting values for the MCMC.
    if (length(data@w) == 0) {
      w1 <- this_model@eff
      x1 <- this_model@eff_dose
    } else {
      # Combine pseudo data with observed efficacy responses and no DLT observed.
      eff_obsrv <- getEff(data, no_dlt = TRUE)
      w1 <- c(this_model@eff, eff_obsrv$w_no_dlt)
      x1 <- c(this_model@eff_dose, eff_obsrv$x_no_dlt)
    }
    x1_level <- match_within_tolerance(x1, data@doseGrid)

    # betaW is constant, the average of the efficacy values.
    beta_w <- rep(mean(w1), data@nGrid)

    # sigma2betaW: use fixed value or prior mean.
    sigma2_beta_w <-
      if (this_model@use_fixed[["sigma2betaW"]]) {
        this_model@sigma2betaW
      } else {
        this_model@sigma2betaW["b"] / (this_model@sigma2betaW["a"] - 1)
      }

    # sigma2: fixed value or just the empirical variance.
    sigma2_w <- if (this_model@use_fixed[["sigma2W"]]) {
      this_model@sigma2W
    } else {
      var(w1)
    }

    # Set up diagonal matrix with the number of patients in corresponding dose levels.
    design_w_crossprod <- crossprod(this_model@X)

    # The MCMC cycle.
    for (iter_mcmc in seq_len(options@iterations)) {
      # 1) Generate coefficients for the Flexible Efficacy model.
      adjusted_var <- sigma2_w
      # New precision matrix.
      this_prec_w <- design_w_crossprod /
        adjusted_var +
        this_model@RW / sigma2_beta_w
      # Draw random normal vector.
      norm_vec <- rnorm(data@nGrid)
      # And its Cholesky factor.
      this_prec_w_chol <- chol(this_prec_w)
      # Solve betaW for L^T * betaW = normVec.
      beta_w <- backsolve(r = this_prec_w_chol, x = norm_vec)
      # The residual.
      adjusted_w <- w1 - this_model@X %*% beta_w

      # Forward substitution: solve L^T * tmp = designW^T * adjustedW / adjustedVar.
      tmp <- forwardsolve(
        l = this_prec_w_chol,
        x = crossprod(this_model@X, adjusted_w) / adjusted_var,
        upper.tri = TRUE,
        transpose = TRUE
      )
      # Backward substitution: solve R * result = tmp (where R is the Cholesky factor).
      tmp <- backsolve(
        r = this_prec_w_chol,
        x = tmp
      )

      # tmp is the mean vector of the distribution.
      # Add tmp to betaW to obtain final sample.
      beta_w <- beta_w + tmp

      # 2) Generate prior variance factor for the random walk.
      # If fixed, do nothing. Otherwise sample from full conditional.
      if (!this_model@use_fixed[["sigma2betaW"]]) {
        sigma2_beta_w <- rinvGamma(
          n = 1L,
          a = this_model@sigma2betaW["a"] + this_model@RW_rank / 2,
          b = this_model@sigma2betaW["b"] +
            crossprod(beta_w, this_model@RW %*% beta_w) / 2
        )
      }

      # 3) Generate variance for the flexible efficacy model.
      # If fixed variance is used, do nothing.
      if (this_model@use_fixed[["sigma2W"]]) {
        accept_history$sigma2W[iter_mcmc] <- TRUE
      } else {
        # Metropolis-Hastings update step here, using an inverse gamma distribution.
        a_star <- this_model@sigma2W["a"] + length(x1) / 2
        # Second parameter bStar depends on the value for sigma2W.
        b_star <- function(x) {
          adj_w <- w1
          sum((adj_w - beta_w[x1_level])^2) / 2 + this_model@sigma2W["b"]
        }
        # Draw proposal.
        b_star_proposal <- b_star(sigma2_w)
        sigma2_w <- rinvGamma(n = 1L, a = a_star, b = b_star_proposal)
      }

      # 4) Save samples.
      if (saveSample(options, iter_mcmc)) {
        samples$ExpEff[iter_save, ] <- beta_w
        samples$sigma2W[iter_save, 1] <- sigma2_w
        samples$sigma2betaW[iter_save, 1] <- sigma2_beta_w
        iter_save <- iter_save + 1L
      }
    }

    Samples(
      data = samples,
      options = options
    )
  }
)

# mcmc-DataOrdinal-LogisticLogNormalOrdinal ----

#' @describeIn mcmc Obtain the posterior samples for the model parameters in the
#'   [`LogisticLogNormalOrdinal`] model.
#'
#'   The generic `mcmc` method returns a [`Samples`] object with elements of the
#'   `data` slot named `alpha[1]`, `alpha[2]`, ..., `alpha[k]` and `beta` when
#'   passed a [`LogisticLogNormalOrdinal`] object. This makes the "alpha elements"
#'   awkward to access and is inconsistent with other [`Model`] objects. So rename
#'   the alpha elements to `alpha1`, `alpha2`, ..., `alpha<k>` for ease and
#'   consistency.
#'
#' @aliases mcmc-DataOrdinal-LogisticLogNormalOrdinal
#' @example examples/mcmc-LogisticLogNormalOrdinal.R
#'
setMethod(
  f = "mcmc",
  signature = signature(
    data = "DataOrdinal",
    model = "LogisticLogNormalOrdinal",
    options = "McmcOptions"
  ),
  def = function(data, model, options, ...) {
    # Obtain samples using the default method, but...
    return_value <- callNextMethod()
    # ...rename the alpha elements from alpha[<k>] to alpha<k>, where <k> is an
    # integer.
    names(return_value@data) <- gsub(
      "\\[(\\d+)\\]",
      "\\1",
      names(return_value@data)
    )
    return_value
  }
)
