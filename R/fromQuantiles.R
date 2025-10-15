##' @include helpers.R
##' @include Model-class.R
NULL

# Helper functions ----

#' Get Starting Values for Quantiles Optimization
#'
#' @param parstart (`numeric` or `NULL`)\cr starting parameter values.
#' @param median (`numeric`)\cr median values.
#' @param dosegrid (`numeric`)\cr dose grid.
#' @param refDose (`number`)\cr reference dose.
#' @param logNormal (`flag`)\cr use log-normal prior?
#'
#' @return Numeric vector of starting values.
#' @keywords internal
h_get_quantiles_start_values <- function(
  parstart,
  median,
  dosegrid,
  refDose,
  logNormal
) {
  if (is.null(parstart)) {
    # Find approximate means for alpha and slope beta from fitting logistic model to medians.
    startAlphaBeta <- coef(lm(I(logit(median)) ~ I(log(dosegrid / refDose))))

    c(
      meanAlpha = startAlphaBeta[1],
      meanBeta = if (logNormal) log(startAlphaBeta[2]) else startAlphaBeta[2],
      sdAlpha = 1,
      sdBeta = 1,
      correlation = 0
    )
  } else {
    parstart
  }
}

#' Target Function for Quantiles Optimization
#'
#' @param dosegrid (`numeric`)\cr dose grid.
#' @param refDose (`number`)\cr reference dose.
#' @param lower (`numeric`)\cr lower quantiles.
#' @param median (`numeric`)\cr median quantiles.
#' @param upper (`numeric`)\cr upper quantiles.
#' @param level (`number`)\cr credible level.
#' @param logNormal (`flag`)\cr use log-normal prior?
#' @param seed (`count`)\cr random seed.
#'
#' @return Function that computes target value for optimization.
#' @keywords internal
h_quantiles_target_function <- function(
  dosegrid,
  refDose,
  lower,
  median,
  upper,
  level,
  logNormal,
  seed
) {
  function(param) {
    # Form the mean vector and covariance matrix
    mean <- param[1:2]
    cov <- matrix(
      c(
        param[3]^2,
        prod(param[3:5]),
        prod(param[3:5]),
        param[4]^2
      ),
      nrow = 2L,
      ncol = 2L
    )

    # Simulate from the corresponding normal distribution
    set.seed(seed)
    normalSamples <- mvtnorm::rmvnorm(
      n = 1e4L,
      mean = mean,
      sigma = cov
    )

    # Extract separate coefficients
    alphaSamples <- normalSamples[, 1L]
    betaSamples <- if (logNormal) {
      exp(normalSamples[, 2L])
    } else {
      normalSamples[, 2L]
    }

    # Compute resulting quantiles
    quants <- matrix(
      nrow = length(dosegrid),
      ncol = 3L
    )
    colnames(quants) <- c("lower", "median", "upper")

    # Process each dose
    for (i in seq_along(dosegrid)) {
      # Create samples of the probability
      probSamples <- plogis(
        alphaSamples + betaSamples * log(dosegrid[i] / refDose)
      )

      # Compute lower, median and upper quantile
      quants[i, ] <- quantile(
        probSamples,
        probs = c((1 - level) / 2, 0.5, (1 + level) / 2)
      )
    }

    # Compute the target value
    ret <- max(abs(quants - c(lower, median, upper)))
    structure(ret, mean = mean, cov = cov, quantiles = quants)
  }
}

# Quantiles2LogisticNormal ----

#' Convert Prior Quantiles to Logistic (Log) Normal Model
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This function uses generalized simulated annealing to optimize
#' a [`LogisticNormal`] model to be as close as possible
#' to the given prior quantiles.
#'
#' @param dosegrid (`numeric`)\cr the dose grid.
#' @param refDose (`number`)\cr the reference dose.
#' @param lower (`numeric`)\cr the lower quantiles.
#' @param median (`numeric`)\cr the medians.
#' @param upper (`numeric`)\cr the upper quantiles.
#' @param level (`number`)\cr the credible level of the (lower, upper) intervals.
#'   Default is 0.95.
#' @param logNormal (`flag`)\cr use the log-normal prior? If `FALSE` (default),
#'   the normal prior for the logistic regression coefficients is used.
#' @param parstart (`numeric` or `NULL`)\cr starting values for the parameters.
#'   By default, these are determined from the medians supplied.
#' @param parlower (`numeric`)\cr lower bounds on the parameters (intercept alpha
#'   and the slope beta, the corresponding standard deviations and the correlation).
#' @param parupper (`numeric`)\cr upper bounds on the parameters.
#' @param seed (`count`)\cr seed for random number generation.
#' @param verbose (`flag`)\cr should the function be verbose?
#' @param control (`list`)\cr additional options for the optimisation routine,
#'   see [GenSA::GenSA()] for more details.
#'
#' @return A list with the best approximating `model`
#'   ([`LogisticNormal`] or [`LogisticLogNormal`]), the resulting `quantiles`,
#'   the `required` quantiles and the `distance` to the required quantiles,
#'   as well as the final `parameters` (which could be used for running the
#'   algorithm a second time).
#'
#' @importFrom GenSA GenSA
#' @importFrom mvtnorm rmvnorm
#' @export
Quantiles2LogisticNormal <- function(
  dosegrid,
  refDose,
  lower,
  median,
  upper,
  level = 0.95,
  logNormal = FALSE,
  parstart = NULL,
  parlower = c(-10, -10, 0, 0, -0.95),
  parupper = c(10, 10, 10, 10, 0.95),
  seed = 12345,
  verbose = TRUE,
  control = list(
    threshold.stop = 0.01,
    maxit = 50000,
    temperature = 50000,
    max.time = 600
  )
) {
  # Argument validation
  assert_numeric(
    dosegrid,
    min.len = 1,
    any.missing = FALSE,
    sorted = TRUE,
    unique = TRUE
  )
  assert_number(refDose, finite = TRUE)
  assert_numeric(lower, len = length(dosegrid), any.missing = FALSE)
  assert_numeric(
    median,
    len = length(dosegrid),
    any.missing = FALSE,
    sorted = TRUE
  )
  assert_numeric(upper, len = length(dosegrid), any.missing = FALSE)
  assert_probability(level, bounds_closed = FALSE)
  assert_flag(logNormal)
  assert_numeric(parstart, len = 5, null.ok = TRUE)
  assert_numeric(parlower, len = 5, any.missing = FALSE)
  assert_numeric(parupper, len = 5, any.missing = FALSE)
  assert_count(seed, positive = TRUE)
  assert_flag(verbose)
  assert_list(control)

  # Additional validation
  assert_true(all(lower < median))
  assert_true(all(median < upper))
  assert_true(all(parlower < parupper))
  if (!is.null(parstart)) {
    assert_true(all(parlower < parstart))
    assert_true(all(parstart < parupper))
  }

  nDoses <- length(dosegrid)
  control$verbose <- verbose

  startValues <- h_get_quantiles_start_values(
    parstart = parstart,
    median = median,
    dosegrid = dosegrid,
    refDose = refDose,
    logNormal = logNormal
  )

  target <- h_quantiles_target_function(
    dosegrid = dosegrid,
    refDose = refDose,
    lower = lower,
    median = median,
    upper = upper,
    level = level,
    logNormal = logNormal,
    seed = seed
  )

  set.seed(seed)
  # Optimize the target function
  genSAres <- GenSA::GenSA(
    par = startValues,
    fn = target,
    lower = parlower,
    upper = parupper,
    control = control
  )
  distance <- genSAres$value
  pars <- genSAres$par
  targetRes <- target(pars)

  # Construct the model
  model <- if (logNormal) {
    LogisticLogNormal(
      mean = attr(targetRes, "mean"),
      cov = attr(targetRes, "cov"),
      ref_dose = refDose
    )
  } else {
    LogisticNormal(
      mean = attr(targetRes, "mean"),
      cov = attr(targetRes, "cov"),
      ref_dose = refDose
    )
  }

  list(
    model = model,
    parameters = pars,
    quantiles = attr(targetRes, "quantiles"),
    required = cbind(lower, median, upper),
    distance = distance
  )
}

#' Helper for Minimal Informative Unimodal Beta Distribution
#'
#' As defined in Neuenschwander et al (2008), this function computes the
#' parameters of the minimal informative unimodal beta distribution, given the
#' request that the p-quantile should be q, i.e. `X ~ Be(a, b)` with
#' `Pr(X <= q) = p`.
#'
#' @param p (`number`)\cr the probability.
#' @param q (`number`)\cr the quantile.
#' @return A list with the two resulting beta parameters `a` and `b`.
#'
#' @keywords internal
h_get_min_inf_beta <- function(p, q) {
  assert_probability(p, bounds_closed = FALSE)
  assert_probability(q, bounds_closed = FALSE)

  if (q > p) {
    list(
      a = log(p) / log(q),
      b = 1
    )
  } else {
    list(
      a = 1,
      b = log(1 - p) / log(1 - q)
    )
  }
}

# MinimalInformative ----

#' Construct a Minimally Informative Prior
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This function constructs a minimally informative prior, which is captured in
#' a [`LogisticNormal`] (or [`LogisticLogNormal`]) object.
#'
#' Based on the proposal by Neuenschwander et al (2008, Statistics in
#' Medicine), a minimally informative prior distribution is constructed. The
#' required key input is the minimum (\eqn{d_{1}} in the notation of the
#' Appendix A.1 of that paper) and the maximum value (\eqn{d_{J}}) of the dose
#' grid supplied to this function. Then `threshmin` is the probability
#' threshold \eqn{q_{1}}, such that any probability of DLT larger than
#' \eqn{q_{1}} has only 5% probability. Therefore \eqn{q_{1}} is the 95%
#' quantile of the beta distribution and hence \eqn{p_{1} = 0.95}. Likewise,
#' `threshmax` is the probability threshold \eqn{q_{J}}, such that any
#' probability of DLT smaller than \eqn{q_{J}} has only 5% probability
#' (\eqn{p_{J} = 0.05}). The probabilities \eqn{1 - p_{1}} and \eqn{p_{J}} can be
#' controlled with the arguments `probmin` and `probmax`, respectively.
#' Subsequently, for all doses supplied in the
#' `dosegrid` argument, beta distributions are set up from the assumption
#' that the prior medians are linear in log-dose on the logit scale, and
#' [Quantiles2LogisticNormal()] is used to transform the resulting
#' quantiles into an approximating [`LogisticNormal`] (or
#' [`LogisticLogNormal`]) model. Note that the reference dose
#' is not required for these computations.
#'
#' @param dosegrid (`numeric`)\cr the dose grid.
#' @param refDose (`number`)\cr the reference dose.
#' @param threshmin (`number`)\cr any toxicity probability above this threshold
#'   would be very unlikely (see `probmin`) at the minimum dose.
#' @param threshmax (`number`)\cr any toxicity probability below this threshold
#'   would be very unlikely (see `probmax`) at the maximum dose.
#' @param probmin (`number`)\cr the prior probability of exceeding `threshmin`
#'   at the minimum dose.
#' @param probmax (`number`)\cr the prior probability of being below `threshmax`
#'   at the maximum dose.
#' @param ... additional arguments for computations, see
#'   [Quantiles2LogisticNormal()], e.g. `refDose` and
#'   `logNormal=TRUE` to obtain a minimal informative log normal prior.
#'
#' @return See [Quantiles2LogisticNormal()].
#'
#' @example examples/MinimalInformative.R
#' @export
MinimalInformative <- function(
  dosegrid,
  refDose,
  threshmin = 0.2,
  threshmax = 0.3,
  probmin = 0.05,
  probmax = 0.05,
  ...
) {
  # Argument validation
  assert_numeric(
    dosegrid,
    min.len = 1,
    any.missing = FALSE,
    sorted = TRUE,
    unique = TRUE
  )
  assert_number(refDose, finite = TRUE)
  assert_probability(threshmin, bounds_closed = FALSE)
  assert_probability(threshmax, bounds_closed = FALSE)
  assert_probability(probmin, bounds_closed = FALSE)
  assert_probability(probmax, bounds_closed = FALSE)

  nDoses <- length(dosegrid)
  xmin <- dosegrid[1]
  xmax <- dosegrid[nDoses]

  # Derive the beta distributions at the lowest and highest dose
  betaAtMin <- h_get_min_inf_beta(
    q = threshmin,
    p = 1 - probmin
  )
  betaAtMax <- h_get_min_inf_beta(
    q = threshmax,
    p = probmax
  )

  # Get the medians of those beta distributions
  medianMin <- with(betaAtMin, qbeta(p = 0.5, a, b))
  medianMax <- with(betaAtMax, qbeta(p = 0.5, a, b))

  # Determine the medians of all beta distributions
  beta <- (logit(medianMax) - logit(medianMin)) / (log(xmax) - log(xmin))
  alpha <- logit(medianMax) - beta * log(xmax / refDose)
  medianDosegrid <- plogis(alpha + beta * log(dosegrid / refDose))

  # Calculate 95% credible interval bounds (lower and upper) for all doses
  lower <- upper <- dosegrid
  for (i in seq_along(dosegrid)) {
    # Get minimal informative beta distribution
    thisMinBeta <- h_get_min_inf_beta(
      p = 0.5,
      q = medianDosegrid[i]
    )

    # Derive required quantiles
    lower[i] <- with(thisMinBeta, qbeta(p = 0.025, a, b))
    upper[i] <- with(thisMinBeta, qbeta(p = 0.975, a, b))
  }

  # Transform quantiles to LogisticNormal model
  Quantiles2LogisticNormal(
    dosegrid = dosegrid,
    refDose = refDose,
    lower = lower,
    median = medianDosegrid,
    upper = upper,
    level = 0.95,
    ...
  )
}

# nolint end
