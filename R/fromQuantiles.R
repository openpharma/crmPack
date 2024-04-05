##' @include helpers.R
##' @include Model-class.R
NULL

#' Helper to convert prior quantiles (lower, median, upper) to logistic (log)
#' normal model
#'
#' This function uses generalized simulated annealing to optimize
#' a [`LogisticNormal`] model to be as close as possible
#' to the given prior quantiles.
#'
#' @param dose_grid (`numeric`)\cr the dose grid to be used (sorted).
#' @param ref_dose (`number`)\cr the reference dose \eqn{x*} (strictly positive
#'   number).
#' @param lower (`numeric`)\cr the lower quantiles.
#' @param median (`numeric`)\cr the medians.
#' @param upper (`numeric`)\cr the upper quantiles.
#' @param level (`number`)\cr the credible level of the (lower, upper) intervals
#'  (default: 0.95).
#' @param log_normal (`flag`)\cr if FALSE the normal prior for the logistic
#' regression coefficients is used (default). If TRUE the log-normal prior
#' is used.
#' @param par_start (`numeric`)\cr starting values for the parameters. By
#'  default, these are determined from the medians supplied.
#' @param par_lower (`numeric`)\cr lower bounds on the parameters (intercept
#' alpha, slope beta, the corresponding standard deviations
#' and the correlation).
#' @param par_upper (`numeric`)\cr upper bounds on the parameters (intercept
#' alpha, slope beta, the corresponding standard deviations
#' and the correlation).
#' @param seed (`number`)\cr seed for random number generation.
#' @param verbose (`flag`)\cr if TRUE verbose output (default).
#' @param control (`list`)\cr additional options for the optimization routine,
#' see [`GenSA`][GenSA] for more details.
#' @return a list with the best approximating `model` [`LogisticNormal`] or
#' [`LogisticLogNormal`], the resulting `quantiles`, the required `quantiles`
#' and the `distance` to the required quantiles,
#' as well as the final `parameters` (which could be used for running the
#' algorithm a second time).
#'
#' @importFrom GenSA GenSA
#' @importFrom mvtnorm rmvnorm
#'
#' @keywords internal
h_quantiles_2_logistic_normal <- function(dose_grid,
                                          ref_dose,
                                          lower,
                                          median,
                                          upper,
                                          level = 0.95,
                                          log_normal = FALSE,
                                          par_start = NULL,
                                          par_lower = c(-10, -10, 0, 0, -0.95),
                                          par_upper = c(10, 10, 10, 10, 0.95),
                                          seed = 12345,
                                          verbose = TRUE,
                                          control =
                                            list(
                                              threshold.stop = 0.01,
                                              maxit = 50000,
                                              temperature = 50000,
                                              max.time = 600
                                            )) {
  # extracts and checks
  assert_numeric(dosegrid, unique = TRUE, sorted = TRUE)
  n_doses <- length(dose_grid)
  # how to check that it is geater 0?
  assert_number(ref_dose, lower = 0)
  assert_numeric(lower, len = n_doses)
  assert_numeric(median, len = n_doses, sorted = TRUE)
  assert_numeric(upper, len = n_doses)
  assert(all(lower < median))
  assert(all(upper > median))
  assert_probability(level, bounds_closed = FALSE)
  assert_flag(log_normal)
  assert_numeric(par_start, len = 5L)
  assert_numeric(par_lower, len = 5L)
  assert_numeric(par_upper, len = 5L)
  assert(all(par_lower < par_start))
  assert(all(par_start < par_upper))
  assert_flag(verbose)
  assert_list(control)

  # put verbose argument in the control list
  control$verbose <- verbose

  ## parametrize in terms of the means for the intercept alpha and the
  ## (log) slope beta,
  ## the corresponding standard deviations and the correlation.
  ## Define start values for optimisation:
  start_values <-
    if (is.null(par_start)) {
      # Find approximate means for alpha and slope beta
      # from fitting logistic model to medians.
      start_alpha_beta <-
        coef(lm(I(logit(median)) ~ I(log(dose_grid / ref_dose))))

      # Overall starting values.
      c(
        mean_alpha =
          start_alpha_beta[1],
        mean_beta =
          ifelse(log_normal, log(start_alpha_beta[2]), startAlphaBeta[2]),
        sd_alpha =
          1,
        sd_beta =
          1,
        correlation =
          0
      )
    } else {
      par_start
    }

  # Target function to be minimized.
  target <- function(param) {
    # Form the mean vector and covariance matrix.
    mean <- param[1:2]
    cov <- matrix(
      c(
        param[3]^2,
        prod(param[3:5]),
        prod(param[3:5]),
        param[4]^2
      ),
      nrow = 2L, ncol = 2L
    )

    # Simulate from the corresponding normal distribution.
    set.seed(seed)
    normal_samples <- mvtnorm::rmvnorm(
      n = 1e4L,
      mean = mean,
      sigma = cov
    )

    # Extract separate coefficients.
    alpha_samples <- normal_samples[, 1L]
    beta_samples <- ifelse(log_normal, exp(normalSamples[, 2L]), normalSamples[, 2L])

    # Compute resulting quantiles.
    quants <- matrix(
      nrow = length(dose_grid),
      ncol = 3L
    )
    colnames(quants) <- c("lower", "median", "upper")

    # Process each dose after another.
    for (i in seq_along(dose_grid))
    {
      # Create samples of the probability.
      prob_samples <-
        plogis(alpha_samples + beta_samples * log(dose_grid[i] / ref_dose))

      # Compute lower, median and upper quantile.
      quants[i, ] <-
        quantile(prob_samples,
          probs = c((1 - level) / 2, 0.5, (1 + level) / 2)
        )
    }

    # Compute the target value.
    return(structure(
      max(abs(quants - c(lower, median, upper))),
      mean = mean,
      cov = cov,
      quantiles = quants
    ))
  }

  set.seed(seed)
  # Optimize the target.
  gen_sa_res <- GenSA::GenSA(
    par = start_values,
    fn = target,
    lower = par_lower,
    upper = par_upper,
    control = control
  )
  distance <- gen_sa_res$value
  pars <- gen_sa_res$par
  target_res <- target(pars)

  # Construct the model.
  model <-
    if (log_normal) {
      LogisticLogNormal(
        mean = attr(target_res, "mean"),
        cov = attr(target_res, "cov"),
        ref_dose = ref_dose
      )
    } else {
      LogisticNormal(
        mean = attr(target_res, "mean"),
        cov = attr(target_res, "cov"),
        ref_dose = ref_dose
      )
    }

  ## return it together with the resulting distance and the quantiles
  return(list(
    model = model,
    parameters = pars,
    quantiles = attr(target_res, "quantiles"),
    required = cbind(lower, median, upper),
    distance = distance
  ))
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

# nolint start

##' Construct a minimally informative prior
##'
##' This function constructs a minimally informative prior, which is captured in
##' a \code{\linkS4class{LogisticNormal}} (or
##' \code{\linkS4class{LogisticLogNormal}}) object.
##'
##' Based on the proposal by Neuenschwander et al (2008, Statistics in
##' Medicine), a minimally informative prior distribution is constructed. The
##' required key input is the minimum (\eqn{d_{1}} in the notation of the
##' Appendix A.1 of that paper) and the maximum value (\eqn{d_{J}}) of the dose
##' grid supplied to this function. Then \code{threshmin} is the probability
##' threshold \eqn{q_{1}}, such that any probability of DLT larger than
##' \eqn{q_{1}} has only 5% probability. Therefore \eqn{q_{1}} is the 95%
##' quantile of the beta distribution and hence \eqn{p_{1} = 0.95}. Likewise,
##' \code{threshmax} is the probability threshold \eqn{q_{J}}, such that any
##' probability of DLT smaller than \eqn{q_{J}} has only 5% probability
##' (\eqn{p_{J} = 0.05}). The probabilities \eqn{1 - p_{1}} and \eqn{p_{J}} can be
##' controlled with the arguments \code{probmin} and \code{probmax}, respectively.
##' Subsequently, for all doses supplied in the
##' \code{dosegrid} argument, beta distributions are set up from the assumption
##' that the prior medians are linear in log-dose on the logit scale, and
##' \code{\link{Quantiles2LogisticNormal}} is used to transform the resulting
##' quantiles into an approximating \code{\linkS4class{LogisticNormal}} (or
##' \code{\linkS4class{LogisticLogNormal}}) model. Note that the reference dose
##' is not required for these computations.
##'
##' @param dosegrid the dose grid
##' @param refDose the reference dose
##' @param threshmin Any toxicity probability above this threshold would
##' be very unlikely (see \code{probmin}) at the minimum dose (default: 0.2)
##' @param threshmax Any toxicity probability below this threshold would
##' be very unlikely (see \code{probmax}) at the maximum dose (default: 0.3)
##' @param probmin the prior probability of exceeding \code{threshmin} at the
##' minimum dose (default: 0.05)
##' @param probmax the prior probability of being below \code{threshmax} at the
##' maximum dose (default: 0.05)
##' @param \dots additional arguments for computations, see
##' \code{\link{Quantiles2LogisticNormal}}, e.g. \code{refDose} and
##' \code{logNormal=TRUE} to obtain a minimal informative log normal prior.
##' @return see \code{\link{Quantiles2LogisticNormal}}
##'
##' @example examples/MinimalInformative.R
##' @export
##' @keywords programming
MinimalInformative <- function(dosegrid,
                               refDose,
                               threshmin = 0.2,
                               threshmax = 0.3,
                               probmin = 0.05,
                               probmax = 0.05,
                               ...) {
  ## extracts and checks
  nDoses <- length(dosegrid)

  assert_probability(threshmin, bounds_closed = FALSE)
  assert_probability(threshmax, bounds_closed = FALSE)
  assert_probability(probmin, bounds_closed = FALSE)
  assert_probability(probmax, bounds_closed = FALSE)
  stopifnot(
    !is.unsorted(dosegrid, strictly = TRUE)
  )
  xmin <- dosegrid[1]
  xmax <- dosegrid[nDoses]

  ## derive the beta distributions at the lowest and highest dose
  betaAtMin <- h_get_min_inf_beta(
    q = threshmin,
    p = 1 - probmin
  )
  betaAtMax <- h_get_min_inf_beta(
    q = threshmax,
    p = probmax
  )

  ## get the medians of those beta distributions
  medianMin <- with(
    betaAtMin,
    qbeta(p = 0.5, a, b)
  )
  medianMax <- with(
    betaAtMax,
    qbeta(p = 0.5, a, b)
  )

  ## now determine the medians of all beta distributions
  beta <- (logit(medianMax) - logit(medianMin)) / (log(xmax) - log(xmin))
  alpha <- logit(medianMax) - beta * log(xmax / refDose)
  medianDosegrid <- plogis(alpha + beta * log(dosegrid / refDose))

  ## finally for all doses calculate 95% credible interval bounds
  ## (lower and upper)
  lower <- upper <- dosegrid
  for (i in seq_along(dosegrid))
  {
    ## get min inf beta distribution
    thisMinBeta <- h_get_min_inf_beta(
      p = 0.5,
      q = medianDosegrid[i]
    )

    ## derive required quantiles
    lower[i] <- with(
      thisMinBeta,
      qbeta(p = 0.025, a, b)
    )
    upper[i] <- with(
      thisMinBeta,
      qbeta(p = 0.975, a, b)
    )
  }

  ## now go to Quantiles2LogisticNormal
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
