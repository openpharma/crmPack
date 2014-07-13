\docType{methods}
\name{simulate,Design-method}
\alias{simulate,Design-method}
\title{Simulate outcomes from a CRM design}
\usage{
\S4method{simulate}{Design}(object, truth, args = NULL,
  firstSeparate = FALSE, nsim = 1L, mcmcOptions = new("McmcOptions"),
  seed = NULL, parallel = FALSE, ...)
}
\arguments{
  \item{object}{the \code{\linkS4class{Design}} object we
  want to simulate data from}

  \item{truth}{a function which takes as input a dose
  (vector) and returns the true probability (vector) for
  toxicity. Additional arguments can be supplied in
  \code{args}.}

  \item{args}{data frame with arguments for the
  \code{truth} function. The column names correspond to the
  argument names, the rows to the values of the arguments.
  The rows are appropriately recycled in the \code{nsim}
  simulations. In order to produce outcomes from the
  posterior predictive distribution, e.g, pass an
  \code{object} that contains the data observed so far,
  \code{truth} contains the \code{prob} function from the
  model in \code{object}, and \code{args} contains
  posterior samples from the model.}

  \item{firstSeparate}{enroll the first patient separately
  from the rest of the cohort? (not default) If yes, the
  cohort will be closed if a DLT occurs in this patient.}

  \item{nsim}{the number of simulations (default: 1)}

  \item{mcmcOptions}{object of class
  \code{\linkS4class{McmcOptions}}, giving the MCMC options
  for each evaluation in the trial. By default, the
  standard options are used}

  \item{seed}{an object specifying if and how the random
  number generator should be initialized (\dQuote{seeded}).
  Either \code{NULL} (default) or an integer that will be
  used in a call to \code{\link{set.seed}} before
  simulating the response vectors. If set, the value is
  saved as the \code{seed} slot of the returned object. The
  default, \code{NULL} will not change the random generator
  state, and \code{.Random.seed} will be saved.}

  \item{parallel}{should the simulation runs be
  parallelized across the clusters of the computer? (not
  default)}

  \item{...}{additional optional arguments.}
}
\value{
an object of class \code{\linkS4class{Simulations}}
}
\description{
Simulate outcomes from a CRM design
}
\keyword{methods}

