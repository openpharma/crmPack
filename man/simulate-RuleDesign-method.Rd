\docType{methods}
\name{simulate,RuleDesign-method}
\alias{simulate,RuleDesign-method}
\title{Simulate outcomes from a rule-based design}
\usage{
\S4method{simulate}{RuleDesign}(object, truth, args = NULL, nsim = 1L,
  seed = NULL, parallel = FALSE, ...)
}
\arguments{
  \item{object}{the \code{\linkS4class{RuleDesign}} object
  we want to simulate data from}

  \item{truth}{a function which takes as input a dose
  (vector) and returns the true probability (vector) for
  toxicity. Additional arguments can be supplied in
  \code{args}.}

  \item{args}{data frame with arguments for the
  \code{truth} function. The column names correspond to the
  argument names, the rows to the values of the arguments.
  The rows are appropriately recycled in the \code{nsim}
  simulations.}

  \item{nsim}{the number of simulations (default: 1)}

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
an object of class \code{\linkS4class{GeneralSimulations}}
}
\description{
Simulate outcomes from a rule-based design
}
\keyword{methods}

