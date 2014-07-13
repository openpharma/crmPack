\docType{methods}
\name{summary,Simulations-method}
\alias{summary,Simulations-method}
\title{Summarize the simulations, relative to a given truth}
\usage{
\S4method{summary}{Simulations}(object, truth, target = c(0.2, 0.35), ...)
}
\arguments{
  \item{object}{the \code{\linkS4class{Simulations}} object
  we want to summarize}

  \item{truth}{a function which takes as input a dose
  (vector) and returns the true probability (vector) for
  toxicity. Additional arguments can be supplied via
  \dots.}

  \item{target}{the target toxicity interval (default:
  20-35\%) used for the computations}

  \item{...}{additional arguments affecting the summary
  produced.}
}
\value{
an object of class \code{\linkS4class{Simulations-summary}}
}
\description{
Summarize the simulations, relative to a given truth
}
\keyword{methods}

