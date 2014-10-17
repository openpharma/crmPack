\docType{methods}
\name{approximate,Samples-method}
\alias{approximate,Samples-method}
\title{Approximate posterior with (log) normal distribution}
\usage{
\S4method{approximate}{Samples}(object, model, data, points = seq(from =
  min(data@doseGrid), to = max(data@doseGrid), length = 5L),
  refDose = median(points), logNormal = FALSE, verbose = TRUE, ...)
}
\arguments{
  \item{object}{the \code{\linkS4class{Samples}} object}

  \item{model}{the \code{\linkS4class{Model}} object}

  \item{data}{the \code{\linkS4class{Data}} object}

  \item{points}{optional parameter, which gives the dose
  values at which the approximation should rely on
  (default: 5 values equally spaced from minimum to maximum
  of the dose grid)}

  \item{refDose}{the reference dose to be used (default:
  median of \code{points})}

  \item{logNormal}{use the log-normal prior? (not default)
  otherwise, the normal prior for the logistic regression
  coefficients is used}

  \item{verbose}{be verbose (progress statements and plot)?
  (default)}

  \item{\dots}{additional arguments for
  \code{\link{Quantiles2LogisticNormal}}, e.g. in order to
  control the approximation quality, etc.}

  \item{...}{unused}
}
\value{
the approximation \code{\linkS4class{Model}}
}
\description{
It is recommended to use \code{\link{set.seed}} before, in
order to be able to reproduce the resulting approximating
model exactly.
}
\keyword{methods}

