\name{MinimalInformative}
\alias{MinimalInformative}
\title{Construct a minimally informative prior}
\usage{
MinimalInformative(dosegrid, refDose, threshmin = 0.2, threshmax = 0.3, ...)
}
\arguments{
  \item{dosegrid}{the dose grid}

  \item{refDose}{the reference dose}

  \item{threshmin}{Any toxicity probability above this
  threshold would be very unlikely (5\%) at the minimum
  dose (default: 0.2)}

  \item{threshmax}{Any toxicity probability below this
  threshold would be very unlikely (5\%) at the maximum
  dose (default: 0.3)}

  \item{\dots}{additional arguments for computations, see
  \code{\link{Quantiles2LogisticNormal}}}
}
\value{
see \code{\link{Quantiles2LogisticNormal}}
}
\description{
This function constructs a minimally informative prior,
which is captured in a \code{\linkS4class{LogisticNormal}}
object.
}
\details{
Based on the proposal by Neuenschwander et al (2008,
Statistics in Medicine), a minimally informative prior
distribution is constructed. The required key input is the
minimum (\eqn{d_{1}} in the notation of the Appendix A.1 of
that paper) and the maximum value (\eqn{d_{J}}) of the dose
grid supplied to this function. Then \code{threshmin} is
the probability threshold \eqn{q_{1}}, such that any
probability of DLT larger than \eqn{q_{1}} has only 5\%
probability. Likewise, \code{threshmax} is the probability
threshold \eqn{q_{J}}, such that any probability of DLT
smaller than \eqn{q_{J}} has only 5\% probability.
Subsequently, for all doses supplied in the \code{dosegrid}
argument, Beta distributions are set up, and
\code{\link{Quantiles2LogisticNormal}} is used to transform
the resulting quantiles into an approximating
\code{\linkS4class{LogisticNormal}} model.
}
\keyword{programming}

