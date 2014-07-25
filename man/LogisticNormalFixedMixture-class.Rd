\docType{class}
\name{LogisticNormalFixedMixture-class}
\alias{LogisticNormalFixedMixture-class}
\title{Standard logistic model with fixed mixture of multiple bivariate normal priors}
\description{
This is standard logistic regression model with a mixture
of multiple bivariate normal priors on the intercept and
slope parameters. The weights of the normal priors are
fixed, hence no additional model parameters are introduced.
This type of prior is often used to better approximate a
given posterior distribution, or when the information is
given in terms of a mixture.
}
\details{
The covariate is the natural logarithm of the dose \eqn{x}
divided by the reference dose \eqn{x^{*}}:

\deqn{logit[p(x)] = \alpha + \beta \cdot \log(x/x^{*})}
where \eqn{p(x)} is the probability of observing a DLT for
a given dose \eqn{x}.

The prior is \deqn{(\alpha, \beta) \sim \sum_{j=1}^{K}
w_{j} Normal(\mu_{j}, \Sigma_{j})}

The weight \eqn{w_{j}} of the components are fixed and sum
to 1.

The slots of this class comprise two lists, containing the
mean vector, the covariance and precision matrices of the
two bivariate normal distributions each, the parameters of
the beta prior for the first component weight, as well as
the reference dose.
}
\section{Slots}{

\describe{
\item{\code{components}}{a list with one entry per component of the mixture.
Each entry is a list with \code{mean}, \code{cov} and \code{prec} for the
bivariate normal prior}

\item{\code{weights}}{the weights of the components, these must be positive and sum
to 1}

\item{\code{refDose}}{the reference dose \eqn{x^{*}}}
}}
\keyword{classes}

