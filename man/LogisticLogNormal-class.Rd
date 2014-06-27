\docType{class}
\name{LogisticLogNormal-class}
\alias{LogisticLogNormal-class}
\title{Standard logistic model with bivariate (log) normal prior}
\description{
This is the usual logistic regression model with a
bivariate normal prior on the intercept and log slope.
}
\details{
The covariate is the natural logarithm of the dose \eqn{x}
divided by the reference dose \eqn{x^{*}}:

\deqn{logit[p(x)] = \alpha + \beta \cdot \log(x/x^{*})}
where \eqn{p(x)} is the probability of observing a DLT for
a given dose \eqn{x}.

The prior is \deqn{(\alpha, \log(\beta)) \sim Normal(\mu,
\Sigma)}

The slots of this class contain the mean vector and the
covariance matrix of the bivariate normal distribution, as
well as the reference dose.
}
\section{Slots}{

\describe{
\item{\code{mean}}{the prior mean vector \eqn{\mu}}

\item{\code{cov}}{the prior covariance matrix \eqn{\Sigma}}

\item{\code{refDose}}{the reference dose \eqn{x^{*}}}
}}
\keyword{classes}

