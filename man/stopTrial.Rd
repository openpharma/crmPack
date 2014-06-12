\name{stopTrial}
\alias{stopTrial}
\title{Stop the trial?}
\usage{
stopTrial(stopping, dose, samples, model, data, ...)
}
\arguments{
  \item{stopping}{The rule, an object of class
  \code{\linkS4class{Stopping}}}

  \item{dose}{the recommended next best dose}

  \item{samples}{the \code{\linkS4class{Samples}} object}

  \item{model}{The model input, an object of class
  \code{\linkS4class{Model}}}

  \item{data}{The data input, an object of class
  \code{\linkS4class{Data}}}

  \item{\dots}{additional arguments}
}
\value{
logical value: \code{TRUE} if the trial can be stopped,
\code{FALSE} otherwise. It should have an attribute
\code{message} which gives the reason for the decision.
}
\description{
This function returns whether to stop the trial.
}
\keyword{methods}

