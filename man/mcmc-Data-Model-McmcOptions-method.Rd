\docType{methods}
\name{mcmc,Data,Model,McmcOptions-method}
\alias{mcmc,Data,Model,McmcOptions-method}
\title{Standard method which uses BUGS}
\usage{
\S4method{mcmc}{Data,Model,McmcOptions}(data, model, options,
  program = "OpenBUGS", verbose = FALSE, ...)
}
\arguments{
  \item{program}{the program which shall be used (see
  \code{\link[R2WinBUGS]{bugs}} for details)}

  \item{verbose}{shall messages be printed? (not default)}

  \item{data}{The data input, an object of class
  \code{\linkS4class{Data}}}

  \item{model}{The model input, an object of class
  \code{\linkS4class{Model}}}

  \item{options}{MCMC options, an object of class
  \code{\linkS4class{McmcOptions}}}

  \item{...}{unused}
}
\description{
Standard method which uses BUGS
}

