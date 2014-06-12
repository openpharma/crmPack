\docType{class}
\name{Simulations-class}
\alias{Simulations-class}
\title{Class for the simulations output}
\description{
This class captures the trial simulations.
}
\details{
Here also the random generator state before starting the
simulation is saved, in order to be able to reproduce the
outcome. For this just use \code{\link{set.seed}} with the
\code{seed} as argument before running
\code{\link{simulate,Design-method}}.
}
\section{Slots}{

\describe{
\item{\code{data}}{list of produced \code{\linkS4class{Data}} objects}

\item{\code{doses}}{the vector of final dose recommendations}

\item{\code{fit}}{list with the final fits}

\item{\code{stopReasons}}{list of stopping reasons for each simulation run}

\item{\code{seed}}{random generator state before starting the simulation}
}}
\keyword{classes}

