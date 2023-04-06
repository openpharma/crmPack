% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Simulations-class.R
\docType{class}
\name{PseudoDualSimulations-class}
\alias{PseudoDualSimulations-class}
\alias{.PseudoDualSimulations}
\title{Class \code{PseudoDualSimulations}}
\description{
This is a class which captures the trial simulations design using both the
DLE and efficacy responses. The design of model from \code{\linkS4class{ModelTox}}
class and the efficacy model from \code{\linkS4class{ModelEff}} class
(except \code{\linkS4class{EffFlexi}} class). It contains all slots from
\code{\linkS4class{GeneralSimulations}} and \code{\linkS4class{PseudoSimulations}} object.
In comparison to the parent class \code{\linkS4class{PseudoSimulations}},
it contains additional slots to
capture the dose-efficacy curve and the sigma2 estimates.
}
\section{Slots}{

\describe{
\item{\code{fitEff}}{list of the final values. If DLE and efficacy samples are generated, it contains the
final fitted values. If no DLE and efficacy samples are used, it contains the modal estimates of the
parameters in the two models and the posterior estimates of the probabilities of the occurrence of a
DLE and the expected efficacy responses.}

\item{\code{FinalGstarEstimates}}{a vector of the final estimates of Gstar at the end of each simulations.}

\item{\code{FinalGstarAtDoseGrid}}{is a vector of the final estimates of Gstar at dose Grid at the end of each simulations}

\item{\code{FinalGstarCIs}}{is the list of all 95\% credibility interval of the final estimates of Gstar}

\item{\code{FinalGstarRatios}}{is the vector of the ratios of the CI, the ratio of the upper to the lower 95\% credibility interval
of the final estimates of Gstar}

\item{\code{FinalOptimalDose}}{is the vector of the final optimal dose, the minimum of the final TDtargetEndOfTrial estimates and Gstar
estimates}

\item{\code{FinalOptimalDoseAtDoseGrid}}{is the vector of the final optimal dose, the minimum of the final TDtargetEndOfTrial estimates
and Gstar estimates at dose Grid}

\item{\code{sigma2est}}{the vector of the final posterior mean sigma2 estimates}
}}
