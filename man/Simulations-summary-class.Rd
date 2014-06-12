\docType{class}
\name{Simulations-summary-class}
\alias{Simulations-summary-class}
\title{Class for the summary of simulations output}
\description{
Class for the summary of simulations output
}
\section{Slots}{

\describe{
\item{\code{target}}{target toxicity interval}

\item{\code{targetDoseInterval}}{corresponding target dose interval}

\item{\code{nsim}}{number of simulations}

\item{\code{propDLTs}}{proportions of DLTs in the trials}

\item{\code{meanToxRisk}}{mean toxicity risks for the patients}

\item{\code{doseSelected}}{doses selected as MTD}

\item{\code{toxAtDosesSelected}}{true toxicity at doses selected}

\item{\code{propAtTarget}}{Proportion of trials selecting target MTD}

\item{\code{doseMostSelected}}{dose most often selected as MTD}

\item{\code{obsToxRateAtDoseMostSelected}}{observed toxicity rate at dose most often
selected}

\item{\code{fitAtDoseMostSelected}}{fitted toxicity rate at dose most often selected}

\item{\code{meanFit}}{list with the average, lower (2.5%) and upper (97.5%)
quantiles of the mean fitted toxicity at each dose level}

\item{\code{nObs}}{number of patients overall}

\item{\code{nAboveTarget}}{number of patients treated above target tox interval}

\item{\code{doseGrid}}{the dose grid that has been used}
}}
\keyword{classes}

