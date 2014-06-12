\docType{class}
\name{NextBestDualEndpoint-class}
\alias{NextBestDualEndpoint-class}
\title{The class with the input for finding the next dose
based on the dual endpoint model}
\description{
The class with the input for finding the next dose based on
the dual endpoint model
}
\section{Slots}{

\describe{
\item{\code{target}}{the biomarker level, relative to the maximum, that
needs to be reached. For example, 0.9 means that a dose with 90%
of the maximum biomarker level is considered as having reached
sufficient biomarker level.}

\item{\code{overdose}}{the overdose toxicity interval}

\item{\code{maxOverdoseProb}}{maximum overdose probability that is allowed}
}}
\keyword{classes}

