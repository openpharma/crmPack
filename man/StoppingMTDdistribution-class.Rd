\docType{class}
\name{StoppingMTDdistribution-class}
\alias{StoppingMTDdistribution-class}
\title{Stop based on MTD distribution}
\description{
Has 90\% probability above a threshold of 50\% of the
current MTD been reached? This class is used for this
question.
}
\section{Slots}{

\describe{
\item{\code{target}}{the target toxicity probability (e.g. 0.33) defining the MTD}

\item{\code{thresh}}{the threshold relative to the MTD (e.g. 0.5)}

\item{\code{prob}}{required probability (e.g. 0.9)}
}}
\keyword{classes}

