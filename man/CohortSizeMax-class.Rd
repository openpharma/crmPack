\docType{class}
\name{CohortSizeMax-class}
\alias{CohortSizeMax-class}
\title{Size based on maximum of multiple cohort size rules}
\description{
This class can be used to combine multiple cohort size
rules with the MAX operation.
}
\details{
\code{cohortSizeList} contains all cohort size rules, which
are again objects of class \code{\linkS4class{CohortSize}}.
The maximum of these individual cohort sizes is taken to
give the final cohort size.
}
\section{Slots}{

\describe{
\item{\code{cohortSizeList}}{list of cohort size rules}
}}
\keyword{classes}

