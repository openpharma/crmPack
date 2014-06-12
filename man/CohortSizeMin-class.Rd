\docType{class}
\name{CohortSizeMin-class}
\alias{CohortSizeMin-class}
\title{Size based on minimum of multiple cohort size rules}
\description{
This class can be used to combine multiple cohort size
rules with the MIN operation.
}
\details{
\code{cohortSizeList} contains all cohort size rules, which
are again objects of class \code{\linkS4class{CohortSize}}.
The minimum of these individual cohort sizes is taken to
give the final cohort size.
}
\section{Slots}{

\describe{
\item{\code{cohortSizeList}}{list of cohort size rules}
}}
\keyword{classes}

