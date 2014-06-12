\docType{methods}
\name{plot,Samples,DualEndpoint-method}
\alias{plot,Samples,DualEndpoint-method}
\title{Plot method for the "Samples" object, when we have
the dual endpoint model}
\usage{
\S4method{plot}{Samples,DualEndpoint}(x, y, data, extrapolate = TRUE, ...)
}
\arguments{
  \item{x}{the \code{\linkS4class{Samples}} object}

  \item{y}{the \code{\linkS4class{DualEndpoint}} object}

  \item{data}{the \code{\linkS4class{DataDual}} object}

  \item{extrapolate}{should the biomarker fit be
  extrapolated to the whole dose grid? (default)}

  \item{...}{Arguments to be passed to methods, such as
  \link{graphical parameters} (see \code{\link{par}}).
  Many methods will accept the following arguments:
  \describe{ \item{\code{type}}{what type of plot should be
  drawn.  Possible types are \itemize{ \item \code{"p"} for
  \bold{p}oints, \item \code{"l"} for \bold{l}ines, \item
  \code{"b"} for \bold{b}oth, \item \code{"c"} for the
  lines part alone of \code{"b"}, \item \code{"o"} for both
  \sQuote{\bold{o}verplotted}, \item \code{"h"} for
  \sQuote{\bold{h}istogram} like (or \sQuote{high-density})
  vertical lines, \item \code{"s"} for stair \bold{s}teps,
  \item \code{"S"} for other \bold{s}teps, see
  \sQuote{Details} below, \item \code{"n"} for no plotting.
  } All other \code{type}s give a warning or an error;
  using, e.g., \code{type = "punkte"} being equivalent to
  \code{type = "p"} for S compatibility.  Note that some
  methods, e.g. \code{\link{plot.factor}}, do not accept
  this.  } \item{\code{main}}{an overall title for the
  plot: see \code{\link{title}}.} \item{\code{sub}}{a sub
  title for the plot: see \code{\link{title}}.}
  \item{\code{xlab}}{a title for the x axis: see
  \code{\link{title}}.} \item{\code{ylab}}{a title for the
  y axis: see \code{\link{title}}.} \item{\code{asp}}{the
  \eqn{y/x} aspect ratio, see \code{\link{plot.window}}.} }
  }
}
\value{
the \code{\link[ggplot2]{ggplot}} object
}
\description{
Plot method for the "Samples" object, when we have the dual
endpoint model
}
\keyword{methods}

