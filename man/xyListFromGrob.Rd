\name{xyListFromGrob}
\alias{xyListFromGrob}
\title{
  Generate Coordinates from Grobs
}
\description{
  This function generates a set of coordinates 
  (a list of (x,y) lists) from a grob.  
}
\usage{
xyListFromGrob(x, op = "union", closed = TRUE, ...)
}
\arguments{
  \item{x}{
    A grob.
  }
  \item{op}{
    A character value describing the operation to be used if 
    \code{x} needs to be collapsed to a single shape.  One of
    \code{"intersection"}, \code{"minus"}, \code{"union"}, or
    \code{"xor"}.
  }
  \item{closed}{
    A logical value indicating whether we coordinates describing
    a closed shape or an open shape from.
  }
  \item{\dots}{
    Arguments passed on to \code{polyclip}.
  }
}
\value{
  The result is a list of lists, each with components \code{x} and \code{y}.  
}
\author{
  Paul Murrell
}
\seealso{
  \code{\link{polyclip}}
}
\examples{
r <- rectGrob(x=1/3, y=1/3, width=.4, height=.4)
xyListFromGrob(r)
}
\keyword{ dplot }
\keyword{ aplot }
