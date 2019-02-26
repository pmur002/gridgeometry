\name{polyclip}
\alias{polyclip}
\alias{polyclip.grob}
\alias{polyclip.gList}
\alias{polyclip.gPath}
\alias{polyclip.character}
\title{
  Perform Geometric Operations on Coordinates
}
\description{
  This function combines two sets of coordinates
  using one of the following operations: intersection, union,
  minus, and xor.  
}
\usage{
polyclip(A, B, ...)
\method{polyclip}{grob}(A, B, op="intersection", closed=TRUE, ...)
\method{polyclip}{gList}(A, B, op="intersection", closed=TRUE, ...)
\method{polyclip}{gPath}(A, B, op="intersection", closed=TRUE,
                         strict=FALSE, grep=FALSE, global=FALSE, ...)
\method{polyclip}{character}(A, B, op="intersection", closed=TRUE,
                             strict=FALSE, grep=FALSE, global=FALSE, ...)
}
\arguments{
  \item{A}{
    A set of coordinates describing the
    \dfn{subject} shape.
    Or a grob, gList, or a gPath (or a character value)
    identifying a grob that has already been drawn from which
    coordinates are generated.  
  }
  \item{B}{
    A set of coordinates describing the
    \dfn{clip} shape.
    Or a grob, gList, or a gPath (or a character value)
    identifying a grob that has already been drawn from which
    coordinates are generated.  
  }
  \item{op}{
    A character value describing the operation.  One of
    \code{"intersection"}, \code{"minus"}, \code{"union"}, or
    \code{"xor"}.
  }
  \item{closed}{
    A logical value indicating whether the \code{A} coordinates describe
    a closed shape or an open shape.
  }
  \item{strict, grep, global}{
    Arguments controlling the interpretation of the gPath
    (passed to \code{grid.get}).
  }
  \item{\dots}{
    Arguments used by methods.
  }
}
\details{
  The subject coordinates are combined with the clip coordinates
  using the \code{op} operation.
}
\value{
  The result is a new set of coordinates.  
}
\author{
  Paul Murrell
}
\seealso{
  \code{\link{polyclip}}
}
\examples{
r <- rectGrob(x=1/3, y=1/3, width=.4, height=.4)
c <- circleGrob(x=2/3, y=2/3, r=.2)
polyclip(r, c)
}
\keyword{ dplot }
\keyword{ aplot }
