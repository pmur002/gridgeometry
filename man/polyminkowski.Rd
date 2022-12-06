\name{polyminkowski}
\alias{polyminkowski}
\alias{polyminkowski.grob}
\alias{polyminkowski.gList}
\alias{polyminkowski.gPath}
\alias{polyminkowski.character}
\title{
  Generate Minkowski Sums on Coordinates
}
\description{
  This function generates the Minkowski Sum of two sets of coordinates.  
}
\usage{
polyminkowski(A, B, ...)
\method{polyminkowski}{grob}(A, B, closed=isClosedShape(B),
         reduceA = "union",
         reduceB = "union",
         ...)
\method{polyminkowski}{gList}(A, B, closed=isClosedShape(B),
         reduceA = "union",
         reduceB = "union",
         ...)
\method{polyminkowski}{gPath}(A, B, closed,
                         strict=FALSE, grep=FALSE, global=FALSE,
         reduceA = "union",
         reduceB = "union",
         ...)
\method{polyminkowski}{character}(A, B, closed,
                             strict=FALSE, grep=FALSE, global=FALSE,
         reduceA = "union",
         reduceB = "union",
         ...)
}
\arguments{
  \item{A}{
    A set of coordinates describing a
    \dfn{pattern} shape.
    Or a grob, gList, or a gPath (or a character value)
    identifying a grob that has already been drawn from which
    coordinates are generated.  
  }
  \item{B}{
    A set of coordinates describing a
    \dfn{path} shape.
    Or a grob, gList, or a gPath (or a character value)
    identifying a grob that has already been drawn from which
    coordinates are generated.  
  }
  \item{closed}{
    A logical value indicating whether the \code{B} coordinates describe
    a closed shape or an open shape.
  }
  \item{reduceA, reduceB}{
    A character value describing the operation to be used if either
    \code{A} or \code{B} need to be reduced to a single set of
    coordinates.   One of
    \code{"intersection"}, \code{"minus"}, \code{"union"}, or
    \code{"xor"}, in which case \code{polyminkowski} is used to
    reduce multiple shapes, or \code{"flatten"}, in which case
    coordinates for all shapes are returned.  
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
  The shape described by the pattern coordinates is added
  to the shape described by the path coordinates.
}
\value{
  The result is a new set of coordinates.  
}
\author{
  Paul Murrell
}
\seealso{
  \code{\link{grid.minkowski}}
}
\examples{
c <- circleGrob(x=0, y=0, r=.1)
r <- rectGrob(width=.5, height=.5)
polyminkowski(c, r)
}
\keyword{ dplot }
\keyword{ aplot }
