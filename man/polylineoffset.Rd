\name{polylineoffset}
\alias{polylineoffset}
\alias{polylineoffset.grob}
\alias{polylineoffset.gList}
\alias{polylineoffset.gPath}
\alias{polylineoffset.character}
\title{
  Perform Line offset region on Grobs
}
\description{
  Given a list of polygonal lines or open grob object, generate the offset region (guard region, buffer region, morphological dilation) formed by shifting the boundary outwards by a specific distance.  
}
\usage{
polylineoffset(A, delta, ...)
\method{polylineoffset}{grob}(A, delta, 
         ...)
\method{polylineoffset}{gList}(A, delta,
         ...)
\method{polylineoffset}{gPath}(A, delta,
                         strict=FALSE, grep=FALSE, global=FALSE,
                         ...)
\method{polylineoffset}{character}(A, delta,
                             strict=FALSE, grep=FALSE, global=FALSE,
                             ...)
\method{polylineoffset}{list}(A, delta,
                             strict=FALSE, grep=FALSE, global=FALSE,
                             ...)
}
\arguments{
  \item{A}{
    A set of coordinates describing the
    \dfn{subject} shape.
    Or a grob, gList, or a gPath (or a character value)
    identifying a grob that has already been drawn from which
    coordinates are generated.  
  }
  
  \item{delta}{
    Distance over which the boundary should be shifted.
  }
  
  \item{jointype}{
    Type of join operation to be performed at each vertex. One of \code{"round"}, \code{"square"}, \code{"miter"}
    \code{"round"} generate circular arc around the corner. \code{"square"} replace the circular arc with single          straight line. \code{"miter"} omit circular arc and replace with straight line completely.
  }
  \item{endtype}{
    Type of geometrical operation to be performed a the start and end of each line. One of  \code{"closedpolygon"},       \code{"closedline"}, \code{"openbutt"}, \code{"opensquare"}, \code{"openround"}
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
  Calculate the offset region by using the subject coordinates shift by a delta distance.
}
\value{
  The result is a new set of coordinates for the outline of the offset region.  
}
\author{
  Jack Wong
}
\seealso{
  \code{\link{polylineoffset}}
}
\examples{
grobLine = linesGrob(x = c(.4, .8, .8, .2, .6), y = c(.3, .3, .8, .8, .6), name = "Line 1")
offset <- polylineoffset(grobLine, delta = unit(0.1, "cm"), jointype="square", endtype = "opensquare")
}
\keyword{ dplot }
\keyword{ aplot }
