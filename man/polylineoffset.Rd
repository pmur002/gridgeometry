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
  Generate the offset region (guard region, buffer region, morphological dilation) formed by shifting the boundary outwards by a specific distance.  
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
\method{polylineoffset}{character}(A delta,
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
grob <- circleGrob(r = 0.25, gp = gpar(col = "blue", lwd = 3))
offset <- polylineoffset(xyListLine(grobCoords(grob, closed = T)), delta = 0.1, jointype="round", endtype = "openround", gp = gpar(col = "red", lwd = 3, fill = "blue"), rule = "winding")

}
\keyword{ dplot }
\keyword{ aplot }
