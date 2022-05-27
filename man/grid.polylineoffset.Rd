\name{grid.polylineoffset}
\alias{grid.polylineoffset}
\alias{polylineoffsetGrob}
\title{
  Perform Line offset region on Grobs
}
\description{
  Generate the offset region (guard region, buffer region, morphological dilation) formed by shifting the boundary outwards by a specific distance.  
}
\usage{
polylineoffsetGrob(A, delta, rule = "winding",
             name=NULL, gp=gpar(),  ...) 
grid.polylineoffset(A, delta, rule, ...)
}
\arguments{
  \item{A}{
    A grob, gList, or gTree, or a gPath or a character value
    identifying a grob that has already been drawn.  This is known as the
    \dfn{subject} grob.
  }
  \item{delta}{
    Distance over which the boundary should be shifted.
  }
  \item{name}{
    A name for the resulting grob.
  }
  \item{gp}{
    Graphical parameter settings for the resulting grob.
  }
  \item{\dots}{
    For \code{polylineoffsetGrob}, arguments passed on to \code{polylineoffset}.
    For \code{grid.polylineoffset}, arguments used by methods.
  }
}
\details{
  The grobs are converted to coordinates by calling \code{grid::xyListFromGrob}.
  
  The result is a new grob.  In the case of \code{grid.polylineoffset},
  this new grob will be drawn on the current device.  In the special
  case that \code{A} is a gPath, by default, the new grob will
  \emph{replace} the old grob (the original grob identified by \code{A})
  in the current scene
  (and the new grob will use the same \code{gp} settings as the
  old grob).
}
\value{
  \code{polylineoffsetGrob} returns a gTree with offset grob coordinate.
  
  \code{grid.polylineoffset} is only used for its side-effect of drawing
  on the current graphics device.
}
\author{
  Jack Wong
}
\seealso{
  \code{\link{polylineoffset}}
}
\examples{
grob <- circleGrob(r = 0.25, gp = gpar(col = "blue", lwd = 3))
offset <- polylineoffsetGrob(xyListLine(grobCoords(grob, closed = T)), delta = 0.1, jointype="round", endtype = "openround", gp = gpar(col = "red", lwd = 3, fill = "blue"), rule = "winding")
grid.draw(offset)
}
\keyword{ dplot }
\keyword{ aplot }
