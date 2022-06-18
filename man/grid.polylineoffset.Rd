\name{grid.polylineoffset}
\alias{grid.polylineoffset}
\alias{polylineoffsetGrob}
\title{
  Perform Line offset region on Grobs
}
\description{
  Given a polygonal lines or open grob, Generate the offset region (guard region, buffer region, morphological dilation) formed by shifting the boundary outwards by a specific distance.  
}
\usage{
polylineoffsetGrob(A, delta, rule = "winding",
             name=NULL, gp=gpar(),  ...) 
grid.polylineoffset(A, delta, rule = "winding", ...)
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
  \item{rule}{
    A character value describing the fill rule to be used. One of \code{"winding"}, \code{"evenodd"}
  }
  \item{jointype}{
    Type of join operation to be performed at each vertex. One of \code{"round"}, \code{"square"}, \code{"miter"}
    \code{"round"} generate circular arc around the corner. \code{"square"} replace the circular arc with single          straight line. \code{"miter"} omit circular arc and replace with straight line completely.
  }
  \item{endtype}{
    Type of geometrical operation to be performed a the start and end of each line. One of  \code{"closedpolygon"},       \code{"closedline"}, \code{"openbutt"}, \code{"opensquare"}, \code{"openround"}
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
grobLine = linesGrob(x = c(.4, .8, .8, .2, .6), y = c(.3, .3, .8, .8, .6), name = "Line 1")
grid.polylineoffset(grobLine, delta = unit(0.1, "cm"), jointype="square", endtype = "opensquare")
}
\keyword{ dplot }
\keyword{ aplot }
