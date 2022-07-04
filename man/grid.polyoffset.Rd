\name{grid.polyoffset}
\alias{grid.polyoffset}
\alias{polyoffsetGrob}
\title{
  Perform offset region on Grobs
}
\description{
  Given a polygonal region or closed grob, generate the offset region (guard region, buffer region, morphological dilation) formed by shifting the boundary outwards by a specific distance.  
}
\usage{
polyoffsetGrob(A, delta, reduce = "union", rule = "winding",
             name=NULL, gp=gpar(),  ...) 
grid.polyoffset(A, delta, ...)
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
  \item{reduce}{
    A character value describing the operation to be used if 
    \code{x} needs to be collapsed to a single shape.  One of
    \code{"intersection"}, \code{"flatten"}, \code{"minus"}, \code{"union"}, or
    \code{"xor"}.
  }

  \item{name}{
    A name for the resulting grob.
  }
  \item{gp}{
    Graphical parameter settings for the resulting grob.
  }
  \item{\dots}{
    For \code{polyoffsetGrob}, arguments passed on to \code{polyclip::polyoffset}.
  }
}
\details{
  The grobs are converted to coordinates by calling \code{grid::xyListFromGrob}.
  
  The result is a new grob.  In the case of \code{grid.polyoffset},
  this new grob will be drawn on the current device.  In the special
  case that \code{A} is a gPath, by default, the new grob will
  \emph{replace} the old grob (the original grob identified by \code{A})
  in the current scene
  (and the new grob will use the same \code{gp} settings as the
  old grob).
  
  Examples of useful arguments that will be passed on to polyclip::polyoffset() are \code{jointype}.

  The argument \code{jointype} determines what happens at the vertices of each line at \code{polyclip::polyoffset}. See code link \code{polyoffset}.
  
  The argument \code{rule} is a character value describing the fill rule to be used. One of \code{"winding"}, \code{"evenodd"}
  
}
\value{
  \code{polyoffsetGrob} returns a gTree with offset grob coordinate.
  
  \code{grid.polyoffset} is only used for its side-effect of drawing
  on the current graphics device.
}
\author{
  Jack Wong
}
\seealso{
  \code{\link{polyoffset}}
}
\examples{
grob <- rectGrob(width = 0.5, height = 0.5)
offset <- polyoffsetGrob(grob, 0.2)
grid.draw(offset)
}
\keyword{ dplot }
\keyword{ aplot }
