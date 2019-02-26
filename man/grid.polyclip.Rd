\name{grid.polyclip}
\alias{grid.polyclip}
\alias{polyclipGrob}
\title{
  Perform Geometric Operations on Grobs
}
\description{
  These functions allow two or more grobs to be combined
  using one of the following operations: intersection, union,
  minus, and xor.  
}
\usage{
polyclipGrob(A, B, op="intersection",
             openFn=xyListLine, closedFn=xyListPath,
             name=NULL, gp=gpar(),  ...) 
grid.polyclip(A, B, ...)
}
\arguments{
  \item{A}{
    A grob, gList, or gTree, or a gPath or a character value
    identifying a grob that has already been drawn.  This is known as the
    \dfn{subject} grob.
  }
  \item{B}{
    A grob, gList, or gTree, or a gPath or a character value
    identifying a grob that has already been drawn.  This is known as the
    \dfn{clip} grob.
  }
  \item{op}{
    A character value describing the operation.  One of
    \code{"intersection"}, \code{"minus"}, \code{"union"}, or
    \code{"xor"}.
  }
  \item{openFn}{
    The function used to create grobs from the open shapes in the result.
  }
  \item{closedFn}{
    The function used to create grobs from the closed shapes in the
    result.
  }
  \item{name}{
    A name for the resulting grob.
  }
  \item{gp}{
    Graphical parameter settings for the resulting grob.
  }
  \item{\dots}{
    For \code{polyclipGrob}, arguments passed on to \code{polyclip}.
    For \code{grid.polyclip}, arguments used by methods.
  }
}
\details{
  The subject grob is combined with the clip grob using the \code{op}
  operation.

  The grobs are converted to coordinates by calling \code{grid::grobCoords}
  and then the operation is performed by calling \code{polyclip}.
  
  The result is a new grob.  In the case of \code{grid.polyclip},
  this new grob will be drawn on the current device.  In the special
  case that \code{A} is a gPath, by default, the new grob will
  \emph{replace} the old grob (the original grob identified by \code{A})
  in the current scene
  (and the new grob will use the same \code{gp} settings as the
  old grob).

  The subject grob can be any combination of open or closed shapes
  (e.g., a combination of lines and polygons), but the clip grob
  must only consist of closed shapes.
}
\value{
  \code{polygonGrob} returns a gTree with two children, one
  representing the open shapes within the result and one representing the
  closed shapes within the result.
  
  \code{grid.polygon} is only used for its side-effect of drawing
  on the current graphics device.
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
grid.draw(r)
grid.draw(c)
grid.polyclip(r, c, gp=gpar(fill="grey"))
}
\keyword{ dplot }
\keyword{ aplot }
