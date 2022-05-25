\name{grid.reduce}
\alias{grid.reduce}
\alias{reduceGrob}
\title{
  Reduce Multiple Shapes to a Single Shape
}
\description{
  This function takes a grob (or gTree) that draws multiple shapes
  and reduces it to a grob that only draws one shape.

  More accurately, the result only draws at most one closed shape and
  at most one open shape.
}
\usage{
reduceGrob(x, op=if (isClosedShape(x)) "union" else "flatten",
           openFn=xyListToLine, closedFn=xyListToPath,
           name=NULL, gp=gpar()) 
grid.reduce(x, ...)
}
\arguments{
  \item{x}{
    A grob, gList, or gTree, or a gPath or a character value
    identifying a grob that has already been drawn.  
  }
  \item{op}{
    A character value describing the operation.  One of
    \code{"intersection"}, \code{"minus"}, \code{"union"},
    \code{"xor"}, or \code{"flatten"}.
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
    For \code{reduceGrob}, arguments passed on to \code{polyclip}.
    For \code{grid.reduce}, arguments used by methods.
  }
}
\details{
  Multiple shapes are combined using \code{polyclip()} and the
  specified operator \code{op}.
}
\value{
  \code{reduceGrob} returns a gTree with two children, one
  representing the open shapes within the result and one representing the
  closed shapes within the result.
  
  \code{grid.reduce} is only used for its side-effect of drawing
  on the current graphics device.
}
\author{
  Paul Murrell
}
\seealso{
  \code{\link{grid.polyclip}}
}
\examples{
r <- rectGrob(x=1/3, y=1/3, width=.4, height=.4)
c <- circleGrob(x=2/3, y=2/3, r=.3)
grid.reduce(grobTree(r, c), gp=gpar(fill="grey"))
}
\keyword{ dplot }
\keyword{ aplot }
