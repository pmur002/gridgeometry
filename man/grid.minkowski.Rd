\name{grid.minkowski}
\alias{grid.minkowski}
\alias{minkowskiGrob}
\title{
  Generate Minkowski Sums of Grobs
}
\description{
  Given a polygonal \dfn{pattern} and a polygonal \dfn{path},
  generate the Minkowski Sum by adding the pattern to the path.
}
\usage{
minkowskiGrob(A, B, 
              grobFn=xyListToPath,
              name=NULL, gp=gpar(), ...) 
grid.minkowski(A, B, ...)
}
\arguments{
  \item{A}{
    A grob, gList, or gTree, or a gPath or a character value
    identifying a grob that has already been drawn.  This is known as the
    \dfn{pattern} grob.
  }
  \item{B}{
    A grob, gList, or gTree, or a gPath or a character value
    identifying a grob that has already been drawn.  This is known as the
    \dfn{path} grob.
  }
  \item{grobFn}{
    The function that is used to create the final grob result.
    Predefined options are:  \code{\link{xyListToPath}},
    \code{\link{xyListToPolygon}}, and
    \code{\link{xyListToLine}}.
  }
  \item{name}{
    A name for the resulting grob.
  }
  \item{gp}{
    Graphical parameter settings for the resulting grob.
  }
  \item{\dots}{
    For \code{minkowskiGrob}, arguments passed on to
    \code{polyclip::polyminkowski}.
  }
}
\details{
  Both \code{A} and \code{B} should not contain self-intersections,
  though they can be non-convex.
}
\value{
  \code{minkowskiGrob} returns a gTree.
  
  \code{grid.minkowski} is only used for its side-effect of drawing
  on the current graphics device.
}
\author{
  Jack Wong
}
\seealso{
  \code{\link{xyListToPath}},
  \code{\link{xyListToPolygon}},
  \code{\link{xyListToLine}},
  \code{\link{polyminkowski}}
}
\examples{
pattern <- circleGrob(x = 0, y = 0, r = .1)
path <- rectGrob(width = 0.5, height = 0.5)
minkowski <- minkowskiGrob(pattern, path)
grid.draw(minkowski)
}
\keyword{ dplot }
\keyword{ aplot }
