\name{polylineoffset}
\alias{polylineoffset}
\alias{polylineoffset.list}
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
                         ..., strict=FALSE, grep=FALSE, global=FALSE)
\method{polylineoffset}{character}(A, delta,
                             ..., strict=FALSE, grep=FALSE, global=FALSE)
\method{polylineoffset}{list}(A, delta,
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
    For \code{polylineoffsetGrob}, arguments passed on to \code{polyclip::polylineoffset}.
  }
}
\details{
  Calculate the offset region by using the subject coordinates shift by a delta distance.
  
  The argument \code{jointype} determines what happens at the vertices of each line.
  \itemize{
    \item{\code{jointype = "round"}: a circular arc is generated.}
    \item{\code{jointype = "square"}: circular arc is replaced by a single straight line.}
    \item{\code{jointype = "miter"}: circular arc is omitted entirely and replaced by a single straight line.}
  }
  
  The argument \code{endtype} determines what happens at the beginning and end of each line.
  \itemize{
    \item{\code{endtype = "closedpolygon"}: ends are joined together (using the \code{jointype value}) and the path filled as a polygon.}
    \item{\code{endtype = "closedline"}: nds are joined together (using the \code{jointype value}) and the path filled as a polyline.}
    \item{\code{endtype = "openbutt"}: ends are squared off abruptly.}
    \item{\code{endtype = "opensquare"}: ends are squared off at distance \code{delta}.}
    \item{\code{endtype = "openround"}: ends are replaced by a semicircular arc.}
  }
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
offset <- polylineoffset(grobLine, delta = unit(0.1, "cm"), 
                        jointype="square", endtype = "opensquare")
}
\keyword{ dplot }
\keyword{ aplot }
