\name{polyoffset}
\alias{polyoffset}
\alias{polyoffset.list}
\alias{polyoffset.grob}
\alias{polyoffset.gList}
\alias{polyoffset.gPath}
\alias{polyoffset.character}
\title{
  Perform offset region on Grobs
}
\description{
  Given a polygonal region or closed grob, generate the offset region (guard region, buffer region, morphological dilation) formed by shifting the boundary outwards by a specific distance.  
}
\usage{
polyoffset(A, delta, reduce = "union", ...)
\method{polyoffset}{grob}(A, delta, reduce = "union",
         ...)
\method{polyoffset}{gList}(A, delta, reduce = "union",
         ...)
\method{polyoffset}{gPath}(A, delta, reduce = "union",
                         ..., strict=FALSE, grep=FALSE, global=FALSE)
\method{polyoffset}{character}(A, delta, reduce = "union",
                             ..., strict=FALSE, grep=FALSE, global=FALSE)
\method{polyoffset}{list}(A, delta, reduce = "union",
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
  
  \item{reduce}{
    A character value describing the operation to be used if 
    \code{A} needs to be collapsed to a single shape.  One of
    \code{"intersection"}, \code{"flatten"}, \code{"minus"}, \code{"union"}, or
    \code{"xor"}.
  }
  
  \item{strict, grep, global}{
    Arguments controlling the interpretation of the gPath
    (passed to \code{grid.get}).
  }
  \item{\dots}{
    For \code{polyoffsetGrob}, arguments passed on to \code{polyclip::polyoffset}.
  }
}
\details{
  Calculate the offset region by using the subject coordinates shift by a delta distance.
  
  Examples of useful arguments that will be passed on to polyclip::polyoffset() are \code{jointype}.
  
  The argument \code{jointype} determines what happens at the vertices of each line.
  \itemize{
    \item{\code{jointype = "round"}: a circular arc is generated.}
    \item{\code{jointype = "square"}: circular arc is replaced by a single straight line.}
    \item{\code{jointype = "miter"}: circular arc is omitted entirely and replaced by a single straight line.}
  }
}
\value{
  The result is a new set of coordinates for the outline of the offset region.  
}
\author{
  Jack Wong
}
\seealso{
  \code{\link{polyoffset}}
}
\examples{
grob <- rectGrob(width = 0.5, height = 0.5)
offset <- polyoffset(grob, 0.2)
}
\keyword{ dplot }
\keyword{ aplot }
