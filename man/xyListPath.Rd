\name{xyListPath}
\alias{xyListPath}
\alias{xyListPolygon}
\alias{xyListLine}
\title{
  Generate Grobs from Coordinates
}
\description{
  This function generates a grob from a set of coordinates
  (a list of (x,y) lists).  
}
\usage{
xyListPath(x, rule="winding", name=NULL, gp=gpar())
xyListPolygon(x, name=NULL, gp=gpar())
xyListLine(x, name=NULL, gp=gpar()) 
}
\arguments{
  \item{x}{
    A set of coordinates (e.g., from \code{polyclip} or \code{trim}).
  }
  \item{rule}{
    A fill rule: \code{"winding"} or \code{"evenodd"}.
  }
  \item{name}{
    A name for the resulting grob.
  }
  \item{gp}{
    Graphical parameter settings for the resulting grob.
  }
}
\value{
  The result is a grob.  
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
coords <- polyclip(r, c)
xyListPath(coords)
xyListLine(coords)
}
\keyword{ dplot }
\keyword{ aplot }
