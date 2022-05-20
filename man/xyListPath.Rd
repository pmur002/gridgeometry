\name{xyListPath}
\alias{xyListPath}
\alias{xyListPolygon}
\alias{xyListLine}
\alias{xyListToPath}
\alias{xyListToPolygon}
\alias{xyListToLine}
\title{
  Generate Grobs from Coordinates
}
\description{
  This function generates a grob from a set of coordinates
  (a list of (x,y) lists).  
}
\usage{
xyListToPath(x, rule=, name=NULL, gp=gpar())
xyListToPolygon(x, name=NULL, gp=gpar())
xyListToLine(x, name=NULL, gp=gpar()) 

xyListPath(x, rule, name=NULL, gp=gpar())
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
\details{
  Following the addition of the \code{\link{xyListFromGrob}} function,
  the \code{*To*()} forms are preferred.
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
