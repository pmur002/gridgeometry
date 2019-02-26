\name{grid.trim}
\alias{grid.trim}
\alias{trimGrob}
\title{
  Subset a Line
}
\description{
  These functions generate one or more lines
  by subsetting a line.
}
\usage{
trimGrob(x, from, to, rep=FALSE, name=NULL, gp=gpar(), ...) 
grid.trim(x, ...)
}
\arguments{
  \item{x}{
    A grob, or a gPath (or a character value)
    identifying a grob that has already been drawn.  
  }
  \item{from}{
    A numeric vector or a unit object describing the start
    point of each subset.
  }
  \item{to}{
    A numeric vector or a unit object describing the end
    point of each subset.
  }
  \item{rep}{
    A logical value indicating whether the \code{from} and
    \code{to} values should be repeated to consume the
    entire line.
  }
  \item{name}{
    A name for the resulting grob.
  }
  \item{gp}{
    Graphical parameter settings for the resulting grob.
  }
  \item{\dots}{
    For \code{trimGrob}, arguments passed on to \code{trim}.
    For \code{grid.trim}, arguments used by methods.
  }
}
\details{
  Both \code{from} and \code{to} should be either numeric values
  between 0 and 1, expressing a proportion of the line length,
  or unit objects.  In the latter case, the unit
  is converted to a proportion of the line length, with \code{"npc"}
  units treated as proportions of the line length.
  
  Both \code{from} and \code{to} can be vectors (and they
  are recycled) so that multiple subsets can be obtained in
  a single call.
  
  Both \code{from} and \code{to}
  can be negative, in which case the value is treated as distance
  backwards along the line from its end.
  
  The result is a new grob.  In the case of \code{grid.trim},
  this new grob will be drawn on the current device.  In the special
  case that \code{x} is a gPath, by default, the new grob will
  \emph{replace} the old grob (the original grob identified by \code{x})
  in the current scene
  (and the new grob will use the same \code{gp} settings as the
  old grob).
}
\value{
  \code{trimGrob} returns a polyline grob.
  
  \code{grid.trim} is only used for its side-effect of drawing
  on the current graphics device.
}
\author{
  Paul Murrell
}
\seealso{
  \code{\link{trim}}
}
\examples{
g <- xsplineGrob(c(.2, .5, .8), c(.2, .8, .2))
grid.draw(g)
grid.trim(g, from=.1, to=.2, gp=gpar(lwd=5))
grid.trim(g, from=.1, to=.2, rep=TRUE, gp=gpar(lwd=3))
}
\keyword{ dplot }
\keyword{ aplot }
