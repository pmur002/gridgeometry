\name{trim}
\alias{trim}
\alias{trim.grob}
\alias{trim.gPath}
\alias{trim.character}
\title{
  Generate Subsets of Coordinates
}
\description{
  This functions generates a new set of coordinates 
  by subsetting a set of coordinates.
}
\usage{
trim(x, from, to, ...)
\method{trim}{grob}(x, from, to, rep=FALSE, ...)
\method{trim}{gPath}(x, from, to, rep=FALSE,
                     strict=FALSE, grep=FALSE, global=FALSE, ...)
\method{trim}{character}(x, from, to, rep=FALSE,
                         strict=FALSE, grep=FALSE, global=FALSE, ...)
}
\arguments{
  \item{x}{
    A set of coordinates.  Or a grob, or a gPath (or a character value)
    identifying a grob that has already been drawn from which
    coordinates are generated.  
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
  \item{strict, grep, global}{
    Arguments controlling the interpretation of the gPath
    (passed to \code{grid.get}).
  }
  \item{\dots}{
    Arguments used by methods.
  }
}
\value{
  A new set of coordinates.
}
\author{
  Paul Murrell
}
\seealso{
  \code{\link{trim}}
}
\examples{
g <- segmentsGrob(0, .5, 1, .5)
trim(g, from=.1, to=.2)
trim(g, from=.1, to=.2, rep=TRUE)
}
\keyword{ dplot }
\keyword{ aplot }
