polylineoffsetGrob <- function(A, delta, ...)
{
  polyA <- grobCoords(A, closed = F)
  coords <- polyclip::polylineoffset(polyA, delta = delta, ...)
  return (xyListPolygon(coords))
}

polyoffsetGrob <- function(A, delta, ...)
{
  polyA <- grobCoords(A, closed = TRUE)
  coords <- polyclip::polyoffset(polyA, delta = delta, ...)
  return (xyListPolygon(coords))
}

grid.polylineoffset <- function(A, delta, ...)
{
  UseMethod("grid.polylineoffset")
}

grid.polylineoffset.default <- function(A, delta, ...)
{
  grid.draw(polylineoffsetGrob(A, delta, ...))
}

grid.polyoffset <- function(A, delta, ...)
{
  UseMethod("grid.polyoffset")
}

grid.polyoffset.default <- function(A, delta, ...)
{
  grid.draw(polyoffsetGrob(A, delta, ...))
}