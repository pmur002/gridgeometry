polylineoffsetGrob <- function(A, delta, name=NULL, gp=gpar(), ...)
{
  gt <- gTree(A = A, delta = delta, 
              polylineoffsetArgs = list(...), 
              name = name, gp = gp, cl = "polylineoffsetGrob")
}

polyoffsetGrob <- function(A, delta, name=NULL, gp=gpar(), ...)
{
  gt <- gTree(A = A, delta = delta, 
              polyoffsetArgs = list(...), 
              name = name, gp = gp, cl = "polyoffsetGrob")
}

grid.polylineoffset <- function(A, delta, ...)
{
  UseMethod("grid.polylineoffset")
}

grid.polylineoffset.default <- function(A, delta, ...)
{
  g <- polylineoffsetGrob(A, delta, ...)
  grid.draw(g)
  return (g)
}

grid.polyoffset <- function(A, delta, ...)
{
  UseMethod("grid.polyoffset")
}

grid.polyoffset.default <- function(A, delta, ...)
{
  g <- polyoffsetGrob(A, delta, ...)
  grid.draw(g)
  return (g)
}

makeContent.polylineoffsetGrob <- function(x)
{
  polyA <- grobCoords(x$A, closed = F)
  coords <- do.call(polyclip::polylineoffset, c(list(A = polyA, delta = x$delta), x$polylineoffsetArgs))
  grob <- xyListPolygon(coords)
  setChildren(x, gList(grob))
}

makeContent.polyoffsetGrob <- function(x)
{
  polyA <- grobCoords(x$A, closed = T)
  coords <- do.call(polyclip::polylineoffset, c(list(A = polyA, delta = x$delta), x$polyoffsetArgs))
  grob <- xyListPolygon(coords)
  setChildren(x, gList(grob))
}
