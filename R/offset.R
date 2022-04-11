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
  delta = x$delta
  if (!is.unit(x$delta))
  {
    delta <- unit(x$delta, "npc")
  }
  delta <- min(convertWidth(delta, "inches", valueOnly = T), convertHeight(delta, "inches", valueOnly = T))
  coords <- do.call(polyclip::polylineoffset, c(list(A = polyA, delta = delta), x$polylineoffsetArgs))
  grob <- xyListPolygon(coords)
  if (length(coords) == 0){
    return (x)
  }
  setChildren(x, gList(grob))
}

makeContent.polyoffsetGrob <- function(x)
{
  polyA <- grobCoords(x$A, closed = T)
  delta = x$delta
  if (!is.unit(x$delta))
  {
    delta <- unit(x$delta, "npc")
  }
  delta <- min(convertWidth(delta, "inches", valueOnly = T), convertHeight(delta, "inches", valueOnly = T))
  coords <- do.call(polyclip::polyoffset, c(list(A = polyA, delta = delta), x$polyoffsetArgs))
  grob <- xyListPolygon(coords)
  if (length(coords) == 0){
    return (x)
  }
  setChildren(x, gList(grob))
}

polylineoffset <- function(A, delta, ...)
{
  UseMethod("polylineoffset")
}

polylineoffset.grob <- function(A, delta, ...)
{
  grid.polylineoffset(A, delta, ...)
}

polylineoffset.list <- function(A, delta, ...)
{
  grob <- xyListLine(A)
  grid.polylineoffset(grob, delta, ...)
}

polyoffset <- function(A, delta, ...)
{
  UseMethod("polyoffset")
}

polyoffset.grob <- function(A, delta, ...)
{
  grid.polyoffset(A, delta, ...)  
}

polyoffset.list <- function(A, delta, ...)
{
  grob <- xyListPolygon(A)
  grid.polyoffset(grob, delta, ...)
}