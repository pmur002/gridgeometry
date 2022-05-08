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
  coords <- polylineoffset(x$A, x$delta, x$polylineoffsetArgs)
  grob <- xyListPolygon(coords)
  setChildren(x, gList(grob))
}

makeContent.polyoffsetGrob <- function(x)
{
  coords <- polyoffset(x$A, x$delta, x$polyoffsetArgs)
  grob <- xyListPolygon(coords)
  setChildren(x, gList(grob))
}

polylineoffset <- function(A, delta, ...)
{
  UseMethod("polylineoffset")
}

polylineoffset.grob <- function(A, delta, ...)
{
  if (isEmptyCoords(A))
  {
    stop("Empty coords grob object.")
  }
  polyA <- grobCoords(A, closed = F)
  coords <- polylineoffset(polyA, delta, ...)
}

polylineoffset.list <- function(A, delta, ...)
{
  if (length(A) == 0)
  {
    stop("Empty coordinate.")
  }
  delta = delta
  if (!is.unit(delta))
  {
    delta <- unit(delta, "npc")
  }
  delta <- min(convertWidth(delta, "inches", valueOnly = T), convertHeight(delta, "inches", valueOnly = T))
  coords <- do.call(polyclip::polylineoffset, c(list(A = A, delta = delta), ...))
}

polylineoffset.character <- function(A, delta, strict=FALSE, grep=FALSE, global=FALSE, ...)
{
  polyA <- grobCoords(grid.get(A, strict, grep, global), closed = F)
  coords <- polylineoffset(polyA, delta, ...)
}

polyoffset <- function(A, delta, ...)
{
  UseMethod("polyoffset")
}

polyoffset.grob <- function(A, delta, ...)
{
  if (isEmptyCoords(A))
  {
    stop("Empty coords grob object.")
  }
  polyA <- grobCoords(A, closed = T)
  delta = delta
  if (!is.unit(delta))
  {
    delta <- unit(delta, "npc")
  }
  delta <- min(convertWidth(delta, "inches", valueOnly = T), convertHeight(delta, "inches", valueOnly = T))
  coords <- do.call(polyclip::polyoffset, c(list(A = polyA, delta = delta), ...))
}

polyoffset.list <- function(A, delta, ...)
{
  if (length(A) == 0)
  {
    stop("Empty coordinate.")
  }
  coords <- do.call(polyclip::polyoffset, c(list(A = A, delta = delta), ...))
}

polyoffset.character <- function(A, delta, strict=FALSE, grep=FALSE, global=FALSE, ...)
{
  polyA <- grobCoords(grid.get(A, strict, grep, global), closed = T)
  coords <- polyoffset(polyA, delta, ...)
}