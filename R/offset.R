polylineoffsetGrob <- function(A, delta, rule = "winding", name=NULL, gp=gpar(), ...)
{
  gt <- gTree(A = A, delta = delta, rule = rule,
              polylineoffsetArgs = list(...), 
              name = name, gp = gp, cl = "polylineoffsetGrob")
}

polyoffsetGrob <- function(A, delta, reduce = "union", rule = "winding", name=NULL, gp=gpar(), ...)
{
  gt <- gTree(A = A, delta = delta, reduce = reduce, rule = rule,
              polyoffsetArgs = list(...), 
              name = name, gp = gp, cl = "polyoffsetGrob")
}

grid.polylineoffset <- function(A, delta, ...)
{
  UseMethod("grid.polylineoffset")
}

grid.polylineoffset.grob <- function(A, delta, ...)
{
  g <- polylineoffsetGrob(A, delta, ...)
  grid.draw(g)
  return (g)
}

grid.polylineoffset.gList <- function(A, delta, ...)
{
  g <- polylineoffsetGrob(A, delta, ...)
  grid.draw(g)
  return (g)
}

grid.polylineoffset.character <- function(A, delta, ...)
{
  g <- polylineoffsetGrob(A, delta, ...)
  grid.draw(g)
  return (g)
}

grid.polylineoffset.gPath <- function(A, delta, ...)
{
  g <- polylineoffsetGrob(A, delta, ...)
  grid.draw(g)
  return (g)
}

grid.polyoffset <- function(A, delta, ...)
{
  UseMethod("grid.polyoffset")
}

grid.polyoffset.grob <- function(A, delta, reduce = "union", ...)
{
  g <- polyoffsetGrob(A, delta, reduce, ...)
  grid.draw(g)
  return (g)
}

grid.polyoffset.gList <- function(A, delta, reduce = "union", ...)
{
  g <- polyoffsetGrob(A, delta, reduce, ...)
  grid.draw(g)
  return (g)
}

grid.polyoffset.character <- function(A, delta, reduce = "union", ...)
{
  g <- polyoffsetGrob(A, delta, reduce, ...)
  grid.draw(g)
  return (g)
}

grid.polyoffset.gPath <- function(A, delta, reduce = "union", ...)
{
  g <- polyoffsetGrob(A, delta, reduce, ...)
  grid.draw(g)
  return (g)
}

makeContent.polylineoffsetGrob <- function(x)
{
  coords <- polylineoffset(x$A, x$delta, x$polylineoffsetArgs)
  grob <- xyListPath(coords, rule = x$rule)
  setChildren(x, gList(grob))
}

makeContent.polyoffsetGrob <- function(x)
{
  coords <- polyoffset(x$A, x$delta, x$reduce, x$polyoffsetArgs)
  grob <- xyListPath(coords, rule = x$rule)
  setChildren(x, gList(grob))
}

polylineoffset <- function(A, delta, ...)
{
  UseMethod("polylineoffset")
}

polylineoffset.grob <- function(A, delta, ...)
{
  if (isEmptyCoords(grobCoords(A, closed = F)))
  {
    stop("Empty coords grob object.")
  }
  polyA <- xyListFromGrob(A, op = "flatten", closed = F)
  coords <- polylineoffset(polyA, delta, ...)
}

polylineoffset.gList <- function(A, delta, ...)
{
  if (isEmptyCoords(grobCoords(A, closed = F)))
  {
    stop("Empty coords grob object.")
  }
  polyA <- xyListFromGrob(A, op = "flatten", closed = F)
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

polylineoffset.character <- function(A, delta, ..., strict=FALSE, grep=FALSE, global=FALSE)
{
  polyA <- xyListFromGrob(grid.get(A, strict, grep, global), op = "flatten", closed = F)
  coords <- polylineoffset(polyA, delta, ...)
}

polylineoffset.gPath <- function(A, delta, ..., strict=FALSE, grep=FALSE, global=FALSE)
{
  polyA <- xyListFromGrob(grid.get(A, strict, grep, global), op = "flatten", closed = F)
  coords <- polylineoffset(polyA, delta, ...)
}

polyoffset <- function(A, delta, reduce = "union", ...)
{
  UseMethod("polyoffset")
}

polyoffset.grob <- function(A, delta, reduce = "union", ...)
{
  if (isEmptyCoords(grobCoords(A, closed = T)))
  {
    stop("Empty coords grob object.")
  }
  polyA <- xyListFromGrob(A, op = reduce, closed = T)
  coords <- polyoffset(polyA, delta, ...)
}

polyoffset.gList <- function(A, delta, reduce = "union", ...)
{
  if (isEmptyCoords(grobCoords(A, closed = T)))
  {
    stop("Empty coords grob object.")
  }
  polyA <- xyListFromGrob(A, op = reduce, closed = T)
  coords <- polyoffset(polyA, delta, ...)
}

polyoffset.list <- function(A, delta, ...)
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
  coords <- do.call(polyclip::polyoffset, c(list(A = A, delta = delta), ...))
}

polyoffset.character <- function(A, delta, ..., reduce = "union", strict=FALSE, grep=FALSE, global=FALSE)
{
  polyA <- xyListFromGrob(grid.get(A, strict, grep, global), op = reduce, closed = T)
  coords <- polyoffset(polyA, delta, ...)
}

polyoffset.gPath <- function(A, delta, ..., reduce = "union", strict=FALSE, grep=FALSE, global=FALSE)
{
  polyA <- xyListFromGrob(grid.get(A, strict, grep, global), reduce, closed = T)
  coords <- polyoffset(polyA, delta, ...)
}
