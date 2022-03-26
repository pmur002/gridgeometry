grid.polyoffset <- function(A, delta, ..., eps, x0, y0, miterlim = 2, arctol = abs(delta) / 100, jointype = c("square", "round", "miter"))
{
  polyA <- grobCoords(A, closed = TRUE)
  coords <- polyclip::polyoffset(polyA, delta = delta, ..., eps = eps, x0 = x0, y0 = y0, miterlim = miterlim, arctol = arctol, jointype = jointype)
  return (xyListPolygon(coords))
}

grid.polylineoffset <- function(A, delta, ..., eps, x0, y0, miterlim = 2, arctol = abs(delta) / 100, jointype = c("square", "round", "miter"), endtype = c("closedpolygon", "closedline", "openbutt", "opensquare", "openround", "closed", "butt", "square", "round"))
{
  polyA <- grobCoords(A, closed = F)
  coords <- polyclip::polylineoffset(polyA, delta = delta, ..., eps = eps, x0 = x0, y0 = y0, miterlim = miterlim, arctol = arctol, jointype = jointype, endtype = endtype)
  return (xyListPolygon(coords))
}