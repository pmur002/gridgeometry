
## Functions for generating a polygon from a grob

grobPolygon <- function(x, ...) {
    UseMethod("grobPolygon")
}

grobClosed <- function(x, ...) {
    UseMethod("grobClosed")
}

grobClosed.grob <- function(x, ...) {
    TRUE
}

grobPolygon.circle <- function(x, n=100, ...) {
    cx <- convertX(x$x, "in", valueOnly=TRUE)
    cy <- convertY(x$y, "in", valueOnly=TRUE)
    r <- min(convertWidth(x$r, "in", valueOnly=TRUE),
             convertHeight(x$r, "in", valueOnly=TRUE))
    t <- seq(0, 2*pi, length.out=n+1)[-(n+1)]
    xx <- cx + r*cos(t)
    yy <- cy + r*sin(t)
    list(x=xx, y=yy)
}

grobPolygon.rect <- function(x, ...) {
    hjust <- resolveHJust(x$just, x$hjust)
    vjust <- resolveVJust(x$just, x$vjust)
    w <- convertWidth(x$width, "in", valueOnly=TRUE)
    h <- convertHeight(x$height, "in", valueOnly=TRUE)
    left <- convertX(x$x, "in", valueOnly=TRUE) - hjust*w
    bottom <- convertY(x$y, "in", valueOnly=TRUE) - vjust*h
    right <- left + w
    top <- bottom + h
    list(x=c(left, left, right, right),
         y=c(bottom, top, top, bottom))
}

grobPolygon.xspline <- function(x, ...) {
    trace <- xsplinePoints(x)
    list(x=convertX(trace$x, "in", valueOnly=TRUE),
         y=convertY(trace$y, "in", valueOnly=TRUE))
}

grobClosed.xspline <- function(x, ...) {
    !x$open
}

## TODO
## More 'grid' primitives

## "gList"s
grobPolygon.gList <- function(x, ...) {
    ## Some children may produce list of lists
    polys <- lapply(x, grobPolygon, ...)
    polyLists <- lapply(polys,
                        function(p) {
                            if ("x" %in% names(p)) {
                                list(p)
                            } else {
                                p
                            }
                        })
    do.call("c", polyLists)
}

## TODO
## "gTree"s
