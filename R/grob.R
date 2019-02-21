
## Convert polyclip() results to grobs

## Convert (closed) 'polyclip' polygon result to 'grid' path
xyListPath <- function(x, rule="winding", name=NULL, gp=gpar()) {
    if (isEmptyCoords(x)) {
        nullGrob(name=name)
    } else {
        xx <- unlist(lapply(x, "[[", "x"))
        yy <- unlist(lapply(x, "[[", "y"))
        lengths <- sapply(x, function(y) length(y$x))
        pathGrob(xx, yy, default.units="in",
                 id.lengths=lengths, rule=rule,
                 name=name, gp=gp)
    }
}

## Convert (closed) 'polyclip' polygon result to 'grid' polygons
xyListPolygon <- function(x, name=NULL, gp=gpar()) {
    if (isEmptyCoords(x)) {
        nullGrob(name=name)
    } else {
        xx <- unlist(lapply(x, "[[", "x"))
        yy <- unlist(lapply(x, "[[", "y"))
        lengths <- sapply(x, function(y) length(y$x))
        polygonGrob(xx, yy, default.units="in",
                    id.lengths=lengths,
                    name=name, gp=gp)
    }
}

## Convert (open) 'polyclip' polygon result to 'grid' polyline
xyListLine <- function(x, name=NULL, gp=gpar()) {
    if (isEmptyCoords(x)) {
        nullGrob(name=name)
    } else {
        xx <- unlist(lapply(x, "[[", "x"))
        yy <- unlist(lapply(x, "[[", "y"))
        lengths <- sapply(x, function(y) length(y$x))
        polylineGrob(xx, yy, default.units="in",
                     id.lengths=lengths,
                     name=name, gp=gp)
    }
}

