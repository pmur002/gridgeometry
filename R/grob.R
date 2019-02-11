
## Convert polyclip() results to grobs

## Convert (closed) 'polyclip' polygon result to 'grid' path
polyclipPath <- function(x, name=NULL, gp=gpar()) {
    if (length(x)) {
        xx <- unlist(lapply(x, "[[", "x"))
        yy <- unlist(lapply(x, "[[", "y"))
        lengths <- sapply(x, function(y) length(y$x))
        pathGrob(xx, yy, default.units="in",
                 id.lengths=lengths,
                 name=name, gp=gp)
    } else {
        nullGrob(name=name)
    }
}

## Convert (closed) 'polyclip' polygon result to 'grid' polygons
polyclipPolygon <- function(x, name=NULL, gp=gpar()) {
    if (length(x)) {
        xx <- unlist(lapply(x, "[[", "x"))
        yy <- unlist(lapply(x, "[[", "y"))
        lengths <- sapply(x, function(y) length(y$x))
        polygonGrob(xx, yy, default.units="in",
                    id.lengths=lengths,
                    name=name, gp=gp)
    } else {
        nullGrob(name=name)
    }
}

## Convert (open) 'polyclip' polygon result to 'grid' polyline
polyclipLine <- function(x, name=NULL, gp=gpar()) {
    if (length(x)) {
        xx <- unlist(lapply(x, "[[", "x"))
        yy <- unlist(lapply(x, "[[", "y"))
        lengths <- sapply(x, function(y) length(y$x))
        polylineGrob(xx, yy, default.units="in",
                     id.lengths=lengths,
                     name=name, gp=gp)
    } else {
        nullGrob(name=name)
    }
}

