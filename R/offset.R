
polylineoffsetGrob <- function(A, delta, rule = "winding",
                               name=NULL, gp=gpar(), ...) {
    gt <- gTree(A = A, delta = delta, rule = rule,
                polylineoffsetArgs = list(...), 
                name = name, gp = gp, cl = "polylineoffsetGrob")
}

polyoffsetGrob <- function(A, delta, reduce = "union", rule = "winding",
                           name=NULL, gp=gpar(), ...) {
    gt <- gTree(A = A, delta = delta, reduce = reduce, rule = rule,
                polyoffsetArgs = list(...), 
                name = name, gp = gp, cl = "polyoffsetGrob")
}

grid.polylineoffset <- function(A, delta, ...) {
    UseMethod("grid.polylineoffset")
}

grid.polylineoffset.grob <- function(A, delta, ...) {
    g <- polylineoffsetGrob(A, delta, ...)
    grid.draw(g)
    invisible(g)
}

grid.polylineoffset.gList <- function(A, delta, ...) {
    g <- polylineoffsetGrob(A, delta, ...)
    grid.draw(g)
    invisible(g)
}

grid.polylineoffset.gPath <- function(A, delta, ..., name=A$name, gp=NULL,
                                      strict=FALSE, grep=FALSE) {
    oldgrob <- grid.get(A, strict=strict, grep=grep)
    if (is.null(gp)) {
        gp <- oldgrob$gp
    } 
    newgrob <- forceGrob(polylineoffsetGrob(oldgrob, delta,
                                            name=name, gp=gp, ...))
    if (name != A$name) {
        grid.draw(newgrob)
    } else {
        grid.set(A, newgrob, strict, grep)
    }
}

grid.polylineoffset.character <- function(A, delta, ...) {
    polylineoffsetGrob(gPath(A), delta, ...)
}

grid.polyoffset <- function(A, delta, ...) {
    UseMethod("grid.polyoffset")
}

grid.polyoffset.grob <- function(A, delta, reduce = "union", ...) {
    g <- polyoffsetGrob(A, delta, reduce, ...)
    grid.draw(g)
    invisible(g)
}

grid.polyoffset.gList <- function(A, delta, reduce = "union", ...) {
    g <- polyoffsetGrob(A, delta, reduce, ...)
    grid.draw(g)
    invisible(g)
}

grid.polyoffset.character <- function(A, delta, reduce = "union", ...)
{
    g <- polyoffsetGrob(A, delta, reduce, ...)
    grid.draw(g)
    invisible(g)
}

grid.polyoffset.gPath <- function(A, delta, reduce = "union", ...,
                                  name=A$name, gp=NULL,
                                  strict=FALSE, grep=FALSE) {
    oldgrob <- grid.get(A, strict=strict, grep=grep)
    if (is.null(gp)) {
        gp <- oldgrob$gp
    } 
    newgrob <- forceGrob(polyoffsetGrob(oldgrob, delta, reduce=reduce,
                                        name=name, gp=gp, ...))
    if (name != A$name) {
        grid.draw(newgrob)
    } else {
        grid.set(A, newgrob, strict, grep)
    }
}

makeContent.polylineoffsetGrob <- function(x) {
    coords <- do.call(polylineoffset,
                      c(list(x$A, x$delta), x$polylineoffsetArgs))
    grob <- xyListPath(coords, rule = x$rule)
    setChildren(x, gList(grob))
}

makeContent.polyoffsetGrob <- function(x) {
    coords <- do.call(polyoffset,
                      c(list(x$A, x$delta, x$reduce), x$polyoffsetArgs))
    grob <- xyListPath(coords, rule = x$rule)
    setChildren(x, gList(grob))
}

polylineoffset <- function(A, delta, ...) {
    UseMethod("polylineoffset")
}

polylineoffset.grob <- function(A, delta, ...) {
    if (isEmptyCoords(grobCoords(A, closed = FALSE))) {
        stop("Empty coords grob object.")
    }
    polyA <- xyListFromGrob(A, op = "flatten", closed = FALSE)
    coords <- polylineoffset(polyA, delta, ...)
}

polylineoffset.gList <- function(A, delta, ...) {
    if (isEmptyCoords(grobCoords(A, closed = FALSE))) {
        stop("Empty coords grob object.")
    }
    polyA <- xyListFromGrob(A, op = "flatten", closed = FALSE)
    coords <- polylineoffset(polyA, delta, ...)
}

polylineoffset.list <- function(A, delta, ...) {
    if (length(A) == 0) {
        stop("Empty coordinate.")
    }
    if (!is.unit(delta)) {
        delta <- unit(delta, "npc")
    }
    delta <- min(convertWidth(delta, "inches", valueOnly = TRUE),
                 convertHeight(delta, "inches", valueOnly = TRUE))
    param <- list(...)
    if (!"jointype" %in% names(param)) {
        param <- c(param, jointype = "round")
    }
    if ("jointype" %in% names(param) &&
        param$jointype == "mitre") {
        param$jointype <- "miter"
    }
    if (!"endtype" %in% names(param)) {
        param <- c(param, endtype = "openround")
    }
    do.call(polyclip::polylineoffset,
            c(list(A = A, delta = delta), param))
}

polylineoffset.character <- function(A, delta, ...,
                                     strict=FALSE, grep=FALSE, global=FALSE) {
    polyA <- xyListFromGrob(grid.get(A, strict, grep, global),
                            op = "flatten", closed = FALSE)
    coords <- polylineoffset(polyA, delta, ...)
}

polylineoffset.gPath <- function(A, delta, ...,
                                 strict=FALSE, grep=FALSE, global=FALSE) {
    polyA <- xyListFromGrob(grid.get(A, strict, grep, global),
                            op = "flatten", closed = FALSE)
    polylineoffset(polyA, delta, ...)
}

polyoffset <- function(A, delta, reduce = "union", ...) {
    UseMethod("polyoffset")
}

polyoffset.grob <- function(A, delta, reduce = "union", ...) {
    if (isEmptyCoords(grobCoords(A, closed = TRUE))) {
        stop("Empty coords grob object.")
    }
    polyA <- xyListFromGrob(A, op = reduce, closed = TRUE)
    polyoffset(polyA, delta, reduce, ...)
}

polyoffset.gList <- function(A, delta, reduce = "union", ...) {
    if (isEmptyCoords(grobCoords(A, closed = TRUE))) {
        stop("Empty coords grob object.")
    }
    polyA <- xyListFromGrob(A, op = reduce, closed = TRUE)
    polyoffset(polyA, delta, reduce, ...)
}

polyoffset.list <- function(A, delta, reduce = "union", ...) {
    if (length(A) == 0) {
        stop("Empty coordinate.")
    }
    if (!is.unit(delta)) {
        delta <- unit(delta, "npc")
    }
    delta <- min(convertWidth(delta, "inches", valueOnly = TRUE),
                 convertHeight(delta, "inches", valueOnly = TRUE))  
    param <- list(...)
    if (!"jointype" %in% names(param)) {
        param <- c(param, jointype = "round")
    }
    if ("jointype" %in% names(param) &&
        param$jointype == "mitre") {
        param$jointype <- "miter"
    }
  
    do.call(polyclip::polyoffset, c(list(A = A, delta = delta), param))
}

polyoffset.character <- function(A, delta, reduce = "union", ...,
                                 strict=FALSE, grep=FALSE, global=FALSE) {
    polyA <- xyListFromGrob(grid.get(A, strict, grep, global),
                            op = reduce, closed = TRUE)
    polyoffset(polyA, delta, reduce, ...)
}

polyoffset.gPath <- function(A, delta, reduce = "union", ...,
                             strict=FALSE, grep=FALSE, global=FALSE) {
    polyA <- xyListFromGrob(grid.get(A, strict, grep, global),
                            op = reduce, closed = TRUE)
    polyoffset(polyA, delta, reduce, ...)
}
