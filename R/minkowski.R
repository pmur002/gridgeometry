
################################################################################
## Low level coordinates interface
## Create S3 generic from polyclip::polyminkowski()
polyminkowski <- function(A, B, ...) {
    UseMethod("polyminkowski")
}

polyminkowski.default <- function(A, B, ...) {
    polyclip::polyminkowski(A, B, ...)
}

polyminkowskiGridGrob <- function(A, B, closed, reduceA, reduceB, ...) {
    if (!(inherits(B, "grob") || inherits(B, "gList")))
        stop("Argument 'B' must be a grob")
    polyA <- xyListFromGrob(A, op = reduceA, closed = TRUE, ...)
    polyB <- xyListFromGrob(B, op = reduceB, closed = closed, ...)
    polyclip::polyminkowski(polyA, polyB, closed = closed, ...)
}

polyminkowski.grob <- function(A, B, closed=isClosedShape(B),
                               reduceA = "union",
                               reduceB = "union",
                               ...) {
    polyminkowskiGridGrob(A, B, closed, reduceA, reduceB, ...)
}

polyminkowski.gList <- function(A, B, closed=isClosedShape(B),
                                reduceA = "union",
                                reduceB = "union",
                                ...) {
    polyminkowskiGridGrob(A, B, closed, reduceA, reduceB, ...)
}

polyminkowski.gPath <- function(A, B, closed,
                                strict=FALSE, grep=FALSE, global=FALSE,
                                reduceA = "union",
                                reduceB = "union",
                                ...) {
    A <- grid.get(A, strict, grep, global)
    if (inherits(B, "gPath") || is.character(B)) {
        B <- grid.get(B, ...)
    }
    if (missing(closed))
        closed <- isClosedShape(B)
    polyminkowskiGridGrob(A, B, closed, reduceA, reduceB, ...)
}

polyminkowski.character <- function(A, B, closed,
                                    strict=FALSE, grep=FALSE, global=FALSE,
                                    reduceA = "union",
                                    reduceB = "union",
                                    ...) {
    A <- grid.get(A, strict, grep, global)
    if (inherits(B, "gPath") || is.character(B)) {
        B <- grid.get(B, ...)
    }
    if (missing(closed))
        closed <- isClosedShape(B)
    polyminkowskiGridGrob(A, B, closed, reduceA, reduceB, ...)
}

################################################################################
## High level grob interface
makeContent.minkowskiGrob <- function(x) {
    children <- vector("list", 2)
    closedPaths <- do.call(polyminkowski,
                           c(list(A=x$A, B=x$B, closed=TRUE),
                             x$polyclipArgs))
    if (length(closedPaths)) {
        children[[1]] <- x$grobFn(closedPaths,
                                  name=paste0(x$name, ".closed"))
    }
    openPaths <- do.call(polyminkowski,
                         c(list(A=x$A, B=x$B, closed=FALSE),
                           x$polyclipArgs))
    if (length(openPaths)) {
        children[[2]] <- x$grobFn(openPaths, 
                                  name=paste0(x$name, ".open"))
    }
    setChildren(x, do.call(gList, children[!is.null(children)]))
}

minkowskiGrob <- function(A, B, 
                          grobFn=xyListToPath,
                          name=NULL, gp=gpar(),
                          ...) {
    if (!(grobArg(A) && grobArg(B)))
        stop("Invalid argument")
    gTree(A=A, B=B, grobFn=grobFn,
          polyclipArgs=list(...),
          gp=gp, name=name, cl="minkowskiGrob")
}

grid.minkowski <- function(A, B, ...) {
    UseMethod("grid.minkowski")
}

grid.minkowski.default <- function(A, B, ...) {
    grid.draw(minkowskiGrob(A, B, ...))
}

grid.minkowski.gPath <- function(A, B, ..., name=A$name, gp=NULL,
                                strict=FALSE, grep=FALSE, global=FALSE) {
    if (global)
        stop("Cannot replace multiple grobs with single grob")
    oldgrob <- grid.get(A, strict=strict, grep=grep)
    if (is.null(gp)) {
        gp <- oldgrob$gp
    } 
    newgrob <- forceGrob(minkowskiGrob(A, B, ..., name=name, gp=gp,
                                      strict=strict, grep=grep))
    if (name != A$name) {
        grid.draw(newgrob)
    } else {
        grid.set(A, newgrob, strict, grep)
    }
}

grid.minkowski.character <- function(A, B, ...) {
    grid.minkowski(gPath(A), B, ...)
}



