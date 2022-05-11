
## High-level interface (starts with grobs and ends with grobs)

makeContent.polyclipgrob <- function(x) {
    children <- vector("list", 2)
    closedPaths <- do.call(polyclip,
                           c(list(A=x$A, B=x$B, op=x$op, closed=TRUE),
                             x$polyclipArgs))
    if (length(closedPaths)) {
        children[[1]] <- x$closedFn(closedPaths,
                                    name=paste0(x$name, ".closed"))
    }
    openPaths <- do.call(polyclip,
                         c(list(A=x$A, B=x$B, op=x$op, closed=FALSE),
                           x$polyclipArgs))
    if (length(openPaths)) {
        children[[2]] <- x$openFn(openPaths, 
                                  name=paste0(x$name, ".open"))
    }
    setChildren(x, do.call(gList, children[!is.null(children)]))
}

grobArg <- function(x) {
    is.character(x) || inherits(x, "gPath") ||
        inherits(x, "grob") || inherits(x, "gList")
}

polyclipGrob <- function(A, B, op="intersection",
                         openFn=xyListToLine, closedFn=xyListToPath,
                         name=NULL, gp=gpar(),
                         ...) {
    if (!(grobArg(A) && grobArg(B)))
        stop("Invalid argument")
    gTree(A=A, B=B, op=op, openFn=openFn, closedFn=closedFn,
          polyclipArgs=list(...),
          gp=gp, name=name, cl="polyclipgrob")
}

grid.polyclip <- function(A, B, ...) {
    UseMethod("grid.polyclip")
}

grid.polyclip.default <- function(A, B, ...) {
    grid.draw(polyclipGrob(A, B, ...))
}

grid.polyclip.gPath <- function(A, B, ..., name=A$name, gp=NULL,
                                strict=FALSE, grep=FALSE, global=FALSE) {
    if (global)
        stop("Cannot replace multiple grobs with single grob")
    oldgrob <- grid.get(A, strict=strict, grep=grep)
    if (is.null(gp)) {
        gp <- oldgrob$gp
    } 
    newgrob <- forceGrob(polyclipGrob(A, B, ..., name=name, gp=gp,
                                      strict=strict, grep=grep))
    if (name != A$name) {
        grid.draw(newgrob)
    } else {
        grid.set(A, newgrob, strict, grep)
    }
}

grid.polyclip.character <- function(A, B, ...) {
    grid.polyclip(gPath(A), B, ...)
}

makeContent.trimgrob <- function(x) {
    pts <- do.call(trim,
                   c(list(x$x, x$from, x$to, x$rep), x$trimArgs))
    setChildren(x, gList(xyListToLine(pts, name=paste0(x$name, ".lines"))))
}

trimGrob <- function(x, from, to, rep=FALSE, name=NULL, gp=gpar(), ...) {
    if (!grobArg(x))
        stop("Invalid argument")    
    gTree(x=x, from=from, to=to, rep=rep, trimArgs=list(...),
          gp=gp, name=name, cl="trimgrob")
}

grid.trim <- function(x, ...) {
    UseMethod("grid.trim")
}

grid.trim.default <- function(x, ...) {
    grid.draw(trimGrob(x, ...))
}

grid.trim.gPath <- function(x, ..., name=x$name, gp=NULL,
                            strict=FALSE, grep=FALSE, global=FALSE) {
    if (global)
        stop("Cannot replace multiple grobs with single grob")
    oldgrob <- grid.get(x, strict=strict, grep=grep)
    if (is.null(gp)) {
        gp <- oldgrob$gp
    } 
    newgrob <- forceGrob(trimGrob(x, ..., name=name, gp=gp,
                                  strict=strict, grep=grep))
    if (name != x$name) {
        grid.draw(newgrob)
    } else {
        grid.set(x, newgrob, strict, grep)
    }
}

grid.trim.character <- function(x, ...) {
    grid.trim(gPath(x), ...)
}
