
## High-level interface (starts with grobs and ends with grobs)

makeContent.polyclipgrob <- function(x) {
    children <- vector("list", 2)
    closedPaths <- do.call(polyclip,
                           c(list(A=x$A, B=x$B, op=x$op, closed=TRUE),
                             x$polyclipArgs))
    if (length(closedPaths)) {
        children[[1]] <- x$closedFn(closedPaths,
                                    paste0(x$name, ".closed"))
    }
    openPaths <- do.call(polyclip,
                         c(list(A=x$A, B=x$B, op=x$op, closed=FALSE),
                           x$polyclipArgs))
    if (length(openPaths)) {
        children[[2]] <- x$openFn(openPaths, 
                                  paste0(x$name, ".open"))
    }
    setChildren(x, do.call(gList, children[!is.null(children)]))
}

polyclipGrob <- function(A, B, op="intersection",
                         openFn=xyListLine, closedFn=xyListPath,
                         name=NULL, gp=gpar(),
                         ...) {
    gTree(A=A, B=B, op=op, openFn=openFn, closedFn=closedFn,
          polyclipArgs=list(...),
          gp=gp, name=name, cl="polyclipgrob")
}

grid.polyclip <- function(...) {
    grid.draw(polyclipGrob(...))
}

makeContent.trimgrob <- function(x) {
    pts <- trim(x$x, x$from, x$to, x$rep)
    setChildren(x, gList(xyListLine(pts, name=paste0(x$name, ".lines"))))
}

trimGrob <- function(x, from, to, rep=FALSE, name=NULL, gp=gpar(), ...) {
    gTree(x=x, from=from, to=to, rep=rep, trimArgs=list(...),
          gp=gp, name=name, cl="trimgrob")
}

grid.trim <- function(...) {
    grid.draw(trimGrob(...))
}
