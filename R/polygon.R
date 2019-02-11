
## Functions for generating a polygon from a grob

grobPolygon <- function(x, closed, ...) {
    UseMethod("grobPolygon")
}

grobPoints <- function(x, closed, ...) {
    UseMethod("grobPoints")
}

emptyPolygon <- list(x = 0, y = 0)

grobPolygon.grob <- function(x, closed, ...) {
    vp <- x$vp
    trans <- current.transform()
    ## Enforce 'gp' and 'vp'
    x <- preDraw(x)
    ## Does this grob change the viewport ?
    ## (including has preDraw() changed the viewport)
    vpgrob <- !is.null(x$vp) || !identical(vp, x$vp)
    ## Generate any draw-time content
    x <- makeContent(x)
    ## Polygon outline in inches
    pts <- grobPoints(x, closed, ...)
    if (vpgrob && !identical(pts, emptyPolygon)) {
        ## Calc locations on device
        pts <- lapply(pts,
                      function(p) {
                          deviceLoc(unit(p$x, "in"), unit(p$y, "in"),
                                    valueOnly=TRUE)
                      })
    }
    postDraw(x)
    if (vpgrob && !identical(pts, emptyPolygon)) {
        ## Transform back to locations
        pts <- lapply(pts,
                      function(p) {
                          ptsMatrix <- cbind(p$x, p$y, 1) %*% solve(trans)
                          list(x=ptsMatrix[,1], y=ptsMatrix[,2])
                      })
    }
    pts
}

grobPoints.circle <- function(x, closed, ..., n=100) {
    if (closed) {
        cx <- convertX(x$x, "in", valueOnly=TRUE)
        cy <- convertY(x$y, "in", valueOnly=TRUE)
        r <- min(convertWidth(x$r, "in", valueOnly=TRUE),
                 convertHeight(x$r, "in", valueOnly=TRUE))
        t <- seq(0, 2*pi, length.out=n+1)[-(n+1)]
        ## Recycle via cbind()
        circs <- cbind(cx, cy, r)
        n <- nrow(circs)
        lapply(1:n,
               function(i) {
                   list(x=circs[i, 1] + circs[i, 3]*cos(t),
                        y=circs[i, 2] + circs[i, 3]*sin(t))
               })
    } else {
        emptyPolygon
    }
}

grobPoints.lines <- function(x, closed, ..., n=100) {
    if (closed) {
        emptyPolygon
    } else {
        xx <- convertX(x$x, "in", valueOnly=TRUE)
        yy <- convertY(x$y, "in", valueOnly=TRUE)
        list(list(x=xx, y=yy))
    }
}

grobPoints.polygon <- function(x, closed, ...) {
    if (closed) {
        ## polygonGrob() ensures that x/y same length
        xx <- convertX(x$x, "in", valueOnly=TRUE)
        yy <- convertY(x$y, "in", valueOnly=TRUE)
        pts <- list(x=xx, y=yy)
        if (is.null(x$id) && is.null(x$id.lengths)) {
            list(pts)
        } else {
            if (is.null(x$id)) {
                n <- length(x$id.lengths)
                id <- rep(1L:n, x$id.lengths)
            } else {
                n <- length(unique(x$id))
                id <- x$id
            }
            if (n > 1) {
                split(as.data.frame(pts), id)
            } else {
                list(pts)
            }
        }
    } else {
        emptyPolygon
    }
}

xyListFromMatrix <- function(m, xcol, ycol) {
    n <- nrow(m)
    lapply(1:n,
           function(i) {
               list(x=m[i, xcol], y=m[i, ycol])
           })
}

grobPoints.rect <- function(x, closed, ...) {
    if (closed) {
        hjust <- resolveHJust(x$just, x$hjust)
        vjust <- resolveVJust(x$just, x$vjust)
        w <- convertWidth(x$width, "in", valueOnly=TRUE)
        h <- convertHeight(x$height, "in", valueOnly=TRUE)
        left <- convertX(x$x, "in", valueOnly=TRUE) - hjust*w
        bottom <- convertY(x$y, "in", valueOnly=TRUE) - vjust*h
        right <- left + w
        top <- bottom + h
        ## Recycle via cbind()
        rects <- cbind(left, right, bottom, top)
        xyListFromMatrix(rects, c(1, 1, 2, 2), c(3, 4, 4, 3))
    } else {
        emptyPolygon
    }
}

grobPoints.segments <- function(x, closed, ...) {
    if (closed) {
        emptyPolygon
    } else {
        x0 <- convertX(x$x0, "in", valueOnly=TRUE)
        x1 <- convertX(x$x1, "in", valueOnly=TRUE)
        y0 <- convertY(x$y0, "in", valueOnly=TRUE)
        y1 <- convertY(x$y1, "in", valueOnly=TRUE)
        ## Recycle via cbind()        
        xy <- cbind(x0, x1, y0, y1)
        xyListFromMatrix(xy, 1:2, 3:4)
    }
}

grobPoints.text <- function(x, closed, ...) {
    warning("Text grobs are ignored by polyclip()")
    emptyPolygon
}

grobPoints.xspline <- function(x, closed, ...) {
    if ((closed && !x$open) ||
        (!closed && x$open)) {
        ## xsplinePoints() takes care of multiple X-splines
        trace <- xsplinePoints(x)
        if ("x" %in% names(trace)) {
            ## Single X-spline
            list(list(x=as.numeric(trace$x),
                      y=as.numeric(trace$y)))
        } else {
            lapply(trace,
                   function(t) {
                       list(x=as.numeric(t$x), y=as.numeric(t$y))
                   })
        }
    } else {
        emptyPolygon
    }
}

## TODO
## More 'grid' primitives

## "gList"s
grobPolygon.gList <- function(x, closed, ...) {
    ## Some children may produce list of lists
    polys <- lapply(x, grobPolygon, closed, ...)
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
grobPolygon.gTree <- function(x, closed, ...) {
    vp <- x$vp
    trans <- current.transform()
    ## Enforce 'gp' and 'vp'
    x <- preDraw(x)
    ## Does this grob change the viewport ?
    ## (including has preDraw() changed the viewport)
    vpgrob <- !is.null(x$vp) || !identical(vp, x$vp)
    ## Generate any draw-time content
    x <- makeContent(x)
    ## Polygon outline in inches
    pts <- grobPolygon(x$children[x$childrenOrder], closed, ...)
    if (vpgrob && !identical(pts, emptyPolygon)) {
        ## Calc locations on device
        pts <- lapply(pts,
                      function(p) {
                          deviceLoc(unit(p$x, "in"), unit(p$y, "in"),
                                    valueOnly=TRUE)
                      })
    }
    postDraw(x)
    if (vpgrob && !identical(pts, emptyPolygon)) {
        ## Transform back to locations
        pts <- lapply(pts,
                      function(p) {
                          ptsMatrix <- cbind(p$x, p$y, 1) %*% solve(trans)
                          list(x=ptsMatrix[,1], y=ptsMatrix[,2])
                      })
    }
    pts
}
