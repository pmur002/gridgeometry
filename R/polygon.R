
## Functions for generating a polygon from a grob

grobPolygon <- function(x, closed, ...) {
    UseMethod("grobPolygon")
}

grobPoints <- function(x, closed, ...) {
    UseMethod("grobPoints")
}

grobPolygon.grob <- function(x, closed, ...) {
    ## Does this grob change the viewport
    vpgrob <- !is.null(x$vp)
    if (vpgrob) {
        trans <- current.transform()
    }
    ## Enforce 'gp' and 'vp'
    x <- preDraw(x)
    ## Polygon outline in inches
    pts <- grobPoints(x, closed, ...)
    if (vpgrob) {
        ## Calc locations on device
        pts <- deviceLoc(unit(pts$x, "in"), unit(pts$y, "in"), valueOnly=TRUE)
    }
    postDraw(x)
    if (vpgrob) {
        ## Transform back to locations
        ptsMatrix <- cbind(pts$x, pts$y, 1) %*% solve(trans)
        pts <- list(x=ptsMatrix[,1], y=ptsMatrix[,2])
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
        xx <- cx + r*cos(t)
        yy <- cy + r*sin(t)
        list(x=xx, y=yy)
    } else {
        NULL
    }
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
        list(x=c(left, left, right, right),
             y=c(bottom, top, top, bottom))
    } else {
        NULL
    }
}

grobPoints.xspline <- function(x, closed, ...) {
    if ((closed && !x$open) ||
        (!closed && x$open)) {
        trace <- xsplinePoints(x)
        list(x=convertX(trace$x, "in", valueOnly=TRUE),
             y=convertY(trace$y, "in", valueOnly=TRUE))
    } else {
        NULL
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
