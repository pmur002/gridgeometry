
## Take an open shape and "trim" it

trim <- function(x, from, to, ...) {
    UseMethod("trim")
}

## Designed for polyclip() results: list of list(x,y)
trim.default <- function(x, from, to, rep=FALSE, ...) {
    if ("x" %in% names(x))
        x <- list(x)
    unlist(lapply(x, trimLine, from, to, rep), recursive=FALSE)
}
    
trim.GridGrobCoords <- function(x, from, to, rep=FALSE, ...) {
    ## Each trimLine() result is a list of lists
    unlist(lapply(x, trimLine, from, to, rep), recursive=FALSE)
}

trim.GridGTreeCoords <- function(x, from, to, rep=FALSE, ...) {
    childCoords <- lapply(x, trim, from, to, rep=FALSE, ...)
    do.call(c, childCoords)
}

trim.grob <- function(x, from, to, rep=FALSE, ...) {
    pts <- grobCoords(x, closed=FALSE)
    trim(pts, from, to, rep, ...)
}

trim.gPath <- function(x, from, to, rep=FALSE,
                       strict=FALSE, grep=FALSE, global=FALSE, ...) {
    trim(grid.get(x, strict, grep, global),
         from, to, rep, ...)
}

trim.character <- function(x, from, to, rep=FALSE,
                           strict=FALSE, grep=FALSE, global=FALSE, ...) {
    trim(grid.get(x, strict, grep, global),
         from, to, rep, ...)
}

trimLine <- function(line, from, to, rep) {
    x <- line$x
    y <- line$y
    dx <- diff(x)
    dy <- diff(y)
    length <- sqrt(dx^2 + dy^2)
    cumLength <- c(0, cumsum(length))
    totLength <- sum(length)
    ## Reduce units to proportions
    if (is.unit(from) || is.unit(to)) {
        ## Temporary graphics device (!)
        ## so "npc" units relative to length of line
        cd <- dev.cur()
        pdf(NULL, width=totLength)
        on.exit(dev.set(cd))
    }
    if (is.unit(from))
        from <- convertX(from, "in", valueOnly=TRUE)/totLength
    else
        from <- as.numeric(from)
    if (is.unit(to))
        to <- convertX(to, "in", valueOnly=TRUE)/totLength
    else
        to <- as.numeric(to)
    n <- max(length(from), length(to))
    lf <- length(from)
    lt <- length(to)
    if (lf < lt)
        from <- rep(from, length.out=lt)
    else if (lt < lf)
        to <- rep(to, length.out=lf)
    from[from < 0] <- 1 + from[from < 0]
    to[to < 0] <- 1 + to[to < 0]
    if (any(from > 1)) {
        from[from > 1] <- 1
        warning("'from' value(s) larger than 1 reduced to 1")
    }
    if (any(to > 1)) {
        to[to > 1] <- 1
        warning("'to' value(s) larger than 1 reduced to 1")
    }
    reverse <- from > to
    if (any(reverse)) {
        temp <- from[reverse]
        from[reverse] <- to[reverse]
        to[reverse] <- temp
    }
    if (rep) {
        shift <- seq(0, 1, max(to))
        from <- as.numeric(outer(from, shift, "+"))
        to <- as.numeric(outer(to, shift, "+"))
        ## Trim 'from' to just those within 0 to 1
        keep <- from <= 1
        from <- from[keep]
        ## Trim 'to' to same length as 'from' but cap at 1
        to <- pmin(to[keep], 1)
    }
    fromLength <- from*totLength
    toLength <- to*totLength
    fromSeg <- apply(outer(fromLength, cumLength, "<="), 1,
                     function(x) min(which(x))) - 1
    fromSeg[fromSeg == 0] <- 1
    toSeg <- apply(outer(toLength, cumLength, "<="), 1,
                   function(x) min(which(x))) - 1
    toSeg[toSeg == 0] <- 1
    startx <- x[fromSeg] +
        (fromLength - cumLength[fromSeg])/length[fromSeg]*dx[fromSeg]
    starty <- y[fromSeg] +
        (fromLength - cumLength[fromSeg])/length[fromSeg]*dy[fromSeg]
    endx <- x[toSeg] + 
        (toLength - cumLength[toSeg])/length[toSeg]*dx[toSeg]
    endy <- y[toSeg] + 
        (toLength - cumLength[toSeg])/length[toSeg]*dy[toSeg]
    midIndex <- mapply(
        function(fs, ts) {
            if (ts - fs > 0) {
                (fs + 1):ts
            } else {
                NULL
            }
        }, fromSeg, toSeg, SIMPLIFY=FALSE)
    mapply(
        function(sx, sy, mi, ex, ey) {
            list(x=c(sx, x[mi], ex),
                 y=c(sy, y[mi], ey))
        },
        startx, starty, midIndex, endx, endy,
        SIMPLIFY=FALSE) 
}
