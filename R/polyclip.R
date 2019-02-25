
## Create S3 generic from polyclip::polyclip()
polyclip <- function(A, B, ...) {
    UseMethod("polyclip")
}

polyclip.default <- function(A, B, ...) {
    polyclip::polyclip(A, B, ...)
}

polyclipGridGrob <- function(A, B, op, closed, ...) {
    if (inherits(B, "gPath") || is.character(B)) {
        B <- grid.get(B, ...)
    }
    if (!(inherits(B, "grob") || inherits(B, "gList")))
        stop("Argument 'B' must be a grob")
    polyA <- grobCoords(A, closed=closed)
    polyB <- grobCoords(B, TRUE)
    polyclip::polyclip(polyA, polyB, op=op, closed=closed, ...)
}

polyclip.grob <- function(A, B, op="intersection", closed=TRUE, ...) {
    polyclipGridGrob(A, B, op, closed, ...)
}

polyclip.gList <- function(A, B, op="intersection", closed=TRUE, ...) {
    polyclipGridGrob(A, B, op, closed, ...)
}

polyclip.gPath <- function(A, B, op="intersection", closed=TRUE,
                           strict=FALSE, grep=FALSE, global=FALSE, ...) {
    A <- grid.get(A, strict, grep, global)
    polyclipGridGrob(A, B, op, closed, strict, grep, global, ...)
}

polyclip.character <- function(A, B, op="intersection", closed=TRUE,
                           strict=FALSE, grep=FALSE, global=FALSE, ...) {
    A <- grid.get(A, strict, grep, global)
    polyclipGridGrob(A, B, op, closed, strict, grep, global, ...)
}




