
## Create S3 generic from polyclip::polyclip()
polyclip <- function(A, B, ...) {
    UseMethod("polyclip")
}

polyclip.default <- function(A, B, ...) {
    polyclip::polyclip(A, B, ...)
}

polyclipGridGrob <- function(A, B, op, closed, ...) {
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



