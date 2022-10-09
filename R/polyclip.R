
convertRule <- function(rule, default = "nonzero") {
    if (is.null(rule)) {
        if (is.null(default)) {
            "nonzero"
        } else {
            if (default == "winding") {
                "nonzero"
            } else {
                default
            }
        }
    } else {
        if (rule == "winding") {
            "nonzero"
        } else {
            rule
        }
    }
}

## Create S3 generic from polyclip::polyclip()
polyclip <- function(A, B, ...) {
    UseMethod("polyclip")
}

polyclip.default <- function(A, B, ...) {
    polyclip::polyclip(A, B, ...)
}

polyclipGridGrob <- function(A, B, op, closed, reduceA, reduceB, fillA, fillB,
                             ...) {
    if (inherits(B, "gPath") || is.character(B)) {
        B <- grid.get(B, ...)
    }
    if (!(inherits(B, "grob") || inherits(B, "gList")))
        stop("Argument 'B' must be a grob")
    polyA <- xyListFromGrob(A, op = reduceA, closed = closed, ...)
    polyB <- xyListFromGrob(B, op = reduceB, closed = TRUE, ...)
    fillA <- convertRule(fillA, attr(polyA, "rule"))
    fillB <- convertRule(fillB, attr(polyB, "rule"))
    polyclip::polyclip(polyA, polyB, op=op, closed=closed,
                       fillA = fillA, fillB = fillB, ...)
}

polyclip.grob <- function(A, B, op="intersection", closed=isClosedShape(A),
                          reduceA = if (closed) "union" else "flatten",
                          reduceB = "union",
                          fillA = NULL, fillB = NULL,
                          ...) {
    polyclipGridGrob(A, B, op, closed, reduceA, reduceB, fillA, fillB, ...)
}

polyclip.gList <- function(A, B, op="intersection", closed=isClosedShape(A),
                           reduceA = if (closed) "union" else "flatten",
                           reduceB = "union",
                           fillA = NULL, fillB = NULL,
                           ...) {
    polyclipGridGrob(A, B, op, closed, reduceA, reduceB, fillA, fillB, ...)
}

polyclip.gPath <- function(A, B, op="intersection", closed,
                           strict=FALSE, grep=FALSE, global=FALSE,
                           reduceA = if (closed) "union" else "flatten",
                           reduceB = "union",
                           fillA = NULL, fillB = NULL,
                           ...) {
    A <- grid.get(A, strict, grep, global)
    if (missing(closed))
        closed <- isClosedShape(A)
    polyclipGridGrob(A, B, op, closed, reduceA, reduceB, fillA, fillB, ...)
}

polyclip.character <- function(A, B, op="intersection", closed,
                               strict=FALSE, grep=FALSE, global=FALSE,
                               reduceA = if (closed) "union" else "flatten",
                               reduceB = "union",
                               fillA = NULL, fillB = NULL,
                               ...) {
    A <- grid.get(A, strict, grep, global)
    if (missing(closed))
        closed <- isClosedShape(A)
    polyclipGridGrob(A, B, op, closed, reduceA, reduceB, fillA, fillB, ...)
}




