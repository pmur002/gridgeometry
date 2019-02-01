
## Make 'grid' S3 classes (including class hierarchy) visible to S4
setOldClass("grob")
setOldClass(c("rect", "grob"))
setOldClass(c("circle", "grob"))
setOldClass(c("xspline", "grob"))
setOldClass("gList")

## Combine "grob"s and "gList"s so can have one method for both
setClassUnion("gridgrob", c("grob", "gList"))

## Convert 'polyclip' polygon results to 'grid' path
polyclipPath <- function(x, name, gp) {
    if (length(x)) {
        xx <- unlist(lapply(x, "[[", "x"))
        yy <- unlist(lapply(x, "[[", "y"))
        lengths <- sapply(x, function(y) length(y$x))
        pathGrob(xx, yy, default.units="in",
                 id.lengths=lengths,
                 name=name, gp=gp)
    } else {
        nullGrob(name=name)
    }
}

polyclipLine <- function(x, name, gp) {
    if (length(x)) {
        xx <- unlist(lapply(x, "[[", "x"))
        yy <- unlist(lapply(x, "[[", "y"))
        lengths <- sapply(x, function(y) length(y$x))
        polylineGrob(xx, yy, default.units="in",
                     id.lengths=lengths,
                     name=name, gp=gp)
    } else {
        nullGrob(name=name)
    }
}

## Create S4 generic from polyclip::polyclip()
setGeneric("polyclip",
           function(A, B, ...) {
               polyclip::polyclip(A, B, ...)
           })

## Grob methods
setMethod("polyclip",
          c("gridgrob", "gridgrob"),
          function(A, B, name=NULL, gp=gpar(), ...) {
              closed <- grobClosed(A)
              result <- polyclip::polyclip(grobPolygon(A),
                                           grobPolygon(B),
                                           closed=closed,
                                           ...)
              if (closed)
                  polyclipPath(result, name, gp)
              else
                  polyclipLine(result, name, gp)
          })


