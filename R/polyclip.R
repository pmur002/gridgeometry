
## Make 'grid' S3 classes (including class hierarchy) visible to S4
setOldClass("grob")
setOldClass(c("circle", "grob"))
setOldClass(c("polygon", "grob"))
setOldClass(c("rect", "grob"))
setOldClass(c("roundrect", "grob"))
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
              closedPolys <- grobPolygon(A, closed=TRUE)
              openPolys <- grobPolygon(A, closed=FALSE)
              children <- vector("list", 2)
              if (!is.null(closedPolys)) {
                  closedPaths <- polyclip::polyclip(closedPolys,
                                                    grobPolygon(B, TRUE),
                                                    closed=TRUE,
                                                    ...)
                  children[[1]] <- polyclipPath(closedPaths,
                                                paste0(name, ".open"),
                                                gp)
              } 
              if (!is.null(openPolys)) {
                  openPaths <- polyclip::polyclip(openPolys,
                                                  grobPolygon(B, TRUE),
                                                  closed=FALSE,
                                                  ...)
                  children[[2]] <- polyclipLine(openPaths, 
                                                paste0(name, ".closed"),
                                                gp)
              }
              gTree(children=do.call(gList, children[!is.null(children)]),
                    name=name)
          })


