
## Make 'grid' S3 classes (including class hierarchy) visible to S4
setOldClass("grob")
setOldClass(c("circle", "grob"))
setOldClass(c("lines", "grob"))
setOldClass(c("polygon", "grob"))
setOldClass(c("rect", "grob"))
setOldClass(c("roundrect", "grob"))
setOldClass(c("segments", "grob"))
setOldClass(c("text", "grob"))
setOldClass(c("xspline", "grob"))
setOldClass("gList")
setOldClass(c("gTree", "grob"))

## Combine "grob"s and "gList"s so can have one method for both
setClassUnion("gridgrob", c("grob", "gList"))

## Convert (closed) 'polyclip' polygon result to 'grid' path
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

## Convert (closed) 'polyclip' polygon result to 'grid' polygons
polyclipPolygon <- function(x, name, gp) {
    if (length(x)) {
        xx <- unlist(lapply(x, "[[", "x"))
        yy <- unlist(lapply(x, "[[", "y"))
        lengths <- sapply(x, function(y) length(y$x))
        polygonGrob(xx, yy, default.units="in",
                    id.lengths=lengths,
                    name=name, gp=gp)
    } else {
        nullGrob(name=name)
    }
}

## Convert (open) 'polyclip' polygon result to 'grid' polyline
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
          function(A, B, op="intersection", name=NULL, gp=gpar(),
                   closedGrob=polyclipPath, openGrob=polyclipLine,
                   ...) {
              closedPolys <- grobPolygon(A, closed=TRUE)
              openPolys <- grobPolygon(A, closed=FALSE)
              children <- vector("list", 2)
              if (!is.null(closedPolys)) {
                  closedPaths <- polyclip::polyclip(closedPolys,
                                                    grobPolygon(B, TRUE),
                                                    op=op,
                                                    closed=TRUE,
                                                    ...)
                  children[[1]] <- closedGrob(closedPaths,
                                              paste0(name, ".open"),
                                              gp)
              } 
              if (!is.null(openPolys)) {
                  openPaths <- polyclip::polyclip(openPolys,
                                                  grobPolygon(B, TRUE),
                                                  op=op,
                                                  closed=FALSE,
                                                  ...)
                  children[[2]] <- openGrob(openPaths, 
                                            paste0(name, ".closed"),
                                            gp)
              }
              gTree(children=do.call(gList, children[!is.null(children)]),
                    name=name)
          })


