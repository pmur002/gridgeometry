
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

## Create S4 generic from polyclip::polyclip()
setGeneric("polyclip",
           function(A, B, ...) {
               polyclip::polyclip(A, B, ...)
           })

## Grob methods
setMethod("polyclip",
          c("gridgrob", "gridgrob"),
          function(A, B, op="intersection", closed=TRUE, ...) {
              polyA <- grobPolygon(A, closed=closed)
              polyB <- grobPolygon(B, TRUE)
              polyclip::polyclip(polyA, polyB, op=op, closed=closed, ...)
          })


