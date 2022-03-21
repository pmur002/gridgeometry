library(gridGeometry, lib.loc = "D:/Stats781/library")
x = list()
emptyGrob1 = xyListPath(x, name = 1)
stopifnot(identical(nullGrob(name = "1"), emptyGrob1))

emptyGrob2 = xyListLine(x, name = 2)
stopifnot(identical(nullGrob(name = "2"), emptyGrob2))

emptyGrob3 = xyListPolygon(x, name = 3)
stopifnot(identical(nullGrob(name = "3"), emptyGrob3))