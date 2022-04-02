library(gridGeometry, lib.loc = "D:/Stats781/library")
x = list()
emptyGrob1 = xyListPath(x, name = 1)
stopifnot(identical(nullGrob(name = "1"), emptyGrob1))

emptyGrob2 = xyListLine(x, name = 2)
stopifnot(identical(nullGrob(name = "2"), emptyGrob2))

emptyGrob3 = xyListPolygon(x, name = 3)
stopifnot(identical(nullGrob(name = "3"), emptyGrob3))

## test for grid.polylineoffset()
grid.newpage()
linePath = list(list(x = c(2, 4, 4, 2), y = c(2, 2, 4, 4) - 1.5))
grobPath <- xyListLine(linePath)
grid.polylineoffset(grobPath,0.1, jointype="square", endtype = "opensquare") # Not working because it is a path. Any solution?
grid.draw(grobPath)

grid.newpage()
grob <- polylineGrob(x = c(2, 4, 4, 2), y = c(0.5, 0.5, 2.5, 2.5), default.units = "in")
offsetGrob <- grid.polylineoffset(grob, 0.1, jointype="square", endtype = "opensquare")
grid.draw(grob)

grid.newpage()
grob <- linesGrob(x = c(.4, .8, .8, .2, .6), y = c(.3, .3, .8, .8, .6))
grid.polylineoffset(grob, 0.1, jointype="square", endtype = "opensquare")
grid.draw(grob)

##Pass line grob to polyOffset. Expect null grob obj.
offsetGrob <- grid.polyoffset(grob, 0.1, jointype = "square", endtype = "opensquare")

## test for grid.polyoffset()
grid.newpage()
grob <- polygonGrob(x = c(.4, .8, .8, .2, .6), y = c(.3, .3, .8, .8, .6))
grid.polyoffset(grob, 0.25, jointype = "round")
grid.draw(grob)
