library(gridGeometry, lib.loc = "D:/Stats781/library")
x = list()
emptyGrob1 = xyListPath(x, name = 1)
stopifnot(identical(nullGrob(name = "1"), emptyGrob1))

emptyGrob2 = xyListLine(x, name = 2)
stopifnot(identical(nullGrob(name = "2"), emptyGrob2))

emptyGrob3 = xyListPolygon(x, name = 3)
stopifnot(identical(nullGrob(name = "3"), emptyGrob3))

## test for grid.polylineoffset()

linePath = list(list(x = c(2, 4, 4, 2), y = c(2, 2, 4, 4) - 1.5))
grobPath <- xyListPath(linePath)
grid.draw(grid.polylineoffset(grobPath,0.5)) # Not working because it is a path. Any solution?
grid.draw(grobPath)

grob <- linesGrob(x = c(.2, .4, .4, .2), y = c(.2, .2, .4, .4))
offsetGrob <- grid.polylineoffset(grob, 0.1, jointype="square", endtype = "opensquare")
grid.draw(offsetGrob)
grid.draw(grob)

grob <- linesGrob(x = c(.4, .8, .8, .2, .6), y = c(.3, .3, .8, .8, .6))
offsetGrob <- grid.polylineoffset(grob, 0.1, jointype="square", endtype = "opensquare")
grid.draw(offsetGrob)
grid.draw(grob)

## test for grid.polyoffset()
grob <- polygonGrob(x = c(.4, .8, .8, .2, .6), y = c(.3, .3, .8, .8, .6))
offsetGrob <- grid.polyoffset(grob, 0.25, jointype = "round")
grid.draw(offsetGrob)
grid.draw(grob)
