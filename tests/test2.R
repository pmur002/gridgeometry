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
grid.draw(polylineGrob(x = c(1.9, 4.1), y = c(2.5, 2.5), default.units = "in")) #Test the delta unit type. It is inch.

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

#Test the offset shift
grid.newpage()
grob <- polylineGrob(x = c(2, 4, 4, 2), y = c(0.5, 0.5, 2.5, 2.5), default.units = "in")
r <- polylineoffsetGrob(A = grob, delta = 0.1, jointype = "square", endtype = "opensquare")
pushViewport(viewport(x=.75, y=.75, width=.5, height=.5))
grid.draw(r)
popViewport()

#Test unit delta
grid.newpage()
grob <- polygonGrob(x = c(.4, .8, .8, .2, .6), y = c(.3, .3, .8, .8, .6))
grid.polyoffset(grob, unit(0.25, "cm"), jointype = "round")
grid.draw(grob)

grid.newpage()
grob <- linesGrob(x = c(.4, .8, .8, .2, .6), y = c(.3, .3, .8, .8, .6))
grid.polylineoffset(grob, unit(0.25, "inch"), jointype="square", endtype = "opensquare")
grid.draw(grob)

#Test generic polylineoffset
grid.newpage()
grob <- linesGrob(x = c(.4, .8, .8, .2, .6), y = c(.3, .3, .8, .8, .6))
offset <- polylineoffset(grob, 0.1, jointype="square", endtype = "opensquare")
grid.draw(xyListPolygon(offset))
grid.draw(grob)

grid.newpage()
grob <- linesGrob(x = c(.4, .8, .8, .2, .6), y = c(.3, .3, .8, .8, .6))
offset <- polylineoffset(grobCoords(grob, F), 0.1, jointype="square", endtype = "opensquare")
grid.draw(xyListPolygon(offset))
grid.draw(grob)

#Test generic polyoffset
grid.newpage()
grob <- rectGrob(width = 0.5, height = 0.5)
offset <- polyoffset(grob, 0.1, jointype="square", endtype = "opensquare")
grid.draw(xyListPolygon(offset))
grid.draw(grob)

grid.newpage()
grob <- rectGrob(width = 0.5, height = 0.5)
offset <- polyoffset(grobCoords(grob, T), unit(0.1, "inch"), jointype="square", endtype = "opensquare")
grid.draw(xyListPolygon(offset))
grid.draw(grob)
