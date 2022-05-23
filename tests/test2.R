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

#Test low level polylineoffset function
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

#Test low level polyoffset function
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

#Test low level polylineoffset.character function
grid.newpage()
grob <- linesGrob(x = c(.4, .8, .8, .2, .6), y = c(.3, .3, .8, .8, .6), name = "Line 1")
grid.draw(grob)
offset <- polylineoffset("Line 1", delta = unit(0.1, "cm"), jointype="square", endtype = "opensquare")
grid.draw(xyListPolygon(offset))
grid.draw(grob)

#Test low level polyoffset.character function
grid.newpage()
grob <- rectGrob(width = 0.5, height = 0.5, name = "Rect 1")
grid.draw(grob)
offset <- polyoffset("Rect 1", delta = unit(0.1, "cm"), jointype="square", endtype = "opensquare")
grid.draw(xyListPolygon(offset))
grid.draw(grob)

#Create donut shape
grid.newpage();
grob <- circleGrob(r = 0.25, gp = gpar(col = "blue", lwd = 3))
offset <- polyoffsetGrob(grob, delta = 0.1, gp = gpar(fill = "blue"))
grid.draw(offset)
grid.draw(grob)

#Create donut shape using polyline offset instead
grid.newpage();
grob <- circleGrob(r = 0.25, gp = gpar(col = "blue", lwd = 3))
offset <- polylineoffsetGrob(xyListLine(grobCoords(grob, closed = T)), delta = 0.1, jointype="round", endtype = "openround", gp = gpar(col = "red", lwd = 3, fill = "blue"), rule = "winding")
grid.draw(offset)

#Test low level polyoffset.gPath
grid.newpage()
grob <- rectGrob(width = 0.5, height = 0.5, name = "Rect 1")
grid.draw(grob)
offset <- polyoffsetGrob(gPath("Rect 1"), delta = unit(0.1, "cm"), jointype="square", endtype = "opensquare")
grid.draw(offset)

#Test low level polylineoffset.gPath
grid.newpage()
grob <- linesGrob(x = c(.4, .8, .8, .2, .6), y = c(.3, .3, .8, .8, .6), name = "Line 1")
grid.draw(grob)
offset <- polylineoffset("Line 1", delta = unit(0.1, "cm"), jointype="square", endtype = "opensquare")
grid.draw(xyListPolygon(offset))
grid.draw(grob)

#Test low level polylineoffset.gList
grid.newpage()
grob1 <- linesGrob(x = c(.4, .8, .8, .2, .6), y = c(.3, .3, .8, .8, .6), name = "Line 1")
grob2 <- linesGrob(x = c(.6, 0.4), y = c(.6, 0.6), name = "Line 2")
grid.draw(gList(grob1, grob2))
offset <- polylineoffset(gList(grob1, grob2), delta = unit(0.1, "cm"), jointype="square", endtype = "opensquare")
grid.draw(xyListPolygon(offset))

#Test low level polyoffset.gList (The circle did not increase delta?)
grid.newpage()
grobRect <- rectGrob(width = 0.5, height = 0.5, name = "Rect 1")
grobCircle <- circleGrob(r = 0.25, gp = gpar(col = "blue"))
grid.draw(gList(grobRect, grobCircle))
offset <- polyoffset(gList(grobCircle, grobRect), delta = unit(0.3, "cm"), jointype="square", endtype = "opensquare")
grid.draw(xyListPolygon(offset))
