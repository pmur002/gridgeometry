#polyoffset tests
library(gridGeometry)

#Prepare the input in different type: grob, list
grobPolygon <- polygonGrob(x = c(.5, .8, .8, .5, .6),
                           y = c(.3, .3, .8, .8, .55),
                           name = "Polygon 1")
grobCircle <- circleGrob(0.25, r = 0.25, name = "Circle 1")
grobPathPolygon <- gPath("Polygon 1")
grobListPolygon <- gList(grobPolygon, grobCircle)
xyListPolygon <- xyListFromGrob(grobPolygon, closed = T)



## Test polyoffsetGrob
#Test PolyoffsetGrob.Grob
grid.newpage()
offset <- polyoffsetGrob(grobPolygon, delta = unit(0.5, "cm"),
                         jointype="square")
grid.draw(offset)

#Test PolyoffsetGrob.list
grid.newpage()
offset <- polyoffsetGrob(xyListPolygon, delta = unit(0.5, "cm"),
                         jointype="square")
grid.draw(offset)

#Test PolyoffsetGrob.character
grid.newpage()
grid.draw(grobPolygon)
offset <- polyoffsetGrob("Polygon 1", delta = unit(0.5, "cm"),
                         jointype="square")
grid.draw(offset)

#Test polyoffsetGrob.gPath
grid.newpage()
grid.draw(grobPolygon)
offset <- polyoffsetGrob(grobPathPolygon, delta = unit(0.5, "cm"),
                         jointype="square")
grid.draw(offset)

#Test polyoffsetGrob.gList
grid.newpage()
offset <- polyoffsetGrob(grobListPolygon, delta = unit(0.5, "cm"),
                         reduce = "union", jointype="square")
grid.draw(offset)



##Test polyoffset method
#Test Polyoffset.Grob
grid.newpage()
offset <- polyoffset(grobPolygon, delta = unit(0.5, "cm"), jointype="square")
grid.draw(xyListPolygon(offset))

#Test Polyoffset.list
grid.newpage()
offset <- polyoffset(xyListPolygon, delta = unit(0.5, "cm"), jointype="square")
grid.draw(xyListPolygon(offset))

#Test Polyoffset.character
grid.newpage()
grid.draw(grobPolygon)
offset <- polyoffset("Polygon 1", delta = unit(0.5, "cm"), jointype="square")
grid.draw(xyListPolygon(offset))

#Test Polyoffset.gPath
grid.newpage()
grid.draw(grobPolygon)
offset <- polyoffset(grobPathPolygon, delta = unit(0.5, "cm"),
                     jointype="square")
grid.draw(xyListPolygon(offset))

#Test Polyoffset.gList
grid.newpage()
offset <- polyoffset(grobListPolygon, delta = unit(0.5, "cm"),
                     reduce = "union", jointype="square")
grid.draw(xyListPolygon(offset))



##Test grid.polyoffset
#Test grid.polyoffset.grob
grid.newpage()
grid.polyoffset(grobPolygon, delta = unit(0.5, "cm"), jointype="square")

#Test grid.polyoffset.list (should be error)
#grid.newpage()
#grid.polyoffset(xyListPolygon, delta = unit(0.5, "cm"), jointype="square")

#Test grid.polyoffset.character
grid.newpage()
grid.draw(grobPolygon)
grid.polyoffset("Polygon 1", delta = unit(0.5, "cm"), jointype="square")

#Test grid.polyoffset.gPath
grid.newpage()
grid.draw(grobPolygon)
grid.polyoffset(grobPathPolygon, delta = unit(0.5, "cm"), jointype="square")

#Test grid.polyoffset.gList
grid.newpage()
grid.polyoffset(grobListPolygon, delta = unit(0.5, "cm"),
                reduce = "union", jointype="square")

