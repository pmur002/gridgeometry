#polylineoffset tests
library(gridGeometry)

#Prepare the input in different type: grob, list
grobLine = linesGrob(x = c(.4, .8, .8, .2, .6), y = c(.3, .3, .8, .8, .6), name = "Line 1")
grobLine2 = linesGrob(x = c(.6, 0.4), y = c(.6, 0.6), name = "Line 2")
grobPathLine = gPath("Line 1")
grobListLine = gList(grobLine, grobLine2)
xyListLine <- xyListFromGrob(grobLine, closed = F)



## Test polylineoffsetGrob
#Test PolylineoffsetGrob.Grob
grid.newpage()
offset <- polylineoffsetGrob(grobLine, delta = unit(0.1, "cm"), jointype="square", endtype = "opensquare")
grid.draw(offset)

#Test PolylineoffsetGrob.list
grid.newpage()
offset <- polylineoffsetGrob(xyListLine, delta = unit(0.1, "cm"), jointype="square", endtype = "opensquare")
grid.draw(offset)

#Test polylineoffsetGrob.character
grid.newpage()
grid.draw(grobLine)
offset <- polylineoffsetGrob("Line 1", delta = unit(0.1, "cm"), jointype="square", endtype = "opensquare")
#grid.draw(xyListPolygon(offset))
grid.draw(offset)

#Test PolylineoffsetGrob.gPath
grid.newpage()
grid.draw(grobLine)
offset <- polylineoffsetGrob(grobPathLine, delta = unit(0.1, "cm"), jointype="square", endtype = "opensquare")
grid.draw(offset)

#Test PolylineoffsetGrob.gList
grid.newpage()
grid.draw(grobLine)
grid.draw(grobLine2)
offset <- polylineoffsetGrob(grobListLine, delta = unit(0.1, "cm"), jointype="square", endtype = "opensquare")
grid.draw(offset)



##Test polylineoffset method
#Test Polylineoffset.Grob
grid.newpage()
offset <- polylineoffset(grobLine, delta = unit(0.1, "cm"), jointype="square", endtype = "opensquare")
grid.draw(xyListPolygon(offset))

#Test PolylineoffsetGrob.list
grid.newpage()
offset <- polylineoffset(xyListLine, delta = unit(0.1, "cm"), jointype="square", endtype = "opensquare")
grid.draw(xyListPolygon(offset))

#Test polylineoffsetGrob.character
grid.newpage()
grid.draw(grobLine)
offset <- polylineoffset("Line 1", delta = unit(0.1, "cm"), jointype="square", endtype = "opensquare")
grid.draw(xyListPolygon(offset))

#Test PolylineoffsetGrob.gPath
grid.newpage()
grid.draw(grobLine)
offset <- polylineoffset(grobPathLine, delta = unit(0.1, "cm"), jointype="square", endtype = "opensquare")
grid.draw(xyListPolygon(offset))

#Test PolylineoffsetGrob.gList
grid.newpage()
grid.draw(grobLine)
grid.draw(grobLine2)
offset <- polylineoffset(grobListLine, delta = unit(0.1, "cm"), jointype="square", endtype = "opensquare")
grid.draw(xyListPolygon(offset))



##Test grid.polylineoffset
#Test grid.polylineoffset.grob
grid.newpage()
grid.polylineoffset(grobLine, delta = unit(0.1, "cm"), jointype="square", endtype = "opensquare")

#Test grid.polylineoffset.list (Should be an error)
#grid.newpage()
#grid.polylineoffset(xyListLine, delta = unit(0.1, "cm"), jointype="square", endtype = "opensquare")

#Test grid.polylineoffset.character
grid.newpage()
grid.draw(grobLine)
grid.polylineoffset("Line 1", delta = unit(0.1, "cm"), jointype="square", endtype = "opensquare")

#Test grid.polylineoffset.gPath
grid.newpage()
grid.draw(grobLine)
grid.polylineoffset(grobPathLine, delta = unit(0.1, "cm"), jointype="square", endtype = "opensquare")

#Test grid.polylineoffset.gList
grid.newpage()
grid.polylineoffset(grobListLine, delta = unit(0.1, "cm"), jointype="square", endtype = "opensquare")
