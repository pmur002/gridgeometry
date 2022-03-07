
library(gridGeometry)

################################################################################
## Closed shapes

## Two grobs, one shape each
r <- rectGrob(.3, .5, .2, .2)
c <- circleGrob(.45, .5, .1)
grid.newpage()
grid.polyclip(r, c, "minus")

## Two grobs, one shape each, null result
r <- rectGrob(.3, .5, .2, .2)
c <- circleGrob(.6, .5, .1)
grid.newpage()
grid.polyclip(r, c)

## Two grobs, one has multiple shapes
r <- rectGrob(.5, .5, .8, .4)
c <- circleGrob(c(.45, .55), .5, .1)
grid.newpage()
grid.polyclip(r, c, "minus", gp=gpar(fill="grey"))

## Two grobs, one has multiple shapes, control reduce
r <- rectGrob(.5, .5, .8, .4)
c <- circleGrob(c(.45, .55), .5, .1)
grid.newpage()
grid.polyclip(r, c, "minus", reduceB="intersection", gp=gpar(fill="grey"))

## grob and gTree
r <- rectGrob(.5, .5, .8, .4)
c1 <- circleGrob(.45, .5, .1)
c2 <- circleGrob(.55, .5, .1)
gt <- gTree(children=gList(c1, c2))
grid.newpage()
grid.polyclip(r, gt, "minus", gp=gpar(fill="grey"))

################################################################################
## Mixture of open and closed

## One open, one closed
line <- segmentsGrob(gp=gpar(col="grey"))
circle <- circleGrob(r = .3, gp=gpar(col="grey", fill = NA))
grid.newpage()
grid.draw(circle)
grid.draw(line)
grid.polyclip(line, circle, "minus", gp=gpar(lwd=5))

## Two open, one closed (reduceA defaults to "flatten" for !closed A)
lines <- segmentsGrob(0:1, 0, 1:0, 1, gp=gpar(col="grey"))
circle <- circleGrob(r = .3, gp=gpar(col="grey", fill = NA))
grid.newpage()
grid.draw(circle)
grid.draw(lines)
grid.polyclip(lines, circle, "minus", gp=gpar(lwd=5))
