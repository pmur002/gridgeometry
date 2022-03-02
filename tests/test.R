
library(gridGeometry)

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

## Two grobs, one has multiple shapes, control collapse
r <- rectGrob(.5, .5, .8, .4)
c <- circleGrob(c(.45, .55), .5, .1)
grid.newpage()
grid.polyclip(r, c, "minus", collapse="intersection", gp=gpar(fill="grey"))

## grob and gTree
r <- rectGrob(.5, .5, .8, .4)
c1 <- circleGrob(.45, .5, .1)
c2 <- circleGrob(.55, .5, .1)
gt <- gTree(children=gList(c1, c2))
grid.newpage()
grid.polyclip(r, gt, "minus", gp=gpar(fill="grey"))
