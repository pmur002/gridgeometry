
library(gridGeometry)

## Two rectangles
r1 <- rectGrob(.4, .4, .4, .4, gp=gpar(col="red", lwd=5))
r2 <- rectGrob(.6, .6, .4, .4, gp=gpar(col="blue", lwd=5))

## Intersect the two
p <- polyclip(r1, r2, name="p",
              gp=gpar(col=NA, fill=rgb(0,0,0,.5)))

grid.newpage()
grid.draw(r1)
grid.draw(r2)
grid.draw(p)

## Merge the two
p <- polyclip(r1, r2, op="union",
              gp=gpar(col=NA, fill=rgb(0,0,0,.5)))

grid.newpage()
grid.draw(r1)
grid.draw(r2)
grid.draw(p)

## Take a chunk out of one with the other
p <- polyclip(r1, r2, op="minus",
              gp=gpar(col=NA, fill=rgb(0,0,0,.5)))

grid.newpage()
grid.draw(r1)
grid.draw(r2)
grid.draw(p)

## Two circles
c1 <- circleGrob(.4, .5, r=.2, gp=gpar(col="red", lwd=5))
c2 <- circleGrob(.6, .5, r=.2, gp=gpar(col="blue", lwd=5))
p <- polyclip(c1, c2, name="p", op="minus",
              gp=gpar(col=NA, fill=rgb(0,0,0,.5)))
grid.newpage()
grid.draw(c1)
grid.draw(c2)
grid.draw(p)

## gList (as at least one argument)
g1 <- rectGrob(.5, .4, .4, .4, gp=gpar(col="red"))
g2 <- gList(circleGrob(.7, .6, r=.2, gp=gpar(col="blue")),
            xsplineGrob(c(.2, .2, .4, .4),
                        c(.4, .8, .8, .4),
                        shape=1, open=FALSE, gp=gpar(col="blue")))
p <- polyclip(g1, g2, name="p", op="minus",
              gp=gpar(col=NA, fill=rgb(0,0,0,.5)))
grid.newpage()
grid.draw(g1)
grid.draw(g2)
grid.draw(p)

## Line between two circles
c1 <- circleGrob(.2, .5, r=unit(.1, "npc"), gp=gpar(col="red"))
c2 <- circleGrob(.8, .5, r=unit(.1, "npc"), gp=gpar(col="red"))
l <- xsplineGrob(c(.2, .5, .8), c(.5, .3, .5), shape=1,
                 gp=gpar(col="blue"))
p <- polyclip(l, gList(c1, c2), name="p", op="minus",
              gp=gpar(col=rgb(0,0,0,.5), lwd=5))
grid.newpage()
grid.draw(c1)
grid.draw(c2)
grid.draw(l)
grid.draw(p)
