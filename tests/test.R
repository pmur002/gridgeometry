
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

################################################################################
## fillA and fillB

## Default fillB is "nonzero"
## Path is TWO shapes (each with rule "winding", which is irrelevant)
## Outer clockwise, inner anticlockwise
## Path is flattened to two sets of coordinates
## Inner rect creates hole in outer rect
## (because default fillB of NULL defaults to "winding"/"nonzero")
## (and [flattened] path has no rule)
grid.newpage()
circle <- circleGrob(r=.4)
rects <- pathGrob(c(.3, .3, .7, .7, .6, .6, .4, .4),
                  c(.3, .7, .7, .3, .4, .6, .6, .4),
                  pathId=rep(1:2, each=4))
grid.polyclip(circle, rects, "minus", reduceB = "flatten",
              gp=gpar(fill="grey"))

## Take fillB from grob (path)
## Path is ONE shape with two parts and rule "evenodd"
## Outer clockwise, inner clockwise
## Path generates two sets of coordinates
## Inner rect creates hole in outer rect
## (because path rule is "evenodd")
grid.newpage()
circle <- circleGrob(r=.4)
rects <- pathGrob(c(.3, .3, .7, .7, .4, .4, .6, .6),
                  c(.3, .7, .7, .3, .4, .6, .6, .4),
                  id=rep(1:2, each=4),
                  rule="evenodd")
grid.polyclip(circle, rects, "minus", 
              gp=gpar(fill="grey"))

## Take fillB from grob (path)
## Path is ONE shape with two parts and rule "winding"
## Outer clockwise, inner clockwise
## Path generates two sets of coordinates
## Inner rect DOES NOT create hole in outer rect
## (because path rule is "winding"/"evenodd")
grid.newpage()
circle <- circleGrob(r=.4)
rects <- pathGrob(c(.3, .3, .7, .7, .4, .4, .6, .6),
                  c(.3, .7, .7, .3, .4, .6, .6, .4),
                  id=rep(1:2, each=4))
grid.polyclip(circle, rects, "minus", 
              gp=gpar(fill="grey"))

## Override rule from grob (path)
## Path is ONE shape with two parts and rule "evenodd"
## Outer clockwise, inner clockwise
## Path generates two sets of coordinates
## Inner rect DOES NOT create hole in outer rect
## (because fillB rule of "winding"/"evenodd" overrules path rule of "evenodd")
grid.newpage()
circle <- circleGrob(r=.4)
rects <- pathGrob(c(.3, .3, .7, .7, .4, .4, .6, .6),
                  c(.3, .7, .7, .3, .4, .6, .6, .4),
                  id=rep(1:2, each=4),
                  rule="evenodd")
grid.polyclip(circle, rects, "minus", fillB = "nonzero",
              gp=gpar(fill="grey"))

################################################################################
## trim()

library(gridGeometry)

l <- bezierGrob(c(.2, .4, .6, .8),
                c(.2, .8, .8, .2),
                gp=gpar(col="grey", lwd=5))

## trim single line, single result
grid.newpage()
grid.draw(l)
grid.trim(l, .2, .8, gp=gpar(lwd=5))

## trim single line (negative 'to'), single result
grid.newpage()
grid.draw(l)
grid.trim(l, .2, -.1, gp=gpar(lwd=5))

## trim single line (unit from/to), single result
grid.newpage()
grid.draw(l)
grid.trim(l, unit(5, "mm"), unit(-1, "in"), gp=gpar(lwd=5))

## trim single line ("npc" 'to'), single result
grid.newpage()
grid.draw(l)
grid.trim(l, .2, unit(.5, "npc") + unit(5, "mm"), gp=gpar(lwd=5))

## trim single line (vector from/to), multiple results
grid.newpage()
grid.draw(l)
grid.trim(l, c(.2, .6), c(.4, .8), gp=gpar(lwd=5))

## trim single line (rep=TRUE), multiple results
grid.newpage()
grid.draw(l)
grid.trim(l, .1, .2, rep=TRUE, gp=gpar(lwd=5))

## raw trim() (instead of grid.trim())
grid.newpage()
xg <- xsplineGrob(c(.2, .5, .8), c(.2, .8, .2), shape=-1)
xcoords <- trim(xg, from=0:1/2, to=1:2/2)
grid.lines(xcoords[[1]]$x, xcoords[[1]]$y, default.units="in",
           gp=gpar(lwd=3, lineend="butt"))
grid.lines(xcoords[[2]]$x, xcoords[[2]]$y, default.units="in",
           gp=gpar(lwd=10, lineend="butt"))

## trim grob with multiple shapes
l <- segmentsGrob(c(.2, .4), .2, c(.6, .8), .8,
                  gp=gpar(lwd=5, col="grey"))
grid.newpage()
grid.draw(l)
grid.trim(l, .2, .8, gp=gpar(lwd=5))

## trim gTree
gt <- gTree(children=gList(l,
                           circleGrob(),
                           gTree(children=gList(linesGrob(c(.2, .2, .4),
                                                          c(.6, .8, .8)),
                                                linesGrob(c(.6, .8, .8),
                                                          c(.2, .2, .4))))),
            gp=gpar(lwd=5, col="grey"))
grid.newpage()
grid.draw(gt)
grid.trim(gt, .2, .8, gp=gpar(lwd=5))



library(gridGeometry)


## grid.reduce
## (three circles become one union'ed shape)
grob <- circleGrob(1:3/4, r=.2)
grid.newpage()
grid.reduce(grob, gp=gpar(fill="grey"))

## grid.reduce
## (three circles become one xor'ed shape)
grid.newpage()
grid.reduce(grob, op="xor", gp=gpar(fill="grey"))

## grid.reduce
## (path with hole remains path with hole because fill rule is retained)
path <- pathGrob(c(.3, .3, .7, .7, .4, .4, .6, .6),
                 c(.3, .7, .7, .3, .4, .6, .6, .4),
                 id.lengths=c(4, 4),
                 rule="evenodd")
grid.newpage()
grid.reduce(path, gp=gpar(fill="grey"))

## grid.reduce
## (gTree with path with hole and rect, still has hole because fill
##  rule on path is retained)
rect <- rectGrob(0, 1, just=c("left", "top"), .5, .5)
gt <- gTree(children=gList(path, rect))
grid.newpage()
grid.reduce(gt, gp=gpar(fill="grey"))

