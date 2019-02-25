
library(gridGeometry)

pdf("tests.pdf")

## Two rectangles
r1 <- rectGrob(.4, .4, .4, .4, gp=gpar(col="red", lwd=5))
r2 <- rectGrob(.6, .6, .4, .4, gp=gpar(col="blue", lwd=5))

## Intersect the two
p <- polyclipGrob(r1, r2, name="p",
                  gp=gpar(col=NA, fill=rgb(0,0,0,.5)))

grid.draw(r1)
grid.draw(r2)
grid.draw(p)

## Merge the two
p <- polyclipGrob(r1, r2, op="union",
                  gp=gpar(col=NA, fill=rgb(0,0,0,.5)))

grid.newpage()
grid.draw(r1)
grid.draw(r2)
grid.draw(p)

## Take a chunk out of one with the other
p <- polyclipGrob(r1, r2, op="minus",
                  gp=gpar(col=NA, fill=rgb(0,0,0,.5)))

grid.newpage()
grid.draw(r1)
grid.draw(r2)
grid.draw(p)

## Two circles
c1 <- circleGrob(.4, .5, r=.2, gp=gpar(col="red", lwd=5))
c2 <- circleGrob(.6, .5, r=.2, gp=gpar(col="blue", lwd=5))
p <- polyclipGrob(c1, c2, name="p", op="minus",
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
p <- polyclipGrob(g1, g2, name="p", op="minus",
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
p <- polyclipGrob(l, gList(c1, c2), name="p", op="minus",
                  gp=gpar(col=rgb(0,0,0,.5), lwd=5))
grid.newpage()
grid.draw(c1)
grid.draw(c2)
grid.draw(l)
grid.draw(p)

## grob with 'vp' slot
grid.newpage()
pushViewport(viewport(width=.5, height=.5))
grid.rect(gp=gpar(col="grey"))
r1 <- rectGrob(width=.5, height=.5, gp=gpar(col="red"))
r2 <- rectGrob(vp=viewport(.25, .25, .5, .25, angle=45),
               gp=gpar(col="blue"))
p <- polyclipGrob(r1, r2, name="p", op="minus",
                  gp=gpar(col=NA, fill=rgb(0,0,0,.5), lwd=5))
grid.draw(r1)
grid.draw(r2)
grid.draw(p)

## gList that is combination of open and closed shapes
grid.newpage()
g1 <- gList(rectGrob(width=.5, height=.5, gp=gpar(col="red")),
            xsplineGrob(c(.25, .5, .75),
                        c(.75, .5, .75),
                        gp=gpar(col="red")))
g2 <- circleGrob(.75, .75, .2, gp=gpar(col="blue"))
p <- polyclipGrob(g1, g2, name="p", op="minus",
                  gp=gpar(col=rgb(0,0,0,.5), lwd=5))
grid.draw(g1)
grid.draw(g2)
grid.draw(p)

## grob that has makeContent() method
grid.newpage()
g1 <- roundrectGrob(width=.5, height=.5, gp=gpar(col="red"))
g2 <- circleGrob(.75, .75, .2, gp=gpar(col="blue"))
p <- polyclipGrob(g1, g2, name="p", op="minus",
                  gp=gpar(col=rgb(0,0,0,.5), lwd=5))
grid.draw(g1)
grid.draw(g2)
grid.draw(p)

## a gTree
grid.newpage()
g1 <- gTree(children=gList(roundrectGrob(width=.5, height=.5),
                           circleGrob(r=.25)),
            gp=gpar(col="red"))
g2 <- circleGrob(.75, .75, .15, gp=gpar(col="blue"))
p <- polyclipGrob(g1, g2, name="p", op="minus",
                  gp=gpar(col=rgb(0,0,0,.5), lwd=5))
grid.draw(g1)
grid.draw(g2)
grid.draw(p)

## a gTree as the "clipping" polygon
grid.newpage()
g1 <- circleGrob(.75, .75, .15, gp=gpar(col="red"))
g2 <- gTree(children=gList(roundrectGrob(width=.5, height=.5),
                           circleGrob(r=.25)),
            gp=gpar(col="blue"))
p <- polyclipGrob(g1, g2, name="p", op="minus",
                  gp=gpar(col=rgb(0,0,0,.5), lwd=5))
grid.draw(g1)
grid.draw(g2)
grid.draw(p)

## a gTree with a 'vp' slot
grid.newpage()
g1 <- gTree(children=gList(roundrectGrob(width=.5, height=.5),
                           circleGrob(r=.25)),
            vp=viewport(angle=45),
            gp=gpar(col="red"))
g2 <- circleGrob(.75, .75, .15, gp=gpar(col="blue"))
p <- polyclipGrob(g1, g2, name="p", op="minus",
                  gp=gpar(col=rgb(0,0,0,.5), lwd=5))
grid.draw(g1)
grid.draw(g2)
grid.draw(p)

## a gTree with 'childrenvp' slot
grid.newpage()
g1 <- gTree(childrenvp=viewport(angle=45, name="vp"),
            children=gList(roundrectGrob(width=.5, height=.5, vp="vp"),
                           circleGrob(r=.25, vp="vp")),
            gp=gpar(col="red"))
g2 <- circleGrob(.75, .75, .15, gp=gpar(col="blue"))
p <- polyclipGrob(g1, g2, name="p", op="minus",
                  gp=gpar(col=rgb(0,0,0,.5), lwd=5))
grid.draw(g1)
grid.draw(g2)
grid.draw(p)

## a gTree with 'vp' slot and child with 'vp' slot
grid.newpage()
g1 <- gTree(children=gList(roundrectGrob(width=.5, height=.5,
                                         vp=viewport(angle=10)),
                           circleGrob(r=.25)),
            vp=viewport(angle=45),
            gp=gpar(col="red"))
g2 <- circleGrob(.75, .75, .15, gp=gpar(col="blue"))
p <- polyclipGrob(g1, g2, name="p", op="minus",
                  gp=gpar(col=rgb(0,0,0,.5), lwd=5))
grid.draw(g1)
grid.draw(g2)
grid.draw(p)

## Destruction test
require(lattice)
g1 <- grid.grabExpr(expression(print(histogram(~ disp, mtcars))))
g2 <- circleGrob()
p <- polyclipGrob(g1, g2, name="p", op="minus",
                  gp=gpar(col=rgb(0,0,0,.5), lwd=5))
grid.newpage()
grid.draw(g1)
grid.draw(g2)
grid.draw(p)

grid.newpage()
grid.draw(p)

## Trimming lines
x <- 1:5
y <- 1:5
from <- 1:2/5
to <- c(2, 4)/5
require(graphics)
testTrim <- function(x, y, from, to) {
    trimmed <- trim(list(x=x, y=y), from, to)
    par(pin=c(diff(range(x)), diff(range(y))))
    plot(x, y, asp=1, xaxs="i", yaxs="i", pch=16)
    mapply(function(x, col)
               lines(x$x, x$y, col=col, lwd=5),
           trimmed, adjustcolor(1:length(trimmed), alpha=.5))
    trimmed
}
testTrim(x, y, from, to)
y <- c(1:3, 2:1)
testTrim(x, y, from, to)
from <- unit(c(1, -1), "in")
testTrim(x, y, from, to)

## Trim a grob
l <- xsplineGrob(c(.2, .5, .8), c(.5, .3, .5), shape=1,
                 gp=gpar(col="red"))
t <- trimGrob(l, from=unit(1, "cm"), to=c(.5, .8), name="t",
              gp=gpar(col=rgb(0,0,0,.5), lwd=5))
grid.newpage()
grid.draw(l)
grid.draw(t)

## Polyclip and trim on gPath
grid.newpage()
grid.rect(.4, .4, .4, .4, gp=gpar(col="red", fill=rgb(0,0,0,.2), lwd=5),
          name="r1")
grid.rect(.6, .6, .4, .4, gp=gpar(col="blue", lwd=5), name="r2")
grid.polyclip("r1", "r2")

grid.newpage()
grid.xspline(c(.2, .5, .8), c(.5, .3, .5), shape=1,
             gp=gpar(col=rgb(0,0,0,.2), lwd=c(5, 3)), name="x")
grid.trim("x", from=unit(1, "cm"), to=c(.5, .8))


dev.off()


####################### REGRESSION CHECK #######################################

## Check graphical output
testoutput <- function(basename) {
    PDF <- paste0(basename, ".pdf")
    savedPDF <- system.file("regression-tests", paste0(basename, ".save.pdf"),
                            package="gridGeometry")
    system(paste0("pdfseparate ", PDF, " test-pages-%d.pdf"))
    system(paste0("pdfseparate ", savedPDF, " model-pages-%d.pdf"))
    modelFiles <- list.files(pattern="model-pages-.*[.]pdf")
    N <- length(modelFiles)
    allGood <- TRUE
    testFiles <- list.files(pattern="test-pages-.*[.]pdf")
    if (length(testFiles) != N) {
        cat(sprintf("Number of test pages (%d) and model pages (%d) differ\n",
                    length(testFiles), N))
        allGood <- FALSE
    }
    for (i in 1:N) {
        system(paste0("convert -density 96 ",
                      "model-pages-", i, ".pdf ",
                      "model-pages-", i, ".png"))
        system(paste0("convert -density 96 ",
                      "test-pages-", i, ".pdf ",
                      "test-pages-", i, ".png"))
        result <- system(paste0("compare -metric AE ",
                                "model-pages-", i, ".png ",
                                "test-pages-", i, ".png ",
                                "diff-pages-", i, ".png ",
                                "2>&1"), intern=TRUE)
        if (result != "0") {
            cat(paste0("Test and model differ (page ", i, "; ",
                       "see diff-pages-", i, ".png)\n"))
            allGood <- FALSE
        }
    }
    if (!allGood)
        stop("Regression testing detected differences")
}

testoutput("tests")
