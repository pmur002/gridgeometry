library(gridGeometry, lib.loc = "D:/Stats781/library")

l1 = linesGrob(x = c(0.4, 0.4), y = c(0.3, 0.7))
l2 = linesGrob(x = c(0.6, 0.6), y = c(0.3, 0.7))
l3 = linesGrob(x = c(0.4, 0.6), y = c(0.5, 0.5))
l = gList(l1, l2, l3)

grid.newpage()
grid.polylineoffset(l, 0.05)

entry <- paste0("Panel 1,Wall 1,13mm Plasterboard on studs")
coord <- polylineoffset(l, 0.05)

for (i in 1:length(coord[[1]]$x))
{
  entry <- paste0(entry, ",", coord[[1]]$x[i] - 4, " ", coord[[1]]$y[i] - 2.5)
}

fileConn<-file("D:/PanelCoordinate.csv")
writeLines(entry, fileConn)
close(fileConn)