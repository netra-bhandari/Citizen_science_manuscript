#Diana just playing with some visualization options

#or a correlation plot
library(corrplot)
corrplot(cor(motivationsDF[,1:9]))
identifyCorrelations(motivationsDF[,1:9])

#chord plot??
library(circlize)
corrMatrix <- cor(motivationsDF[,1:9])
corrMatrixm <- reshape2::melt(corrMatrix)
corrMatrixm <- subset(corrMatrixm,!is.na(value))
corrMatrixm <- subset(corrMatrixm,value!=1)
#specific colour of strong correlation links
corrMatrixm$Colour <- "#FFFFFF00"
corrMatrixm$Colour[abs(corrMatrixm$value) > 0.5] <- gplots::col2hex("grey70")

#plot chord diagram
chordDiagram(corrMatrixm,symmetric = FALSE,
             transparency = 0.5,
             col = corrMatrixm$Colour,
             #grid.col=rev(mycols),
             order= names(motivationsDF),
             annotationTrack = "grid", preAllocateTracks = 1)

#change label direction
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .1, sector.name, cex=0.6,facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
  circos.axis(h = "top", labels.cex = 0.2, major.tick.length = 0.2, sector.index = sector.name, track.index = 2)
}, bg.border = NA)

