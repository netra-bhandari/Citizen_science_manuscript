#Diana just playing with some visualization options

#get motivationsDF from the pca script
motivationsDF <- readRDS("model-outputs/motivationsDF.RDS")
motivationsDF <- motivationsDF[,1:9]

#### correlation plot ####

library(corrplot)
corrplot(cor(motivationsDF))
identifyCorrelations(motivationsDF)

#### chord plot ####
library(circlize)
corrMatrix <- cor(motivationsDF)
corrMatrixm <- reshape2::melt(corrMatrix)
corrMatrixm <- subset(corrMatrixm,!is.na(value))
corrMatrixm <- subset(corrMatrixm,value!=1)
corrMatrixm$Colour <- "#FFFFFF00"
corrMatrixm$Colour[abs(corrMatrixm$value) > 0.5] <- gplots::col2hex("grey70")

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

#### qgraph ####
#http://sachaepskamp.com/files/Cookbook.html#pearson-correlations

library("qgraph")
corMat <- cor_auto(motivationsDF) 
#The cor_auto function in the qgraph package can be used to automatically detect ordinal variables and compute polychoric and polyserial correlations in combination to Pearson correlations:

Graph_pcor <- qgraph::qgraph(corMat, graph = "pcor", layout = "spring", nodeNames = names(motivationsDF))

Graph_pcor <- qgraph::qgraph(corMat, graph = "pcor", layout = "spring", threshold = "bonferroni",
                     sampleSize = nrow(motivationsDF), alpha = 0.05)

Graph_lasso <- qgraph::qgraph(corMat, graph = "glasso", layout = "spring", tuning = 0.25,
                      sampleSize = nrow(motivationsDF), alpha = 0.05)
#can add a groups argument to colour code by group

#### ggraph ####
#https://drsimonj.svbtle.com/how-to-create-correlation-network-plots-with-corrr-and-ggraph

library(corrr)
library(igraph)
library(ggraph)

# Create a tidy data frame of correlations
tidy_cors <- motivationsDF %>% 
  correlate() %>% 
  stretch()

# Convert correlations stronger than some value
# to an undirected graph object
graph_cors <- tidy_cors %>% 
  filter(abs(r) > 0.2) %>% 
  graph_from_data_frame(directed = FALSE)

# Plot
ggraph(graph_cors) +
  geom_edge_link(aes(edge_alpha = abs(r), edge_width = abs(r), color = r)) +
  guides(edge_alpha = "none", edge_width = "none") +
  scale_edge_colour_gradientn(limits = c(-1, 1), colors = c("firebrick2", "dodgerblue2")) +
  geom_node_point(color = "white", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_graph() +
  labs(title = "Correlations between car variables")

#### end ####
