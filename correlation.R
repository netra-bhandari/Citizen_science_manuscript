#Diana just playing with some visualization options
#https://www.statmethods.net/stats/correlations.html

library(tidyverse)
library(wesanderson)

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


#### igraph #####
#code from Maria - see script - 06.1_Fig_networks_circles.R
library(igraph)
library(corrr)
library(ggraph)

tidy_cors <- motivationsDF[,-1] %>% 
  correlate() %>% 
  stretch()  

routes_igraph <- tidy_cors %>% 
  graph_from_data_frame(directed = FALSE)

#sort attributes of network
#replace NAs witj lower number
tidy_cors2 <- tidy_cors
tidy_cors2[is.na(tidy_cors2)] <- 0.01
#decide on colours
tidy_cors2$Colour<- ifelse(abs(tidy_cors2$r>0.4), gplots::col2hex("grey70"),"#FFFFFF00")
table(tidy_cors2$Colour)

# function to align the vertex labels nicely (from https://kieranhealy.org/blog/archives/2011/02/18/aligning-labels-in-circular-igraph-layouts/)
radian.rescale <- function(x, start=0, direction=1) { #start = offset from 12 o'clock in radians; direction = 1 for clockwise; -1 for anti-clockwise.
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}

lab.locs <- radian.rescale(x=1:length(names(V(routes_igraph))), 
                           direction=-1, start=0) #start = offset from 12 o'clock in 

plot(routes_igraph, 
            edge.curved = 0,
            edge.color = tidy_cors2$Colour,
            edge.width = tidy_cors2$r*5,
            vertex.color= "red", 
            vertex.label.dist = 2.5,
            vertex.label.degree = lab.locs,
            vertex.label.color = "black",
            vertex.label.cex = 0.9,
            vertex.label = names(V(routes_igraph)),
            layout=layout_in_circle(routes_igraph))


#### combining questions ####

library(qgraph)
library(igraph)

#get the data frames in the pca script
allDF <- surveytypeDF %>%
          full_join(.,activeDF,by="ID")%>%
          full_join(.,incidentalDF,by="ID")%>%
          full_join(.,motivationsDF,by="ID")%>%
          full_join(.,locationDF,by="ID")%>%
          full_join(.,experienceDF,by="ID")

myOrder <- c(rep("Survey_type",ncol(surveytypeDF[,-1])),
            rep("Active search",ncol(activeDF[,-1])),
            rep("Incidental",ncol(incidentalDF[,-1])),
            rep("Motivations",ncol(motivationsDF[,-1])),
            rep("Location",ncol(locationDF[,-1])),
            rep("Experience",ncol(experienceDF[,-1])))

myCols <- wes_palette("Zissou1", 6, type = "continuous")
myCols <- c(rep(myCols[1],ncol(surveytypeDF[,-1])),
            rep(myCols[2],ncol(activeDF[,-1])),
            rep(myCols[3],ncol(incidentalDF[,-1])),
            rep(myCols[4],ncol(motivationsDF[,-1])),
            rep(myCols[5],ncol(locationDF[,-1])),
            rep(myCols[6],ncol(experienceDF[,-1])))

#restrict to bird people
#IDs <- readRDS("ID_to_Taxa.rds")
#allDF <- filter(allDF, ID %in% IDs$ID[IDs$Taxa=="Vögel"])
#allDF <- select(allDF, -"using traps")

tidy_cors <- allDF[,-1] %>% 
  cor_auto() %>% #uses the correct correlation
  as.data.frame() %>%
  mutate(term = names(allDF)[-1]) %>%
  pivot_longer(!term,values_to="r")

tidy_cors %>%
  filter(abs(r)>0.4) %>%
  nrow()

routes_igraph <- tidy_cors %>% 
  graph_from_data_frame(directed = FALSE)

#sort attributes of network
#replace NAs witj lower number
tidy_cors2 <- tidy_cors
tidy_cors2[is.na(tidy_cors2)] <- 0.01
tidy_cors2[tidy_cors2==1] <- 0.01
summary(tidy_cors2$r)
#decide on colours
#stidy_cors2$Colour <- ifelse(abs(tidy_cors2$r)>0.6, gplots::col2hex("grey70"),"#FFFFFF00")
tidy_cors2$Colour <- "#FFFFFF00"
tidy_cors2$Colour[tidy_cors2$r > 0.4] <- gplots::col2hex("grey70")
tidy_cors2$Colour[tidy_cors2$r<(-0.4)] <-  gplots::col2hex("grey30")#darker
table(tidy_cors2$Colour)
tidy_cors2$r <- abs(tidy_cors2$r) 

# function to align the vertex labels nicely (from https://kieranhealy.org/blog/archives/2011/02/18/aligning-labels-in-circular-igraph-layouts/)
radian.rescale <- function(x, start=0, direction=1) { #start = offset from 12 o'clock in radians; direction = 1 for clockwise; -1 for anti-clockwise.
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}

lab.locs <- radian.rescale(x=1:length(names(V(routes_igraph))), 
                           direction=-1, start=0) #start = offset from 12 o'clock in 

plot(routes_igraph, 
     edge.curved = 0,
     edge.color = tidy_cors2$Colour,
     edge.width = 4,
     vertex.size = 10,
     vertex.color= myCols, 
     vertex.label.dist = 2.5,
     vertex.label.degree = lab.locs,
     vertex.label.color = "black",
     vertex.label.cex = 0.6,
     vertex.label = names(V(routes_igraph)),
     layout = layout_in_circle(routes_igraph))

legend(-1.5,1.55,
       legend=unique(myOrder),cex=0.8,col='black',
       pch=21, pt.bg=unique(myCols),bty="n",
       ncol=3)




#### end ####

