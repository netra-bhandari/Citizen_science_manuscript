#script to produce the correlation chord diagram among all question items

library(tidyverse)
library(wesanderson)

#### combining questions ####

library(qgraph)
library(igraph)
library(circlize)

#combine the variables of interest into a data frame
allDF <- locationDF %>% 
  full_join(.,incidentalDF,by="ID") %>%
  full_join(.,activeDF,by="ID") %>%
  full_join(.,surveytypeDF,by="ID") %>%
  full_join(.,motivationsDF,by="ID") %>%
  full_join(.,experienceDF,by="ID")

#decide on the order
myOrder <- c( rep("Location",ncol(locationDF[,-1])),
rep("Incidental",ncol(incidentalDF[,-1])),
rep("Active search",ncol(activeDF[,-1])),
rep("Survey_type",ncol(surveytypeDF[,-1])),
rep("Motivations",ncol(motivationsDF[,-1])),
rep("Experience",ncol(experienceDF[,-1])))
             
#decide on colour scheme
myCols <- wes_palette("Zissou1", 6, type = "continuous")
myCols <- c(rep(myCols[5],ncol(locationDF[,-1])),
rep(myCols[3],ncol(incidentalDF[,-1])),
rep(myCols[2],ncol(activeDF[,-1])),
rep(myCols[1],ncol(surveytypeDF[,-1])),
rep(myCols[4],ncol(motivationsDF[,-1])),
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

#tidy some of the names
tidy_cors$term[which(tidy_cors$term=="Frq")] <- "frequency activity"
tidy_cors$term[which(tidy_cors$term=="Member")] <- "membership"
tidy_cors$term[which(tidy_cors$term=="Knowledge")] <- "knowledge"
tidy_cors$term[which(tidy_cors$term=="nuYears")] <- "number of years"
tidy_cors$term[which(tidy_cors$term=="StandMonitor")] <- "standardized monitoring"
tidy_cors$term[which(tidy_cors$term=="many indivdiduals at the same time")] <- "many individuals"
tidy_cors$term[which(tidy_cors$term=="many species at same time")] <- "many species"


tidy_cors$name[which(tidy_cors$name=="Frq")] <- "frequency activity"
tidy_cors$name[which(tidy_cors$name=="Member")] <- "membership"
tidy_cors$name[which(tidy_cors$name=="Knowledge")] <- "knowledge"
tidy_cors$name[which(tidy_cors$name=="nuYears")] <- "number of years"
tidy_cors$name[which(tidy_cors$name=="StandMonitor")] <- "standardized monitoring"
tidy_cors$name[which(tidy_cors$name=="many indivdiduals at the same time")] <- "many individuals"
tidy_cors$name[which(tidy_cors$name=="many species at same time")] <- "many species"

routes_igraph <- tidy_cors %>% 
  graph_from_data_frame(directed = FALSE)

#sort attributes of network
#replace NAs witj lower number
tidy_cors2 <- tidy_cors
tidy_cors2[is.na(tidy_cors2)] <- 0.01
tidy_cors2[tidy_cors2==1] <- 0.01
summary(tidy_cors2$r)

#decide on colours
tidy_cors2$Colour <- "#FFFFFF00"
#tidy_cors2$Colour[abs(tidy_cors2$r) > 0.3] <- gplots::col2hex("grey70") #positive and large
col_fun = colorRamp2(breaks=c(0.3,0.72), 
                     c("grey90","grey10"))
tidy_cors2$Colour[abs(tidy_cors2$r) > 0.3] <- col_fun(tidy_cors2$r[abs(tidy_cors2$r)>0.3]) 

#Line type for positive and negative
tidy_cors2$Linetype <- ifelse(tidy_cors2$r>0,1,2)
table(tidy_cors2$Linetype)

# function to align the vertex labels nicely 
# https://kieranhealy.org/blog/archives/2011/02/18/aligning-labels-in-circular-igraph-layouts/)

radian.rescale <- function(x, start=0, direction=1) { 
  #start = offset from 12 o'clock in radians
  #direction = 1 for clockwise; -1 for anti-clockwise
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}

lab.locs <- radian.rescale(x=1:length(names(V(routes_igraph))), 
                           direction=-1, start=0) #start = offset from 12 o'clock in 

png("plots/radialplot.png",width = 800, height = 650)

plot(routes_igraph, 
     edge.curved = 0,
     edge.color = tidy_cors2$Colour,
     edge.lty = tidy_cors2$Linetype,
     edge.width = 4,
     vertex.size = 10,
     vertex.color= myCols, 
     vertex.label.dist = 4,
     vertex.label.degree = lab.locs,
     vertex.label.color = "black",
     vertex.label.cex = 1.2,
     vertex.label = names(V(routes_igraph)),
     layout = layout_in_circle(routes_igraph))

# #add legend
library(ComplexHeatmap)

lgd_links = Legend(at = c(0.3, 0.7),
                   col_fun = col_fun,
                   title_position = "topleft", title = "r")
draw(lgd_links,
     x = unit(0.08, "npc"), y = unit(0.6, "npc"),
     just = c("right", "top"))

dev.off()



### not used #####
# 
# #### qgraph ####
# #http://sachaepskamp.com/files/Cookbook.html#pearson-correlations
# 
# library("qgraph")
# corMat <- cor_auto(motivationsDF) 
# #The cor_auto function in the qgraph package can be used to automatically detect ordinal variables and compute polychoric and polyserial correlations in combination to Pearson correlations:
# 
# Graph_pcor <- qgraph::qgraph(corMat, graph = "pcor", layout = "spring", nodeNames = names(motivationsDF))
# 
# Graph_pcor <- qgraph::qgraph(corMat, graph = "pcor", layout = "spring", threshold = "bonferroni",
#                      sampleSize = nrow(motivationsDF), alpha = 0.05)
# 
# Graph_lasso <- qgraph::qgraph(corMat, graph = "glasso", layout = "spring", tuning = 0.25,
#                       sampleSize = nrow(motivationsDF), alpha = 0.05)
# #can add a groups argument to colour code by group
# 
# #### ggraph ####
# #https://drsimonj.svbtle.com/how-to-create-correlation-network-plots-with-corrr-and-ggraph
# 
# library(corrr)
# library(igraph)
# library(ggraph)
# 
# # Create a tidy data frame of correlations
# tidy_cors <- motivationsDF %>% 
#   correlate() %>% 
#   stretch() 
# 
# # Convert correlations stronger than some value
# # to an undirected graph object
# graph_cors <- tidy_cors %>% 
#   filter(abs(r) > 0.2) %>% 
#   graph_from_data_frame(directed = FALSE)
# 
# # Plot
# ggraph(graph_cors) +
#   geom_edge_link(aes(edge_alpha = abs(r), edge_width = abs(r), color = r)) +
#   guides(edge_alpha = "none", edge_width = "none") +
#   scale_edge_colour_gradientn(limits = c(-1, 1), colors = c("firebrick2", "dodgerblue2")) +
#   geom_node_point(color = "white", size = 5) +
#   geom_node_text(aes(label = name), repel = TRUE) +
#   theme_graph() +
#   labs(title = "Correlations between car variables")
# 
# 
# #### igraph #####
# #code from Maria - see script - 06.1_Fig_networks_circles.R
# library(igraph)
# library(corrr)
# library(ggraph)
# 
# tidy_cors <- motivationsDF[,-1] %>% 
#   correlate() %>% 
#   stretch()  
# 
# routes_igraph <- tidy_cors %>% 
#   graph_from_data_frame(directed = FALSE)
# 
# #sort attributes of network
# 
# #replace NAs witj lower number
# tidy_cors2 <- tidy_cors
# tidy_cors2[is.na(tidy_cors2)] <- 0.01
# #decide on colours
# tidy_cors2$Colour<- ifelse(abs(tidy_cors2$r>0.4), gplots::col2hex("grey70"),"#FFFFFF00")
# table(tidy_cors2$Colour)
# 
# # function to align the vertex labels nicely (from https://kieranhealy.org/blog/archives/2011/02/18/aligning-labels-in-circular-igraph-layouts/)
# radian.rescale <- function(x, start=0, direction=1) { #start = offset from 12 o'clock in radians; direction = 1 for clockwise; -1 for anti-clockwise.
#   c.rotate <- function(x) (x + start) %% (2 * pi) * direction
#   c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
# }
# 
# lab.locs <- radian.rescale(x=1:length(names(V(routes_igraph))), 
#                            direction=-1, start=0) #start = offset from 12 o'clock in 
# 
# plot(routes_igraph, 
#             edge.curved = 0,
#             edge.color = tidy_cors2$Colour,
#             edge.width = tidy_cors2$r*5,
#             vertex.color= "red", 
#             vertex.label.dist = 2.5,
#             vertex.label.degree = lab.locs,
#             vertex.label.color = "black",
#             vertex.label.cex = 0.9,
#             vertex.label = names(V(routes_igraph)),
#             layout=layout_in_circle(routes_igraph))



#### end ####

