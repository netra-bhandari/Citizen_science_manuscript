# PCA ---------------------------------------------------------------------

#library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
library(dplyr)
library(tidyverse)
library(stringr)
library(gridExtra)
library(ggrepel)
#install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")
library(ggpubr)
library(ggthemes)

source('helper_functions.R', encoding = 'UTF-8')

### read in a cleaned data frame ###

### load dataset ###

##trial with cleaned-data (keeping variable name same here so as not to make multiple changes below)
survey_data <- readRDS("cleaned-data/clean_data.RDS")
survey_data$Taxa <- survey_data$Bitte.wählen.Sie.EINE.Artengruppe..

### get taxa data frames ####

unique(survey_data$Taxa)

#get birdDF
birdDF <- subset(survey_data,Taxa="Vögel")

#get plantsDF
plantDF <- subset(survey_data,Taxa="Pflanzen")

#get insectDF
insectDF <- subset(survey_data,Taxa %in% c("Libellen","Schmetterlinge","Käfer","Bienen"))

#get amphibians
frogsDF <- subset(survey_data,Taxa="Amphibien/Reptilien")

### function for pca per question ####

## make a common theme for plots
theme_pca <- theme_classic()+
  theme(plot.title = element_text(size = 20, color = "black"),
        axis.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.8),
        axis.title.x = element_text(size = 20, color = "black"),   
        axis.title.y = element_text(size = 20, color = "black"))


doTaxaPCA <- function(sample_data){
  
  #q1
  sample_data_Q <- sample_data[,grepl("gegangen_sind__wie_sind_Sie_bei_der_Sammlung_von_Beobachtungen_vorgegangen",names(sample_data))]
  #removed first part of the question to make it generic for all taxa
  names(sample_data_Q) <- sapply(names(sample_data_Q),function(x)strsplit(x,"___")[[1]][2])
  sample_data_Q <- na.omit(sample_data_Q)
  names(sample_data_Q) <- c("complete checklist","all species","interesting species","common species",
                            "rare species")
  fit <-  prcomp(sample_data_Q, scale = TRUE)
  q1 <- fviz_pca_biplot(fit, 
                        # Fill individuals by groups
                        geom.ind = "point",
                        pointshape = 21,
                        pointsize = 0.8,
                        fill.ind = "black",
                        col.ind = "black",
                        col.var = "#225ea8",
                        repel = TRUE,
                        labelsize = 5,
                        arrowsize = 0.1,
                        title = "Which species are reported during an active search?")
  q1 <- ggpubr::ggpar(q1,
                      xlab = "PC1", ylab = "PC2",
                      ggtheme = theme_pca
  )
  
  #q2
  sample_data_Q <- sample_data[,grepl("Was_veranlasst_Sie_dazu",names(sample_data))]
  names(sample_data_Q) <- sapply(names(sample_data_Q),function(x)strsplit(x,"___")[[1]][3])
  sample_data_Q <- na.omit(sample_data_Q)
  names(sample_data_Q) <- c("rare species","many species at same time", "unexpected species",
                            "first time of year","unknown species","many indivdiduals at the same time",
                            "interesing species")
  fit <-  prcomp(sample_data_Q, scale = TRUE)
  
  q2 <- fviz_pca_biplot(fit, 
                        # Fill individuals by groups
                        geom.ind = "point",
                        pointshape = 21,
                        pointsize = 0.8,
                        fill.ind = "black",
                        col.ind = "black",
                        col.var = "#225ea8",
                        repel = TRUE,
                        labelsize = 5,
                        arrowsize = 0.1,
                        title = "What triggers the reporting of an incidental observation?")
  q2 <- ggpubr::ggpar(q2,
                      xlab = "PC1", ylab = "PC2",
                      ggtheme = theme_pca
  )
  #q3
  sample_data_Q <- sample_data[,grepl("wenn_Sie_sich_bei_der_Bestimmung",names(sample_data))]
  names(sample_data_Q) <- sapply(names(sample_data_Q),function(x)strsplit(x,"___")[[1]][2])
  sample_data_Q <- na.omit(sample_data_Q)
  names(sample_data_Q) <- c("guess","not reported","report at higher taxa level",
                            "ask another person","use an identification guide")
  
  fit <-  prcomp(sample_data_Q, scale = TRUE)
  q3 <- fviz_pca_biplot(fit, 
                        # Fill individuals by groups
                        geom.ind = "point",
                        pointsize = 0.8,
                        fill.ind = "black",
                        col.ind = "black",
                        col.var = "#225ea8",
                        repel = TRUE,
                        labelsize = 5,
                        arrowsize = 0.1,
                        title = "How do people deal with species identification uncertainity?")
  
  q3 <- ggpubr::ggpar(q3,
                      xlab = "PC1", ylab = "PC2",
                      ggtheme = theme_pca
  )
  #q4
  sample_data_Q <- sample_data[,grepl("wie_oft_haben_Sie_an_den_folgenden_Orten_nach_Arten",names(sample_data))]
  names(sample_data_Q) <- sapply(names(sample_data_Q),function(x)strsplit(x,"___")[[1]][2])
  sample_data_Q <- na.omit(sample_data_Q)
  names(sample_data_Q) <- c("protected areas","forest","wetland/water bodies","meadows",
                            "agricultural land","green urban","non-green urban","remote areas")
  
  fit <-  prcomp(sample_data_Q, scale = TRUE)
  q4 <- fviz_pca_biplot(fit, 
                        # Fill individuals by groups
                        geom.ind = "point",
                        pointshape = 21,
                        pointsize = 0.8,
                        fill.ind = "black",
                        col.ind = "black",
                        col.var = "#225ea8",
                        repel = TRUE,
                        labelsize = 5,
                        arrowsize = 0.1,
                        title = "What places do people visit?")
  q4 <- ggpubr::ggpar(q4,
                      xlab = "PC1", ylab = "PC2",
                      ggtheme = theme_pca
                      
  )
  
  grid.arrange(q1,q2,q3,q4)
  
}

### apply function ###

a <- doTaxaPCA(birdDF)
b <- doTaxaPCA(insectDF)
c <- doTaxaPCA(plantDF)
d <- doTaxaPCA(frogsDF)

### make a panel plot (4*4)

#@DIANA how should I arrange the plots? 
#below is just all plots in panel 
#we can also make use of different colors for 4 taxa groups 
#or we make 4 separate plots 

#plist <- list(a,b,c,d)

#pca_panel <- cowplot::plot_grid(plotlist = plist, ncol = 2)
#ggsave("pca_panel.png", filename = pca_panel)

### motivations PCA ####

motivationsDF <- sample_data[,grepl("Wie_wichtig_waren_Ihnen_die_folgenden_Aspekte",
                                    names(sample_data))]

#convert likert scale to ordinal scale
motivationsDF <- ordinal_fn(motivationsDF)
motivationsDF <- mutate_all(motivationsDF, function(x) as.numeric(as.character(x)))

names(motivationsDF) <- gsub("Wie_wichtig_waren_Ihnen_die_folgenden_Aspekte_als_Sie_Beobachtungsdaten_erfasst_haben_","",names(motivationsDF))

names(motivationsDF) <- c("improve species knowledge",
                          "contribute to science",
                          "support conservation",
                          "spend time outdoors",
                          "physical activity",
                          "protect a specific place",
                          "gain local knowledge",
                          "meet other people",
                          "have fun exploring")


# use rotated PCA - maximizes the loading of each axis onto x and y
#https://stats.stackexchange.com/questions/59213/how-to-compute-varimax-rotated-principal-components-in-r

library(psych)
pca_rotated <- principal(motivationsDF, rotate="varimax", nfactors=2, scores=TRUE)
biplot(pca_rotated)
summary(pca_rotated)
loadings <- as.data.frame(unclass(pca_rotated$loadings))
loadings$Names<-rownames(loadings)

#use ggplot
ggplot()+
  geom_segment(data=loadings, aes(x=0, y=0, xend=RC1, yend=RC2), 
               arrow=arrow(length=unit(0.2,"cm")),colour="grey")+
  geom_text_repel(data=loadings, aes(x=RC1, y=RC2, label=Names), 
            alpha=0.6, size=4)+
  scale_x_continuous("Principal Component 1", limits=c(-0.25,0.9))+
  scale_y_continuous("Principal Component 2", limits=c(-0.1,0.9))+
  theme_few()

#save scores and person ID
motivationsDF$ID <- sample_data$ID
motivationsDF$scores1 <- pca_rotated$scores[,1]
motivationsDF$scores2 <- pca_rotated$scores[,2]
saveRDS(motivationsDF,file="model-outputs/motivationsDF.rds")

#or a correlation plot
library(corrplot)
corrplot(cor(motivationsDF[,1:9]))
identifyCorrelations(motivationsDF[,1:9])

#chord plot??
corrMatrix<-cor(motivationsDF[,1:9])

library(circlize)
corrMatrix[upper.tri(corrMatrix)] <- NA
#set everything less than 0.7 as 0 (for transparency, see later)
corrMatrix[corrMatrix<0.7] <- 0.0

#melt matrix and remove identity correlations
corrMatrixm<-melt(corrMatrix)
corrMatrixm<-subset(corrMatrixm,!is.na(value))
corrMatrixm<-subset(corrMatrixm,value!=1)

#specific colour of strong correlation links
corrMatrixm$Colour[corrMatrixm$value!=0.1]<-col2hex("grey70")
#shade out weak links
corrMatrixm$Colour[corrMatrixm$value==0.0]<-"#FFFFFF00"

#plot chord diagram
#Fig. 1#
chordDiagram(corrMatrixm,symmetric = FALSE,
             transparency=0.5,
             col=corrMatrixm$Colour,
             grid.col=rev(mycols),
             order=rev(myorder),
             annotationTrack = "grid", preAllocateTracks = 1)
#change label direction
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .1, sector.name, cex=0.6,facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
  circos.axis(h = "top", labels.cex = 0.2, major.tick.percentage = 0.2, sector.index = sector.name, track.index = 2)
}, bg.border = NA)
