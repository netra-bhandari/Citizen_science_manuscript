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
library(psych)

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

### taxa pca per question ####

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
motivationsDF <- format4PCA(motivationsDF)
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

#rotated PCA
pca_rotated <- principal(motivationsDF, rotate="varimax", nfactors=2, scores=TRUE)
summary(pca_rotated)
plotPCA(pca_rotated)

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
library(circlize)
corrMatrix <- cor(motivationsDF[,1:9])
corrMatrixm <- melt(corrMatrix)
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

### active search pca ###

sample_data_Q <- sample_data[,grepl("gegangen_sind_wie_sind_Sie_bei_der_Sammlung_von_Beobachtungen_vorgegangen",names(sample_data))]
#removed first part of the question to make it generic for all taxa

sample_data_Q <- format4PCA(sample_data_Q)

names(sample_data_Q) <- gsub("Wenn_Sie_aktiv_auf_die_Suche_nach_gegangen_sind_wie_sind_Sie_bei_der_Sammlung_von_Beobachtungen_vorgegangen_","",names(sample_data_Q))

sample_data_Q <- na.omit(sample_data_Q)

names(sample_data_Q) <- c("complete checklist","all species","interesting species",
                          "common species","rare species")
#rotated PCA
pca_rotated <- principal(sample_data_Q, rotate="varimax", nfactors=2, scores=TRUE)
summary(pca_rotated)
loadings(pca_rotated)
plotPCA(pca_rotated)

### incidental PCA ####

sample_data_Q <- sample_data[,grepl("Was_veranlasst_Sie_dazu",names(sample_data))]
#removed first part of the question to make it generic for all taxa

sample_data_Q <- format4PCA(sample_data_Q)

names(sample_data_Q) <- gsub("Was_veranlasst_Sie_dazu_eine_Beobachtung_zu_melden_wenn_Sie_eine_Art_zufallig_sehen_ohne_aktive_Suche_Bitte_geben_Sie_an_wie_oft_die_folgenden_Grunde_bei_Ihnen_zur_Meldung_einer_Beobachtung_fuhren_","",names(sample_data_Q))

sample_data_Q <- na.omit(sample_data_Q)

names(sample_data_Q) <- c("rare species","many species at same time", "unexpected species",
                          "first time of year","unknown species","many indivdiduals at the same time",
                          "interesing species")
#rotated PCA
pca_rotated <- principal(sample_data_Q, rotate="varimax", nfactors=2, scores=TRUE)
summary(pca_rotated)
loadings(pca_rotated)
plotPCA(pca_rotated)


#### trap PCA ####

sample_data_Q <- sample_data[,grepl("Nach_welchem_Schema_erfassen_Sie_",names(sample_data))]
#removed first part of the question to make it generic for all taxa

sample_data_Q <- format4PCA(sample_data_Q)

names(sample_data_Q) <- gsub("Nach_welchem_Schema_erfassen_Sie_die_mit_der_Falle_gefangenen_Arten_","",names(sample_data_Q))

sample_data_Q <- na.omit(sample_data_Q)
nrow(sample_data_Q)

names(sample_data_Q) <- c("all species","interesting species","common species",
                          "rare species","new species")
#rotated PCA
pca_rotated <- principal(sample_data_Q, rotate="varimax", nfactors=2, scores=TRUE)
summary(pca_rotated)
loadings(pca_rotated)
plotPCA(pca_rotated)

#### location PCA ####

sample_data_Q <- sample_data[,grepl("wie_oft_haben_Sie_an_den_folgenden_Orten_nach_Arten",names(sample_data))]
#removed first part of the question to make it generic for all taxa

sample_data_Q <- format4PCA(sample_data_Q)

names(sample_data_Q) <- gsub("Wenn_Sie_an_den_Fruhling_oder_Sommer_2020_denken_wie_oft_haben_Sie_an_den_folgenden_Orten_nach_Arten_gesucht_","",names(sample_data_Q))

sample_data_Q <- na.omit(sample_data_Q)
nrow(sample_data_Q)

names(sample_data_Q) <- c("protected areas","forest","wetland/water bodies","meadows",
                          "agricultural land","green urban","non-green urban","remote areas")

#rotated PCA
pca_rotated <- principal(sample_data_Q, rotate="varimax", nfactors=2, scores=TRUE)
summary(pca_rotated)
loadings(pca_rotated)
plotPCA(pca_rotated)
