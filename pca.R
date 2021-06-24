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
sample_data <- readRDS("cleaned-data/clean_data.RDS")
sample_data$Taxa <- sample_data$Bitte.wahlen.Sie.EINE.Artengruppe..

### get taxa data frames ####

unique(sample_data$Taxa)

#get birdDF
birdDF <- subset(sample_data,Taxa="Vögel")

#get plantsDF
plantDF <- subset(sample_data,Taxa="Pflanzen")

#get insectDF
insectDF <- subset(sample_data,Taxa %in% c("Libellen","Schmetterlinge","Käfer","Bienen"))

#get amphibians
frogsDF <- subset(sample_data,Taxa="Amphibien/Reptilien")

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
  sample_data_Q <- sample_data[,grepl("gegangen_sind_wie_sind_Sie_bei_der_Sammlung_von_Beobachtungen_vorgegangen",names(sample_data))]
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

motivationsDF <- sample_data[,c(1,grep("Wie_wichtig_waren_Ihnen_die_folgenden_Aspekte",
                                    names(sample_data)))]


motivationsDF <- format4PCA(motivationsDF)
names(motivationsDF) <- gsub("Wie_wichtig_waren_Ihnen_die_folgenden_Aspekte_als_Sie_Beobachtungsdaten_erfasst_haben_","",names(motivationsDF))

names(motivationsDF)[-1] <- c("improve species knowledge",
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
pca_rotated <- principal(motivationsDF[,-1], rotate="varimax", nfactors=2, scores=TRUE)
summary(pca_rotated)
plotPCA(pca_rotated)

#save scores and person ID
motivationsDF$scores1 <- pca_rotated$scores[,1]
motivationsDF$scores2 <- pca_rotated$scores[,2]
saveRDS(motivationsDF,file="model-outputs/motivationsDF.rds")

#### active search pca ####

activeDF <- sample_data[,c(1,grep("gegangen_sind_wie_sind_Sie_bei_der_Sammlung_von_Beobachtungen_vorgegangen",names(sample_data)))]
#removed first part of the question to make it generic for all taxa

activeDF <- format4PCA(activeDF)

names(activeDF) <- gsub("Wenn_Sie_aktiv_auf_die_Suche_nach_gegangen_sind_wie_sind_Sie_bei_der_Sammlung_von_Beobachtungen_vorgegangen_","",names(activeDF))

activeDF <- na.omit(activeDF)

names(activeDF)[-1] <- c("complete checklist","all species","interesting species",
                          "common species","rare species")
#rotated PCA
pca_rotated <- principal(activeDF[,-1], rotate="varimax", nfactors=2, scores=TRUE)
summary(pca_rotated)
loadings(pca_rotated)
plotPCA(pca_rotated)

### incidental PCA ####

incidentalDF <- sample_data[,c(1,grep("Was_veranlasst_Sie_dazu",names(sample_data)))]
#removed first part of the question to make it generic for all taxa

incidentalDF <- format4PCA(incidentalDF)

names(incidentalDF) <- gsub("Was_veranlasst_Sie_dazu_eine_Beobachtung_zu_melden_wenn_Sie_eine_Art_zufallig_sehen_ohne_aktive_Suche_Bitte_geben_Sie_an_wie_oft_die_folgenden_Grunde_bei_Ihnen_zur_Meldung_einer_Beobachtung_fuhren_","",names(incidentalDF))

incidentalDF <- na.omit(incidentalDF)

names(incidentalDF)[-1] <- c("rare species","many species at same time", "unexpected species",
                          "first time of year","unknown species","many indivdiduals at the same time",
                          "interesing species")
#rotated PCA
pca_rotated <- principal(incidentalDF[,-1], rotate="varimax", nfactors=2, scores=TRUE)
summary(pca_rotated)
loadings(pca_rotated)
plotPCA(pca_rotated)

#### trap PCA ####

trapDF <- sample_data[,c(1,grep("Nach_welchem_Schema_erfassen_Sie_",names(sample_data)))]

#removed first part of the question to make it generic for all taxa
names(trapDF) <- gsub("Nach_welchem_Schema_erfassen_Sie_die_mit_der_Falle_gefangenen_Arten_","",names(trapDF))

trapDF <- format4PCA(trapDF)

trapDF <- na.omit(trapDF)
nrow(trapDF)

names(trapDF)[-1] <- c("all species","interesting species","common species",
                          "rare species","new species")
#rotated PCA
pca_rotated <- principal(trapDF[,-1], rotate="varimax", nfactors=2, scores=TRUE)
summary(pca_rotated)
loadings(pca_rotated)
plotPCA(pca_rotated)

#### location PCA ####

locationDF <- sample_data[,c(1,grep("wie_oft_haben_Sie_an_den_folgenden_Orten_nach_Arten",names(sample_data)))]
#removed first part of the question to make it generic for all taxa

locationDF <- format4PCA(locationDF)

names(locationDF) <- gsub("Wenn_Sie_an_den_Fruhling_oder_Sommer_2020_denken_wie_oft_haben_Sie_an_den_folgenden_Orten_nach_Arten_gesucht_","",names(locationDF))

locationDF <- na.omit(locationDF)
nrow(locationDF)

names(locationDF)[-1] <- c("protected areas","forest","wetland/water bodies","meadows",
                          "agricultural land","green urban","non-green urban","remote areas")

#rotated PCA
pca_rotated <- principal(locationDF[,-1], rotate="varimax", nfactors=2, scores=TRUE)
summary(pca_rotated)
loadings(pca_rotated)
plotPCA(pca_rotated)


#### experience PCA ####

experienceDF <- sample_data[,c("ID","Wie_viele_Jahre_sind_Sie_schon_in_der_Erfassung_der_Artenbeobachtungsdaten_aktiv_","Wie_oft_haben_Sie_im_Fruhling_oder_Sommer_2020_Artdaten_gesammelt_",
                                "Nehmen_Sie_an_einem_gross_angelegten_standardisierten_Monitoringsystem_teil_z_B_Tagfalter_Monitoring_Deutschland_",
                                "Besitzen_Sie_Fachkenntnisse_im_Bereich_des_Biodiversitatsmonitorings_",
                                "Sind_Sie_Mitglied_in_einer_Fachgesellschaft_fur_eine_bestimmte_Artengruppe_z_B_GdO_GAC_DDA_etc_")]

#removed first part of the question to make it generic for all taxa

experienceDF <- format4PCA(experienceDF)

names(experienceDF)[-1] <- c("nuYears","Frq","StandMonitor","Knowledge","Member")

experienceDF <- na.omit(experienceDF)
nrow(experienceDF)

#rotated PCA
pca_rotated <- principal(experienceDF[,-1], rotate="varimax", nfactors=2, scores=TRUE)
summary(pca_rotated)
loadings(pca_rotated)
plotPCA(pca_rotated)


#### id uncertainty pca ####

idDF <- sample_data[,c(1,grep("wenn_Sie_sich_bei_der_Bestimmung",names(sample_data)))]

#removed first part of the question to make it generic for all taxa
names(idDF) <- gsub("Was_tun_Sie_wenn_Sie_sich_bei_der_Bestimmung_einer_Art_unsicher_sind_Bitte_geben_Sie_an_wie_haufig_Sie_in_diesem_Fall_folgende_Dinge_tun_","",names(idDF))

idDF <- format4PCA(idDF)

names(idDF)[-1] <- c("guess","not report","report at higher taxa level",
                          "ask another person","use an identification guide")

idDF <- na.omit(idDF)
nrow(idDF)

#rotated PCA
pca_rotated <- principal(idDF[,-1], rotate="varimax", nfactors=2, scores=TRUE)
summary(pca_rotated)
loadings(pca_rotated)
plotPCA(pca_rotated)

#### survey type ####

surveytypeDF <- sample_data[,c(1,grep("Wie_viele_der_Beobachtungen_die_Sie_im_Fruhling_und_Sommer_2020_gemeldet_haben",names(sample_data)))]

names(surveytypeDF) <- gsub("Wie_viele_der_Beobachtungen_die_Sie_im_Fruhling_und_Sommer_2020_gemeldet_haben_waren_","",names(surveytypeDF))

surveytypeDF <- format4PCA(surveytypeDF)

names(surveytypeDF)[-1] <- c("active search","opportunistic","using traps")

surveytypeDF <- na.omit(surveytypeDF)
nrow(surveytypeDF)

#rotated PCA
pca_rotated <- principal(surveytypeDF[,-1], rotate="varimax", nfactors=2, scores=TRUE)
summary(pca_rotated)
loadings(pca_rotated)
plotPCA(pca_rotated)

#### end ####