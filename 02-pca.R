#Dimension reduction for all questions

#PCA and cluster analysis

#library(devtools)
#install_github("vqv/ggbiplot")
#library(ggbiplot)
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
library(cowplot)
library(wesanderson)

source('helper_functions.R', encoding = 'UTF-8')

### read in a cleaned data frame ###

### load dataset ###

##trial with cleaned-data (keeping variable name same here so as not to make multiple changes below)
sample_data <- readRDS("cleaned-data/clean_data.RDS")
#saveRDS(sample_data[,c("ID","Taxa")],file="ID_to_Taxa.rds")

nrow(sample_data)#899

### across all species ####

#### motivations PCA ####

motivationsDF <- sample_data[,c(1,grep("Wie_wichtig_waren_Ihnen_die_folgenden_Aspekte",
                                    names(sample_data)))]

motivationsDF_notknow <- as.numeric(apply(motivationsDF,2,function(x)
  mean(x[!is.na(x)]=="weiß nicht")))

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
loadings(pca_rotated)
p1 <- plotPCA(pca_rotated)

p1#identify variable loading strongly on each axis - of most interest given the top two
#axis 1 = spend time outdoors
#axis 2 = support conservation
PCA_motivations <- motivationsDF[,c("ID","spend time outdoors","support conservation")]

scores_motivations <- data.frame(ID = PCA_motivations$ID,
                                 scores = pca_rotated$scores[,1],
                                 scores2 = pca_rotated$scores[,2])

#### active search pca ####

activeDF <- sample_data[,c(1,grep("gegangen_sind_wie_sind_Sie_bei_der_Sammlung_von_Beobachtungen_vorgegangen",names(sample_data)))]
#removed first part of the question to make it generic for all taxa

activeDF_notknow <- as.numeric(apply(activeDF,2,function(x)
  mean(x[!is.na(x)]=="weiß nicht")))

activeDF <- format4PCA(activeDF)

names(activeDF) <- gsub("Wenn_Sie_aktiv_auf_die_Suche_nach_gegangen_sind_wie_sind_Sie_bei_der_Sammlung_von_Beobachtungen_vorgegangen_","",names(activeDF))

activeDF <- na.omit(activeDF)

names(activeDF)[-1] <- c("complete checklist","all species","interesting species",
                          "common species","rare species")
#rotated PCA
pca_rotated <- principal(activeDF[,-1], rotate="varimax", nfactors=2, scores=TRUE)
summary(pca_rotated)
loadings(pca_rotated)
p2 <- plotPCA(pca_rotated)

#identify variable loading most strongly on each axis
#axis 1 = interesting species
#axis 2 = common species
PCA_active <- activeDF[,c("ID","interesting species","common species")]

scores_active <- data.frame(ID = PCA_active$ID,
                            scores = pca_rotated$scores[,1],
                            scores2 = pca_rotated$scores[,2])

#### incidental PCA ####

incidentalDF <- sample_data[,c(1,grep("Was_veranlasst_Sie_dazu",names(sample_data)))]
#removed first part of the question to make it generic for all taxa

incidentalDF_notknow <- as.numeric(apply(incidentalDF,2,function(x)
  mean(x[!is.na(x)]=="weiß nicht")))[-6]

incidentalDF <- format4PCA(incidentalDF)

names(incidentalDF) <- gsub("Was_veranlasst_Sie_dazu_eine_Beobachtung_zu_melden_wenn_Sie_eine_Art_zufallig_sehen_ohne_aktive_Suche_Bitte_geben_Sie_an_wie_oft_die_folgenden_Grunde_bei_Ihnen_zur_Meldung_einer_Beobachtung_fuhren_","",names(incidentalDF))

incidentalDF <- na.omit(incidentalDF)

names(incidentalDF)[-1] <- c("rare species","many species at same time", "unexpected species",
                          "first time of year","unknown species","many indivdiduals at the same time",
                          "interesting species")

#remove unknown - silly answer
incidentalDF <- incidentalDF[,-6]

#rotated PCA
pca_rotated <- principal(incidentalDF[,-1], rotate="varimax", nfactors=2, scores=TRUE)
summary(pca_rotated)
loadings(pca_rotated)
p3 <- plotPCA(pca_rotated)

#identify variable loading most strongly on each axis
#axis 1 = rare species
#axis 2 = many indivdiduals at the same time
PCA_incidental <- incidentalDF[,c("ID","rare species","many indivdiduals at the same time")]

scores_incidental <- data.frame(ID = PCA_incidental$ID,
                                scores = pca_rotated$scores[,1],
                                scores2 = pca_rotated$scores[,2])

#### trap PCA ####

trapDF <- sample_data[,c(1,grep("Nach_welchem_Schema_erfassen_Sie_",names(sample_data)))]

trapDF_notknow <- as.numeric(apply(trapDF,2,function(x)
  mean(x[!is.na(x)]=="weiß nicht")))

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
p4 <- plotPCA(pca_rotated)

#identify variable loading most strongly on each axis
#axis 1 = rare species
#axis 2 = all species

#### location PCA ####

locationDF <- sample_data[,c(1,grep("wie_oft_haben_Sie_an_den_folgenden_Orten_nach_Arten",names(sample_data)))]
#removed first part of the question to make it generic for all taxa

locationDF_notknow <- as.numeric(apply(locationDF,2,function(x)
  mean(x[!is.na(x)]=="weiß nicht")))

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
p5 <- plotPCA(pca_rotated)

#identify variable loading most strongly on each axis
#axis 1 = protected areas
#axis 2 = non-green urban
PCA_location <- locationDF[,c("ID","protected areas","non-green urban")]

scores_location <- data.frame(ID = PCA_location$ID,
                              scores = pca_rotated$scores[,1],
                              scores2 = pca_rotated$scores[,2])

#### experience PCA ####

experienceDF <- sample_data[,c("ID","Wie_viele_Jahre_sind_Sie_schon_in_der_Erfassung_der_Artenbeobachtungsdaten_aktiv_","Wie_oft_haben_Sie_im_Fruhling_oder_Sommer_2020_Artdaten_gesammelt_",
                                "Nehmen_Sie_an_einem_gross_angelegten_standardisierten_Monitoringsystem_teil_z_B_Tagfalter_Monitoring_Deutschland_",
                                "Besitzen_Sie_Fachkenntnisse_im_Bereich_des_Biodiversitatsmonitorings_",
                                "Sind_Sie_Mitglied_in_einer_Fachgesellschaft_fur_eine_bestimmte_Artengruppe_z_B_GdO_GAC_DDA_etc_")]

#removed first part of the question to make it generic for all taxa


experienceDF_notknow <- as.numeric(apply(experienceDF,2,function(x)
  mean(x[!is.na(x)]=="weiß nicht")))

experienceDF <- format4PCA(experienceDF)

names(experienceDF)[-1] <- c("nuYears","Frq","StandMonitor","Knowledge","Member")

experienceDF <- na.omit(experienceDF)
experienceDF$nuYears <- log2(experienceDF$nuYears+1)

nrow(experienceDF)

#rotated PCA
pca_rotated <- principal(experienceDF[,-1], rotate="varimax", nfactors=2, scores=TRUE)
summary(pca_rotated)
loadings(pca_rotated)
p6 <- plotPCA(pca_rotated)

#identify variable loading most strongly on each axis
#axis 1 = Member
#axis 2 = Frq
PCA_experience <- experienceDF[,c("ID","Member","Frq")]

scores_experience <- data.frame(ID = PCA_experience$ID,
                                scores = pca_rotated$scores[,1],
                                scores2 = pca_rotated$scores[,2])

#### id uncertainty pca ####

idDF <- sample_data[,c(1,grep("wenn_Sie_sich_bei_der_Bestimmung",names(sample_data)))]

#removed first part of the question to make it generic for all taxa
names(idDF) <- gsub("Was_tun_Sie_wenn_Sie_sich_bei_der_Bestimmung_einer_Art_unsicher_sind_Bitte_geben_Sie_an_wie_haufig_Sie_in_diesem_Fall_folgende_Dinge_tun_","",names(idDF))


idDF_notknow <- as.numeric(apply(idDF,2,function(x)
  mean(x[!is.na(x)]=="weiß nicht")))

idDF <- format4PCA(idDF)

names(idDF)[-1] <- c("guess","not report","report at higher taxa level",
                          "ask another person","use an identification guide")

idDF <- na.omit(idDF)
nrow(idDF)

#rotated PCA
pca_rotated <- principal(idDF[,-1], rotate="varimax", nfactors=2, scores=TRUE)
summary(pca_rotated)
loadings(pca_rotated)
p7 <- plotPCA(pca_rotated)

#identify variable loading most strongly on each axis
#axis 1 = use an identification guide
#axis 2 = not report
PCA_id <- idDF[,c("ID","use an identification guide","not report")]

scores_id <- data.frame(ID = PCA_id$ID,
                        scores = pca_rotated$scores[,1],
                        scores2 = pca_rotated$scores[,2])

#### survey type ####

surveytypeDF <- sample_data[,c(1,grep("Wie_viele_der_Beobachtungen_die_Sie_im_Fruhling_und_Sommer_2020_gemeldet_haben",names(sample_data)))]

names(surveytypeDF) <- gsub("Wie_viele_der_Beobachtungen_die_Sie_im_Fruhling_und_Sommer_2020_gemeldet_haben_waren_","",names(surveytypeDF))

surveytypeDF_notknow <- as.numeric(apply(surveytypeDF,2,function(x)
  mean(x[!is.na(x)]=="weiß nicht")))

surveytypeDF <- format4PCA(surveytypeDF)

names(surveytypeDF)[-1] <- c("active search","opportunistic","using traps")

surveytypeDF <- na.omit(surveytypeDF)
nrow(surveytypeDF)

#rotated PCA
pca_rotated <- principal(surveytypeDF[,-1], rotate="varimax", nfactors=2, scores=TRUE)
summary(pca_rotated)
loadings(pca_rotated)
p8 <- plotPCA(pca_rotated)

#identify variable loading most strongly on each axis
#axis 1 = opportunistic
#axis 2 = using traps
PCA_survey <- surveytypeDF[,c("ID","active search","using traps")]

scores_survey <- data.frame(ID = PCA_survey$ID,
                            scores = pca_rotated$scores[,1],
                            scores2 = pca_rotated$scores[,2])

### combine all PCA plots ####

library(cowplot)
plot_grid(p6,p1,p8,p5,
          nrow=2,
          labels=c("a","b","c","d"))

ggsave(file="plots/SI_PCA1.png",width=5.5,height=3.5)

plot_grid(p2,p3,p4,p7,
          nrow=2,
          labels=c("e","f","g","h"))

ggsave(file="plots/SI_PCA2.png",width=5.5,height=3.5)

### analyse prop of dont knows ####

dontknows <- ls()[grepl("notknow",ls())]
allDK <- lapply(dontknows, function(x){
  mainObject <- strsplit(x,"_")[[1]][1]
  data.frame(items=names(get(mainObject)),prop=get(x),file=x)
  })

allDK <- do.call(rbind,allDK)
#all less than 0.55

ggplot(allDK)+
  geom_col(aes(x=items, y=prop))+
  coord_flip()+
  facet_wrap(~file,scales="free")

### PCA of top axes ####

#identify main axes of variation

#Choice:

#(1) first main axis
allpcaDF <- PCA_survey[,1:2] %>%
  full_join(.,PCA_active[,1:2],by="ID")%>%
  full_join(.,PCA_incidental[,1:2],by="ID")%>%
  full_join(.,PCA_motivations[,1:2],by="ID")%>%
  full_join(.,PCA_location[,1:2],by="ID")%>%
  full_join(.,PCA_experience[,1:2],by="ID")%>%
  full_join(.,PCA_id[,1:2],by="ID")%>%
  janitor::clean_names(.)

#check which columns have NAs
apply(allpcaDF,2,function(x)sum(is.na(x)))
allpcaDF <- na.omit(allpcaDF)
nrow(allpcaDF)

#tidy names - for first option
names(allpcaDF) <- c("id","active_searches","interesting species (active)",
                     "rare_species(opportunistic)","spend_time_outdoors",
                     "visit_protected_areas","membership","use_ID_guide")

#(2) first two main axis
allpcaDF <- PCA_survey[,1:3] %>%
  full_join(.,PCA_active[,1:3],by="ID")%>%
  full_join(.,PCA_incidental[,1:3],by="ID")%>%
  full_join(.,PCA_motivations[,1:3],by="ID")%>%
  full_join(.,PCA_location[,1:3],by="ID")%>%
  full_join(.,PCA_experience[,1:3],by="ID")%>%
  full_join(.,PCA_id[,1:3],by="ID")%>%
  janitor::clean_names(.)

#check which columns have NAs
apply(allpcaDF,2,function(x)sum(is.na(x)))
allpcaDF <- na.omit(allpcaDF)
nrow(allpcaDF)

#for second option
names(allpcaDF) <- c("id","active searches","using traps","interesting species (active)", "common species (active)","rare species(opportunistic)","many individuals (opportunistic)", "spend time outdoors","support conservation","visit protected areas","visit non-green urban","membership","frq activity","use ID guide","not report")


#pca analysis
pca_rotated <- principal(apply(allpcaDF,2,scale)[,-1], 
                         rotate="varimax", nfactors=2, scores=TRUE)
summary(pca_rotated)
biplot(pca_rotated)
loadings(pca_rotated)
main_pca <- plotPCA2(pca_rotated)+
  theme_classic()
main_pca

ggsave("plots/PCA_toptwo.png",width=5,height=3.5)

# Loadings:
#   RC1    RC2   
# active_searches                   0.592       
# using traps                       0.525 -0.166
# interesting species (active)     -0.433  0.198
# common species (active)          -0.293       
# rare_species(opportunistic)      -0.116  0.741
# many individuals (opportunistic)         0.606
# spend_time_outdoors              -0.461  0.198
# support_conservation                     0.111
# visit_protected_areas             0.351  0.440
# visit_non-green_urban            -0.138       
# membership                        0.562  0.149
# frq_activity                      0.408  0.439
# use_ID_guide                     -0.152  0.406
# not report                       -0.129  0.279
# 
# RC1   RC2
# SS loadings    1.798 1.693
# Proportion Var 0.128 0.121
# Cumulative Var 0.128 0.249

### cluster analysis ####

centre <- function(x) (x - min(x))/(max(x)-min(x))
mydata <- data.frame(apply(allpcaDF,2,centre))[,-1]

# K-Means Cluster Analysis

wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:10) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

fit <- kmeans(mydata, 3) 
# get cluster means
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata$groups <- fit$cluster

# Ward Hierarchical Clustering

# d <- dist(mydata, method = "euclidean") # distance matrix
# fit <- hclust(d, method="ward.D2")
# plot(fit) # display dendogram
# groups <- cutree(fit, k=3) # cut tree into 5 clusters
# # draw dendogram with red borders around the 5 clusters
# rect.hclust(fit, k=3, border="red") 
# mydata$groups <- as.numeric(groups)

library(ggradar)

groupSummary <- mydata %>%
  #mutate(across(!"groups",centre)) %>%
  group_by(groups) %>%
  summarise(across(everything(),mean))

radar_data <- cbind(Group=1:nrow(groupSummary),groupSummary[,-1])
radar_data<- radar_data[,c("Group","support.conservation","spend.time.outdoors",
                            "membership","frq.activity",
                            "active.searches","using.traps",
                            "interesting.species..active.","common.species..active.",
                            "rare.species.opportunistic.","many.individuals..opportunistic.",
                            "visit.protected.areas","visit.non.green.urban",
                            "use.ID.guide","not.report")]
                            

radar_data_subset <- radar_data
radar1 <- ggradar(
  radar_data_subset, 
  values.radar = c("0", "0.5", "1"),
  grid.min = 0, grid.mid = 0.5, grid.max = 1,
  background.circle.colour = "white",
  gridline.mid.colour = "grey",
  group.colours = wes_palette("Darjeeling1", 
                              type="continuous", n=nrow(radar_data_subset)),
  axis.labels  = c("support\n conservation","spend time\n outdoors",
                   "membership","frq activity",
                   "active search","using traps",
                   "interesting species\n (active)","common species\n (active)",
                   "rare species\n (opportunistic)","many individuals\n (opportunistic)",
                   "visit protected\nareas","visit non-green",
                   "use ID guide","not report"),
  group.line.width = 1.5,
  group.point.size = 2.5,
  legend.position = "none",
  axis.label.size = 2.5,
  grid.label.size = 2.5,
)

radar1

ggsave(file="plots/radar1.png",width=5,height=3.5)

### combine ####

plot_grid(main_pca,
          radar1,
          ncol=1,
          scale=c(0.7,1.1),
          labels=c("a)","b)")
          )


### not used ####
### taxa analysis ####

unique(sample_data$Taxa)

#get birdDF
birdDF <- subset(sample_data,Taxa="Vögel")

#get plantsDF
plantDF <- subset(sample_data,Taxa="Pflanzen")

#get insectDF
insectDF <- subset(sample_data,Taxa %in% c("Libellen","Schmetterlinge","Käfer","Bienen"))

#get amphibians
frogsDF <- subset(sample_data,Taxa="Amphibien/Reptilien")

#### taxa pca per question ####

## make a common theme for plots
theme_pca <- theme_classic()+
  theme(plot.title = element_text(size = 15, color = "black"),
        axis.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.8),
        axis.title.x = element_text(size = 15, color = "black"),   
        axis.title.y = element_text(size = 15, color = "black"))


doTaxaPCA <- function(sample_data){
  
  #q1
  sample_data_Q <- sample_data[,grepl("gegangen_sind_wie_sind_Sie_bei_der_Sammlung_von_Beobachtungen_vorgegangen",names(sample_data))]
  sample_data_Q <- format4PCA(sample_data_Q)
  #removed first part of the question to make it generic for all taxa
  names(sample_data_Q) <- sapply(names(sample_data_Q),function(x)strsplit(x,"_")[[1]][2])
  sample_data_Q <- na.omit(sample_data_Q)
  names(sample_data_Q) <- c("complete checklist","all species","interesting species","common species","rare species")
  
  fit <-  prcomp(sample_data_Q, scale = TRUE)
  q1 <- fviz_pca_biplot(fit, 
                        # Fill individuals by groups
                        geom.ind = "point",
                        pointshape = 21,
                        pointsize = 0.1,
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
  sample_data_Q <- format4PCA(sample_data_Q)
  names(sample_data_Q) <- sapply(names(sample_data_Q),function(x)strsplit(x,"_")[[1]][3])
  sample_data_Q <- na.omit(sample_data_Q)
  names(sample_data_Q) <- c("rare species","many species at same time", "unexpected species",
                            "first time of year","unknown species","many indivdiduals at the same time",
                            "interesting species")
  fit <-  prcomp(sample_data_Q, scale = TRUE)
  
  q2 <- fviz_pca_biplot(fit, 
                        # Fill individuals by groups
                        geom.ind = "point",
                        pointshape = 21,
                        pointsize = 0.1,
                        fill.ind = "black",
                        col.ind = "black",
                        col.var = "#225ea8",
                        repel = TRUE,
                        labelsize = 5,
                        arrowsize = 0.1,
                        title = "What triggers the reporting of an opportunistic observation?")
  q2 <- ggpubr::ggpar(q2,
                      xlab = "PC1", ylab = "PC2",
                      ggtheme = theme_pca
  )
  #q3
  sample_data_Q <- sample_data[,grepl("wenn_Sie_sich_bei_der_Bestimmung",names(sample_data))]
  sample_data_Q <- format4PCA(sample_data_Q)
  names(sample_data_Q) <- sapply(names(sample_data_Q),function(x)strsplit(x,"_")[[1]][2])
  sample_data_Q <- na.omit(sample_data_Q)
  names(sample_data_Q) <- c("guess","not report","report at higher taxa level",
                            "ask another person","use an identification guide")
  
  fit <-  prcomp(sample_data_Q, scale = TRUE)
  q3 <- fviz_pca_biplot(fit, 
                        # Fill individuals by groups
                        geom.ind = "point",
                        pointsize = 0.1,
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
  sample_data_Q <- format4PCA(sample_data_Q)
  names(sample_data_Q) <- sapply(names(sample_data_Q),function(x)strsplit(x,"_")[[1]][2])
  sample_data_Q <- na.omit(sample_data_Q)
  names(sample_data_Q) <- c("protected areas","forest","wetland/water bodies","meadows",
                            "farmland","green urban","non-green urban","remote areas")
  
  fit <-  prcomp(sample_data_Q, scale = TRUE)
  q4 <- fviz_pca_biplot(fit, 
                        # Fill individuals by groups
                        geom.ind = "point",
                        pointshape = 21,
                        pointsize = 0.1,
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

### saving the plot

bird_pca_plot <- ggsave(a, filename = "plots/bird_pca_plot.png",width = 13, height = 8, units = "in" )
insect_pca_plot <- ggsave(b, filename = "plots/insect_pca_plot.png",width = 13, height = 8, units = "in" )
plant_pca_plot <- ggsave(c, filename = "plots/plant_pca_plot.png",width = 13, height = 8, units = "in" )
frog_pca_plot <- ggsave(d, filename = "plots/frog_pca_plot.png",width = 13, height = 8, units = "in" )

#### end ####
