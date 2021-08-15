# PCA ---------------------------------------------------------------------

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

source('helper_functions.R', encoding = 'UTF-8')

### read in a cleaned data frame ###

### load dataset ###

##trial with cleaned-data (keeping variable name same here so as not to make multiple changes below)
sample_data <- readRDS("cleaned-data/clean_data.RDS")
#saveRDS(sample_data[,c("ID","Taxa")],file="ID_to_Taxa.rds")

### taxa analysis ####
#### get taxa data frames ####

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
  names(sample_data_Q) <- c("complete checklist","all species","interesting species","common species",
                            "rare species")
  
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
                            "interesing species")
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
                        title = "What triggers the reporting of an incidental observation?")
  q2 <- ggpubr::ggpar(q2,
                      xlab = "PC1", ylab = "PC2",
                      ggtheme = theme_pca
  )
  #q3
  sample_data_Q <- sample_data[,grepl("wenn_Sie_sich_bei_der_Bestimmung",names(sample_data))]
  sample_data_Q <- format4PCA(sample_data_Q)
  names(sample_data_Q) <- sapply(names(sample_data_Q),function(x)strsplit(x,"_")[[1]][2])
  sample_data_Q <- na.omit(sample_data_Q)
  names(sample_data_Q) <- c("guess","not reported","report at higher taxa level",
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
                            "agricultural land","green urban","non-green urban","remote areas")
  
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

#@DIANA how should I arrange the plots? 
#below is just all plots in panel 
#we can also make use of different colors for 4 taxa groups 
#or we make 4 separate plots 

#plist <- list(a,b,c,d)

#pca_panel <- cowplot::plot_grid(plotlist = plist, ncol = 2)
#ggsave("pca_panel.png", filename = pca_panel)

### across all species ####

#### motivations PCA ####

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
loadings(pca_rotated)
p1 <- plotPCA(pca_rotated)

p1#identify variable loading strongly on each axis - of most interest given the top two
#axis 1 = spend time outdoors
#axis 2 = support conservation
PCA_motivations <- motivationsDF[,c("ID","spend time outdoors","support conservation")]

#save scores and person ID
#motivationsDF$scores1 <- pca_rotated$scores[,1]
#motivationsDF$scores2 <- pca_rotated$scores[,2]
#saveRDS(motivationsDF,file="model-outputs/motivationsDF.rds")

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
p2 <- plotPCA(pca_rotated)

#identify variable loading most strongly on each axis
#axis 1 = interesting species
#axis 2 = common species
PCA_active <- activeDF[,c("ID","all species","common species")]


#### incidental PCA ####

incidentalDF <- sample_data[,c(1,grep("Was_veranlasst_Sie_dazu",names(sample_data)))]
#removed first part of the question to make it generic for all taxa

incidentalDF <- format4PCA(incidentalDF)

names(incidentalDF) <- gsub("Was_veranlasst_Sie_dazu_eine_Beobachtung_zu_melden_wenn_Sie_eine_Art_zufallig_sehen_ohne_aktive_Suche_Bitte_geben_Sie_an_wie_oft_die_folgenden_Grunde_bei_Ihnen_zur_Meldung_einer_Beobachtung_fuhren_","",names(incidentalDF))

incidentalDF <- na.omit(incidentalDF)

names(incidentalDF)[-1] <- c("rare species","many species at same time", "unexpected species",
                          "first time of year","unknown species","many indivdiduals at the same time",
                          "interesting species")
#rotated PCA
pca_rotated <- principal(incidentalDF[,-1], rotate="varimax", nfactors=2, scores=TRUE)
summary(pca_rotated)
loadings(pca_rotated)
p3 <- plotPCA(pca_rotated)

#identify variable loading most strongly on each axis
#axis 1 = interesting species
#axis 2 = many indivdiduals at the same time
PCA_incidental <- incidentalDF[,c("ID","rare species","many indivdiduals at the same time")]

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
p4 <- plotPCA(pca_rotated)

#identify variable loading most strongly on each axis
#axis 1 = rare species
#axis 2 = all species

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
p5 <- plotPCA(pca_rotated)

#identify variable loading most strongly on each axis
#axis 1 = protected areas
#axis 2 = non-green urban
PCA_location <- locationDF[,c("ID","protected areas","non-green urban")]

#### experience PCA ####

experienceDF <- sample_data[,c("ID","Wie_viele_Jahre_sind_Sie_schon_in_der_Erfassung_der_Artenbeobachtungsdaten_aktiv_","Wie_oft_haben_Sie_im_Fruhling_oder_Sommer_2020_Artdaten_gesammelt_",
                                "Nehmen_Sie_an_einem_gross_angelegten_standardisierten_Monitoringsystem_teil_z_B_Tagfalter_Monitoring_Deutschland_",
                                "Besitzen_Sie_Fachkenntnisse_im_Bereich_des_Biodiversitatsmonitorings_",
                                "Sind_Sie_Mitglied_in_einer_Fachgesellschaft_fur_eine_bestimmte_Artengruppe_z_B_GdO_GAC_DDA_etc_")]

#removed first part of the question to make it generic for all taxa

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
p7 <- plotPCA(pca_rotated)

#identify variable loading most strongly on each axis
#axis 1 = use an identification guide
#axis 2 = not report
PCA_id <- idDF[,c("ID","use an identification guide","not report")]

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
p8 <- plotPCA(pca_rotated)

#identify variable loading most strongly on each axis
#axis 1 = opportunistic
#axis 2 = using traps
PCA_survey <- surveytypeDF[,c("ID","active search","using traps")]

### combine PCA plots

library(cowplot)
plot_grid(p1,p2,p3,p4,nrow=2)
plot_grid(p5,p6,p7,p8,nrow=2)

#### top two analysis ####

#combine the top two from previous question groups
#identify main axes of variation

#first main axis
allpcaDF <- PCA_survey[,1:2] %>%
  full_join(.,PCA_active[,1:2],by="ID")%>%
  full_join(.,PCA_incidental[,1:2],by="ID")%>%
  full_join(.,PCA_motivations[,1:3],by="ID")%>%
  full_join(.,PCA_location[,1:2],by="ID")%>%
  full_join(.,PCA_experience[,1:3],by="ID")%>%
  full_join(.,PCA_id[,1:2],by="ID")%>%
  janitor::clean_names(.)

#check which columns have NAs
apply(allpcaDF,2,function(x)sum(is.na(x)))
allpcaDF <- na.omit(allpcaDF)
nrow(allpcaDF)

#tidy names
names(allpcaDF) <- c("id","active_searches","all_species(active)",
                     "rare_species(opportunistic)","spend_time_outdoors",
                     "support_conservation","protected_areas","member",
                     "frq_activity","use_ID_guide")

#pca analysis
pca_rotated <- principal(apply(allpcaDF,2,scale)[,-1], 
                         rotate="varimax", nfactors=2, scores=TRUE)
summary(pca_rotated)
biplot(pca_rotated)
loadings(pca_rotated)
main_pca <- plotPCA(pca_rotated)+
  theme_classic()
main_pca

# Loadings:
#   RC1    RC2   
# active_search                0.631 -0.124
# all_species                  0.434 -0.243
# rare_species                        0.710
# spend_time_outdoors         -0.347  0.371
# support_conservation         0.125       
# protected_areas              0.444  0.445
# member                       0.547       
# frq                          0.621  0.255
# use_an_identification_guide         0.648
# 
# RC1   RC2
# SS loadings    1.611 1.404
# Proportion Var 0.179 0.156
# Cumulative Var 0.179 0.335

### cluster analysis ####

#centre <- function(x) (x - median(x))
centre <- function(x) (x - min(x))/(max(x)-min(x))
#centre <- function(x) (x - median(x))/IQR(x)

mydata <- data.frame(apply(allpcaDF,2,centre))[,-1]

# K-Means Cluster Analysis

wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:10) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

fit <- kmeans(mydata, 4) 
# get cluster means
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata$groups <- fit$cluster

# Ward Hierarchical Clustering

d <- dist(mydata, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D2")
plot(fit) # display dendogram
groups <- cutree(fit, k=4) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=4, border="red") 
mydata$groups <- as.numeric(groups)

#boxplots:
groupSummary <- mydata %>%
  mutate(across(!"groups",centre)) %>%
  pivot_longer(!groups,names_to = "behaviour", values_to = "mean")

ggplot(groupSummary)+
  geom_boxplot(aes(x=behaviour,y=mean))+
  coord_flip()+
  geom_hline(yintercept=0)+
  theme_few()+
  facet_wrap(~groups,nrow=1)+
  ylab("scaled mean")

#star plots

groupSummary <- mydata %>%
  mutate(across(!"groups",centre)) %>%
  pivot_longer(!groups,names_to = "behaviour", values_to = "values") %>%
  group_by(groups, behaviour) %>%
  summarise(med = mean(values))

ggplot(groupSummary)+
  geom_col(aes(x=behaviour,y=med, fill=behaviour))+
  coord_polar()+
  facet_wrap(~groups)+
  theme_minimal()+
  theme(legend.position = "none")

#radar plots

groupSummary <- mydata %>%
  mutate(across(!"groups",centre)) %>%
  group_by(groups) %>%
  summarise(across(everything(),mean))
  
# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
data <-rbind(rep(1,(ncol(groupSummary)-1)), 
             rep(0,(nrow(groupSummary)-1)), 
             arrange(groupSummary,desc(frequency_activity))[,-1])

# Prepare color
colors_border=c( rgb(0.8,0.2,0.5,0.9),
                rgb(0.2,0.5,0.5,0.9), 
                 rgb(0.2,0.2,0.5,0.9), 
                 rgb(0.2,0.8,0.5,0.9))

colors_in=c( rgb(0.8,0.2,0.5,0.15),
                 rgb(0.2,0.5,0.5,0.15), 
                 rgb(0.2,0.2,0.5,0.15), 
                 rgb(0.2,0.8,0.5,0.15))

#set margins
par(xpd = TRUE)

#radar chart for multiple
fmsb::radarchart(data, axistype=2, 
            
            #custom polygon
            pcol=colors_border , pfcol=colors_in, plwd=2, plty=1 , 

            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", cglwd=1,
            
            #custom labels
            vlcex=0.6 
)

# Legend

# legend(x=-1.5, y=-1.2, legend = c("High", "Mid-High","Mid-Low","Low"),
#        bty = "n", pch=20 , col=colors_border , text.col = "black", 
#        cex=0.8, pt.cex=1,
#        ncol=4)


#cant be saved as an object

#try ggradar
library(ggradar)

radar_data <- cbind(Group=1:4,data[-c(1:2),])
radar_data<- radar_data[,c("Group","member","frequency_activity",
                           "support_conservation","spend_time_outdoors",
                           "use_ID_guide","protected_areas",
                           "active_searches","all_species.active.",
                           "rare_species.opportunistic.")]

radar1 <- ggradar(
  radar_data, 
  values.radar = c("0", "0.5", "1"),
  grid.min = 0, grid.mid = 0.5, grid.max = 1,
  group.colours = colors_border,
  axis.labels  = c("membership","frequency\n activity","support \nconservation",
                   "spend time\noutdoors","use ID guide", "visit\nprotected areas",
                   "conduct\nactive searches", "all species\n(active survey)",
                    "rare species\n(opportunistic survey)"),
  group.line.width = 1,
  group.point.size = 1.5,
  legend.position = "none",
  axis.label.size = 2.5,
  grid.label.size = 2,
)

radar1

plot_grid(main_pca,radar1,ncol=2,
          scale=c(0.9,1.1))

#### end ####
