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


### load dataset ###

survey_data <- read.csv("data/results-survey796935.csv",encoding = "UTF-8")

#the following line Diana needed to do so that Netras script worked:
names(survey_data) <- sapply(names(survey_data), function(x)gsub("\\.","_",x))

#### function #####

#replace ordinal scale to a numeric scale

ordinal_fn <- function(x){
  x <- x %>%  mutate_all(funs(case_when(. == "sehr oft" ~ "5" ,
                                        . ==  "oft" ~ "4" ,
                                        . ==  "manchmal" ~ "3" ,
                                        . ==  "selten" ~ "2",
                                        . ==  "nie" ~ "1",
                                        . ==  "wei? nicht" ~ "0",
                                        . == "sehr wichtig" ~ "5",
                                        . == "wichtig" ~ "4",
                                        . == "mäßig wichtig" ~ "3",
                                        . == "wenig wichtig" ~ "2",
                                        . == "überhaupt nicht wichtig" ~ "1",
                                        . ==   "alle"~ "5",
                                        . ==   "die meisten" ~ "4",
                                        . ==   "einige" ~ "3",
                                        . ==   "wenige" ~ "2",
                                        . ==   "keine" ~ "1",
                                        . ==  "?berhaupt nicht wahrscheinlich" ~ "1",
                                        . ==  "wenig wahrscheinlich" ~ "2" ,
                                        . ==  "m??ig wahrscheinlich" ~ "3" ,
                                        . ==  "ziemlich wahrscheinlich" ~ "4",
                                        . ==  "sehr wahrscheinlich" ~ "5",
                                        . ==  "Ja" ~ "1",
                                        . ==   "Nein" ~ "0"
                                        )))
  
  return(x)
  
}

### subset to people completing most of the survey ####

pca_data <- survey_data %>% filter(Letzte_Seite == 12 | Letzte_Seite == 13)
colnames(pca_data)[1]<- "ID"

### function to pull data for a specific taxa ####

getTaxaData <- function(myTaxa){ 
  
pca_data <- pca_data[grepl(paste0(myTaxa,collapse="|"), pca_data$Bitte_w?hlen_Sie_EINE_Artengruppe__),]
taxa <- data.frame(pca_data[,grepl(paste0(myTaxa,collapse="|"), names(survey_data))])

### #get other question (without taxa in question) ####

otherQuestions <- c("Stellen_Sie_sich_vor__Sie_besuchen_einen_Ort",
                    "wie_oft_haben_Sie_an_den_folgenden_Orte")

#get columns with these headings
other_data <- pca_data[,grepl(paste0(otherQuestions,collapse="|"),names(pca_data))]
 
### combine all questions ####

pca_data <- cbind(taxa,other_data)

### clean data for PCA ####

pca_data <- as.data.frame(lapply(pca_data, factor)) 
pca_data <- ordinal_fn(pca_data)
sample_data <- mutate_all(pca_data, function(x) as.numeric(as.character(x)))


return(sample_data)

}

### get taxa data frames ####

#get birdDF
birdDF <- getTaxaData(c("Vogel","V?gel"))

#get plantsDF
plantDF <- getTaxaData("Pflanzen")

#get insectDF
butterflyDF <- getTaxaData("Schmetterl") %>% add_column(taxa = "Butterfly")
dragonflyDF <- getTaxaData("Libellen")%>% add_column(taxa = "Dragonfly")
beetleDF <- getTaxaData(c("K?fer"))%>% add_column(taxa = "Beetle")
beeDF <- getTaxaData("Bienen")%>% add_column(taxa = "Bee")

#renaming taxa names to "insect" to make a common dataframe
#then replace the colnames with new names 
colnames(butterflyDF) <- gsub("Schmetterling", "insect", colnames(butterflyDF))
colnames(butterflyDF) <- str_replace_all(colnames(butterflyDF),
                                         c("insecten" = "insect",
                                           "insects" = "insect"))
colnames(beetleDF) = colnames(butterflyDF)
colnames(dragonflyDF) = colnames(butterflyDF)
colnames(beeDF) = colnames(butterflyDF)

insectDF <- rbind(butterflyDF,dragonflyDF,beetleDF,beeDF)

#get amphibians
frogsDF <- getTaxaData("Amphibien")

#combine all together
allDF <- rbind(frogDF,insectDF,butterflyDF,birdDF)

## make a common theme for plots
theme_pca <- theme_classic()+
             theme(plot.title = element_text(size = 20, color = "black"),
                   axis.text.x = element_text(size = 10, face = "bold"),
                   axis.text.y = element_text(size = 10, face = "bold"),
                   panel.border = element_rect(colour = "black", fill=NA, size=0.8),
                   axis.title.x = element_text(size = 20, color = "black"),   
                   axis.title.y = element_text(size = 20, color = "black"))


### function for pca per question ####

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

motivationsDF <- pca_data[,10:18]
motivationsDF <- ordinal_fn(motivationsDF)
motivationsDF <- mutate_all(motivationsDF, function(x) as.numeric(as.character(x)))
names(motivationsDF) <- sapply(names(motivationsDF),function(x)strsplit(x,"_____")[[1]][2])
names(motivationsDF) <- c("improve species knowledge",
                          "contribute to science",
                          "support conservation",
                          "spend time outdoors",
                          "physical activity",
                          "protect a specific place",
                          "gain local knowledge",
                          "meet other people",
                          "have fun exploring")

# #fit PCA
# fit <-  prcomp(motivationsDF, scale = TRUE)
# 
# fviz_eig(fit)
# 
# biplot(fit)
# 
# fviz_pca_biplot(fit, 
#                 col.var = "contrib",
#                 gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#                 title = "Motivations to collect species data")


#rotated PCA - maximizes the loading of each axis onto x and y
#https://stats.stackexchange.com/questions/59213/how-to-compute-varimax-rotated-principal-components-in-r

library(psych)
pca_rotated <- principal(motivationsDF, rotate="varimax", nfactors=2, scores=TRUE)
biplot(pca_rotated)
summary(pca_rotated)
loadings <- as.data.frame(unclass(pca_rotated$loadings))

#using different package:
#pca_rotated <- princomp(motivationsDF)
#biplot(pca_rotated)
#loadings<-as.data.frame(unclass(loadings(pca_rotated)))

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
motivationsDF$ID <- pca_data$ID
motivationsDF$scores1 <- pca_rotated$scores[,1]
motivationsDF$scores2 <- pca_rotated$scores[,2]
saveRDS(motivationsDF,file="model-outputs/motivationsDF.rds")

