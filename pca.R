# PCA ---------------------------------------------------------------------

#library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
library(dplyr)
library(tidyverse)
library(stringr)
library(gridExtra)
library(ggrepel)

### load dataset ###

survey_data <- read.csv("results-survey796935.csv",encoding = "UTF-8")

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
                                        . ==  "weiß nicht" ~ "0",
                                        . ==   "alle"~ "5",
                                        . ==   "die meisten" ~ "4",
                                        . ==   "einige" ~ "3",
                                        . ==   "wenige" ~ "2",
                                        . ==   "keine" ~ "1",
                                        . ==  "überhaupt nicht wahrscheinlich" ~ "1",
                                        . ==  "wenig wahrscheinlich" ~ "2" ,
                                        . ==  "mäßig wahrscheinlich" ~ "3" ,
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
  
pca_data <- pca_data[grepl(paste0(myTaxa,collapse="|"), pca_data$Bitte_wählen_Sie_EINE_Artengruppe__),]
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
birdDF <- getTaxaData(c("Vogel","Vögel"))

#get plantsDF
plantDF <- getTaxaData("Pflanzen")

#get insectDF
butterflyDF <- getTaxaData("Schmetterl") %>% add_column(taxa = "Butterfly")
dragonflyDF <- getTaxaData("Libellen")%>% add_column(taxa = "Dragonfly")
beetleDF <- getTaxaData(c("Käfer"))%>% add_column(taxa = "Beetle")
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

insectDF <- rbind(butterflyDF,dragonflyDF,beetleDF,beeDF)# you need to fix the headings somehow....

#get amphibians
frogsDF <- getTaxaData("Amphibien")



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
names(sample_data_Q) <- c("complete_checklist","all_species","interesting_species","common_species",
                          "rare_species")
fit <-  prcomp(sample_data_Q, scale = TRUE)
q1 <- ggbiplot(fit, obs.scale = 1,
               var.scale = 1,
               var.axes = T)+
      theme_pca+
      ggtitle("Which species are reported during an active search?")+ #changed reporting to reported
      coord_equal(ratio = 0.5)# we use this to make plots wider
      
#q2
sample_data_Q <- sample_data[,grepl("Was_veranlasst_Sie_dazu",names(sample_data))]
names(sample_data_Q) <- sapply(names(sample_data_Q),function(x)strsplit(x,"___")[[1]][3])
sample_data_Q <- na.omit(sample_data_Q)
names(sample_data_Q) <- c("rare_species","many_species_at_same_time", "unexpected_species",
                          "first_time of_year","unknown_species","many_indivdiduals_at_the_same_time",
                          "interesing_species")
fit <-  prcomp(sample_data_Q, scale = TRUE)

q2 <- ggbiplot(fit, obs.scale = 1,
               var.scale = 1,
               var.axes = T)+
      theme_pca+
      ggtitle("What triggers the reporting of an incidental observation?")+
      coord_equal(ratio = 0.5)

#q3
sample_data_Q <- sample_data[,grepl("wenn_Sie_sich_bei_der_Bestimmung",names(sample_data))]
names(sample_data_Q) <- sapply(names(sample_data_Q),function(x)strsplit(x,"___")[[1]][2])
sample_data_Q <- na.omit(sample_data_Q)
names(sample_data_Q) <- c("guess","not_reported","report_at_higher_taxa_level",
                          "ask_another_person","use_an_identification guide")

fit <-  prcomp(sample_data_Q, scale = TRUE)
q3 <- ggbiplot(fit, obs.scale = 1, 
               var.scale = 1,
               var.axes = T)+
      theme_pca+
      ggtitle("How do people deal with species identification uncertainity?") #changed ID to identification
      
#q4
sample_data_Q <- sample_data[,grepl("wie_oft_haben_Sie_an_den_folgenden_Orten_nach_Arten",names(sample_data))]
names(sample_data_Q) <- sapply(names(sample_data_Q),function(x)strsplit(x,"___")[[1]][2])
sample_data_Q <- na.omit(sample_data_Q)
names(sample_data_Q) <- c("protected_areas","forest","wetland/water_bodies","meadows",
                          "agricultural_land","green_urban","non-green_urban","remote_areas")

fit <-  prcomp(sample_data_Q, scale = TRUE)
q4 <- ggbiplot(fit, obs.scale = 1,
               var.scale = 1,
               var.axes = T)+
      theme_pca+
      ggtitle("What places do people visit?")+
      coord_equal(ratio = 0.5)
      

grid.arrange(q1,q2,q3,q4)

}

### apply function ###

a <- doTaxaPCA(birdDF)
b <- doTaxaPCA(insectDF)
c <- doTaxaPCA(plantDF)
d <- doTaxaPCA(frogsDF)

### make a panel plot (4*4)
pca_panel <- cowplot::plot_grid(a,b,c,d,labels = "auto", nrow = 4, ncol = 4)
#ggsave(pca_panel, filename = "pca_panel.png", width = 30, height = 15, units = "in")
