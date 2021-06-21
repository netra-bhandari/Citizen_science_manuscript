#script to format the data frame to align taxa-specific questions in the same columns


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

### load dataset ###

survey_data <- read.csv("data/results-survey796935.csv",encoding = "UTF-8")

#the following line Diana needed to do so that Netras script worked:
names(survey_data) <- sapply(names(survey_data), function(x)gsub("\\.","_",x))

### subset to people completing most of the survey ####

pca_data <- survey_data %>% filter(Letzte_Seite == 12 | Letzte_Seite == 13)
colnames(pca_data)[1]<- "ID"

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

