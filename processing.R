#script to format the data frame to align taxa-specific questions in the same columns

library(dplyr)
library(tidyverse)
library(stringr)
library(gridExtra)
library(ggrepel)
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
  
  pca_data <- pca_data[grepl(paste0(myTaxa,collapse="|"), pca_data$Bitte_wählen_Sie_EINE_Artengruppe__),]
  taxa <- data.frame(pca_data[,grepl(paste0(myTaxa,collapse="|"), names(survey_data))])
  
  #remove the taxa name from the question
  
  
  ### #get other question (without taxa in question) ####
  
  otherQuestions <- c("Wie_viele_Jahre_sind_Sie_schon_in_der_Erfassung",
                      "Wie_oft_haben_Sie_im_Frühling_oder_Sommer_2020",
                      "Wie_wichtig_waren_Ihnen_die_folgenden_Aspekte",
                      "Bitte_wählen_Sie_EINE_Artengruppe_",
                      "Erfassen_Sie_vorrangig_Beobachtungsdaten",
                      "Falls_Ja___Bitte_spezifizieren_Sie",
                      "War_Ihre_Artenbeobachtung_oder__berichterstattung_im_Frühling_Sommer_2020_aufgrund_der_Corona_Situation",
                      "Stellen_Sie_sich_vor__Sie_besuchen_einen_Ort",
                      "wie_oft_haben_Sie_an_den_folgenden_Orte",
                      "Ich bin",
                      "Zu_welcher_Altersklasse_gehören_Sie_",
                      "Nehmen_Sie_an_einem_groß_angelegten_standardisierten_Monitoringsystem_teil",
                      "Besitzen_Sie_Fachkenntnisse",
                      "Wo_haben_Sie_die_Fachkenntnisse",
                      "Sind_Sie_Mitglied_in_einer_Fachgesellschaft",
                      "Wie_lauten_die_ersten_zwei_Ziffern")
  
  #get columns with these headings
  other_data <- pca_data[,grepl(paste0(otherQuestions,collapse="|"),names(pca_data))]
  
  ### combine all questions ####
  
  pca_data <- cbind(taxa,other_data)
  
  return(pca_data)
  
}

### get taxa data frames ####

birdDF <- getTaxaData(c("Vogel","V?gel","Vögel"))
plantDF <- getTaxaData("Pflanzen")
butterflyDF <- getTaxaData("Schmetterl") 
dragonflyDF <- getTaxaData("Libellen")
beetleDF <- getTaxaData(c("K?fer","Käfer"))
beeDF <- getTaxaData("Bienen")
frogsDF <- getTaxaData("Amphibien")


#make all have the same set of headings

#to be done...

#combine all together
allDF <- rbind(frogDF,insectDF,butterflyDF,birdDF)

#still need to sort the "others" out

