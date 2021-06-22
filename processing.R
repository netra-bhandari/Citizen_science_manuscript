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

survey_data <- survey_data %>% filter(Letzte_Seite == 12 | Letzte_Seite == 13)
colnames(survey_data)[1]<- "ID"
#write.csv(data.frame(Questions=names(survey_data)),file="Survey_columns.csv",row.names = FALSE)

#remove all German characters
names(survey_data) <- sapply(names(survey_data),function(x)iconv(x, to='ASCII//TRANSLIT'))
names(survey_data) <- gsub("\\?","ss",names(survey_data))
names(survey_data) <- gsub("ß","ss",names(survey_data))

### Main taxa-group function ######################################################

getTaxaData <- function(survey_data,myTaxa){ 
  
  #get rows for the taxa
  survey_data_taxa <- survey_data[grepl(paste0(myTaxa,collapse="|"), survey_data$Bitte_wahlen_Sie_EINE_Artengruppe__),]
  
  #get columns for the taxa (with taxa specific questions)
  survey_data_taxa_questions <- data.frame(survey_data_taxa[,grepl(paste0(myTaxa,collapse="|"), names(survey_data))])
  
  #remove the taxa name from the question
  names(survey_data_taxa_questions) <- gsub(paste0(myTaxa,collapse="|"),"",names(survey_data_taxa_questions))
  
  ### #get other question (without taxa in question) ####
  
  otherQuestions <- c("Wie_viele_Jahre_sind_Sie_schon_in_der_Erfassung",
                      "Wie_oft_haben_Sie_im_Fruhling_oder_Sommer_2020",
                      "Wie_wichtig_waren_Ihnen_die_folgenden_Aspekte",
                      "Bitte_wahlen_Sie_EINE_Artengruppe_",
                      "Erfassen_Sie_vorrangig_Beobachtungsdaten",
                      "Falls_Ja___Bitte_spezifizieren_Sie",
                      "War_Ihre_Artenbeobachtung_oder__berichterstattung_im_Fruhling_Sommer_2020_aufgrund_der_Corona_Situation",
                      "Stellen_Sie_sich_vor__Sie_besuchen_einen_Ort",
                      "wie_oft_haben_Sie_an_den_folgenden_Orte",
                      "Ich bin",
                      "Zu_welcher_Altersklasse_gehoren_Sie_",
                      "Nehmen_Sie_an_einem_gross_angelegten_standardisierten_Monitoringsystem_teil",
                      "Besitzen_Sie_Fachkenntnisse",
                      "Wo_haben_Sie_die_Fachkenntnisse",
                      "Sind_Sie_Mitglied_in_einer_Fachgesellschaft",
                      "Wie_lauten_die_ersten_zwei_Ziffern")
  
  #first questions
  survey_data_first <- survey_data_taxa[,1:7]
    
  #get columns with these headings
  other_questions <- survey_data_taxa[,grepl(paste0(otherQuestions,collapse="|"),names(survey_data_taxa))]
  
  ### combine all questions ####
  temp <- cbind(survey_data_first,survey_data_taxa_questions,other_questions)
  
  #trim all double spaces
  names(temp) <- gsub("____","__",names(temp))
  names(temp) <- gsub("___","__",names(temp))
  names(temp) <- gsub("__","_",names(temp))
  return(temp)

}

### get taxa data frames ####

birdDF <- getTaxaData(survey_data,c("Vogel"))
plantDF <- getTaxaData(survey_data,c("Pflanzen","Planzen"))
butterflyDF <- getTaxaData(survey_data,c("Schmetterlinge","Schmetterling","Schmetterlings","Schmetterlingen")) 
dragonflyDF <- getTaxaData(survey_data,c("Libellen","Libelle"))
beetleDF <- getTaxaData(survey_data,c("Kafer"))
beeDF <- getTaxaData(survey_data,"Bienen")
frogDF <- getTaxaData(survey_data,c("Amphibien/Reptilien","Amphibien_Reptilien","Amphibien___Reptilien"))


#check column headings are the same
all(names(butterflyDF)==names(dragonflyDF))
all(names(beetleDF)==names(dragonflyDF))
all(names(beeDF)==names(dragonflyDF))
all(names(frogDF)==names(dragonflyDF))

#differences for birds and plants
names(birdDF)[!names(birdDF) %in% names(beeDF)]
#these are because the bird question had more options for the platforms

#combine all together
taxaDF <- bind_rows(birdDF,plantDF,butterflyDF,dragonflyDF,beetleDF,beeDF,frogDF)

### Other taxa questions ######################################################

getOtherTaxaData <- function(survey_data){ 
  
  #get rows for the other taxa
  survey_data_taxa <- subset(survey_data,Bitte_wahlen_Sie_EINE_Artengruppe__=="Sonstiges")
  
  #get columns for the taxa (with taxa specific questions)
  taxa_questions <- c("Welche_Plattform_oder_Plattformen_nutzen",
                                  "die_Sie_im_Fruhling_und_Sommer_2020_gemeldet_haben",
                                  "Wenn_Sie_aktiv_auf_die_Suche_nach",
                                  "Was_veranlasst_Sie_dazu__",
                                  "wie_lang_war_typischerweise_das_Erfassungs_Zeitfenster",
                                  "Welche_Typen_von_",
                                  "Nach_welchem_Schema_erfassen_Sie",
                                  "Was_tun_Sie__wenn_Sie_sich_bei_der_Bestimmung")
  
  survey_data_taxa_questions <- survey_data_taxa[,grepl(paste0(taxa_questions,collapse="|"),
                                                        names(survey_data_taxa))]
  
  #remove those mentioning specific taxa
  taxaNames <- c("Vogel","Pflanzen","Planzen","Schmetterlinge","Schmetterling","Schmetterlings","Schmetterlingen",
                 "Libellen","Libelle","Kafer","Bienen","Amphibien/Reptilien","Amphibien_Reptilien","Amphibien___Reptilien")
  survey_data_taxa_questions <- survey_data_taxa_questions[,!grepl(paste0(taxaNames,collapse="|"),
                                                        names(survey_data_taxa_questions))]
  
  #remove taxa names
  names(survey_data_taxa_questions) <- gsub("Ihrer_ausgewahlten_Artengruppe","",names(survey_data_taxa_questions))
  
  ### #get other question (without taxa in question) ####
  
  otherQuestions <- c("Wie_viele_Jahre_sind_Sie_schon_in_der_Erfassung",
                      "Wie_oft_haben_Sie_im_Fruhling_oder_Sommer_2020",
                      "Wie_wichtig_waren_Ihnen_die_folgenden_Aspekte",
                      "Bitte_wahlen_Sie_EINE_Artengruppe_",
                      "Erfassen_Sie_vorrangig_Beobachtungsdaten",
                      "Falls_Ja___Bitte_spezifizieren_Sie",
                      "War_Ihre_Artenbeobachtung_oder__berichterstattung_im_Fruhling_Sommer_2020_aufgrund_der_Corona_Situation",
                      "Stellen_Sie_sich_vor__Sie_besuchen_einen_Ort",
                      "wie_oft_haben_Sie_an_den_folgenden_Orte",
                      "Ich bin",
                      "Zu_welcher_Altersklasse_gehoren_Sie_",
                      "Nehmen_Sie_an_einem_gross_angelegten_standardisierten_Monitoringsystem_teil",
                      "Besitzen_Sie_Fachkenntnisse",
                      "Wo_haben_Sie_die_Fachkenntnisse",
                      "Sind_Sie_Mitglied_in_einer_Fachgesellschaft",
                      "Wie_lauten_die_ersten_zwei_Ziffern")
  
  #first questions
  survey_data_first <- survey_data_taxa[,1:7]
  
  #get columns with these headings
  other_questions <- survey_data_taxa[,grepl(paste0(otherQuestions,collapse="|"),names(survey_data_taxa))]
  
  ### combine all questions ####
  temp <- cbind(survey_data_first,survey_data_taxa_questions,other_questions)

  #trim all double spaces
  names(temp) <- gsub("____","__",names(temp))
  names(temp) <- gsub("___","__",names(temp))
  names(temp) <- gsub("__","_",names(temp))
  
  return(temp)
  
}

### get other taxa data frames ####

#for please selecting "Sonstige"

otherDF <- getOtherTaxaData(survey_data)

#check column names match
names(taxaDF)[!names(taxaDF) %in% names(otherDF)]

#missing
#Was_veranlasst_Sie_dazu_eine_Beobachtung
#Was_tun_Sie_wenn_Sie_sich_bei_der_Bestimmung
names(otherDF) <- gsub("der_Bestimmung_einer_Beobachtung","der_Bestimmung_einer_Art",names(otherDF))
names(otherDF) <- gsub("eine_Ihre_ausgewahlte_Artengruppe_Beobachtung","eine_Beobachtung",names(otherDF))
names(taxaDF)[!names(taxaDF) %in% names(otherDF)]#should be empty

#and thee reverse way
names(otherDF)[!names(otherDF) %in% names(taxaDF)]

### combine all ############

allDF <- bind_rows(taxaDF,otherDF)
saveRDS(allDF,file="cleaned-data/clean_data.RDS")

