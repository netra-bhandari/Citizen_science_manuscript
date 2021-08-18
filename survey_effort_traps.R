library(tidyverse)
library(ggridges)
library(ggthemes)

source('helper_functions.R', encoding = 'UTF-8')

### read in a cleaned data frame ###

sample_data <- readRDS("cleaned-data/clean_data.RDS")

### active search length ####

summary(sample_data$activeHrs)
summary(sample_data$activeMins)

#do we ever have data in both columns
subset(sample_data,!is.na(activeHrs) & !is.na(activeMins))
#yes but seem to be equivalent

#plot as a histogram
ggplot(sample_data)+
  geom_histogram((aes(activeMins)),bins=5)+
  facet_wrap(~Taxa)

#replacing german names with english

sample_data$Taxa <-   str_replace_all(sample_data$Taxa,
                                         c("Amphibien/Reptilien" = "Amphibians/Reptiles",
                                           "Bienen" = "Bees",
                                           "Käfer" = "Beetles",
                                           "Libellen" = "Dragonflies",
                                           "Pflanzen" = "Plants",
                                           "Schmetterlinge" = "Butterflies/Moths",
                                           "Sonstiges" = "Others",
                                           "Vögel" = "Birds") )



activity_plot <- ggplot(subset(sample_data,!is.na(activeMins)))+
  geom_density_ridges(aes(y=Taxa, x=activeMins, fill=Taxa))+
  scale_fill_brewer(type="qual", palette="Set3")+ #changed pallete from "Accent" to "Set 3" to add the 9th color in the plot
  theme_few()+
  theme(legend.position="none")+
  xlab("Activity duration (minutes)")
  
ggsave(activity_plot, filename = "plots/fig9_new.png")

sample_data %>%
  dplyr::group_by(Taxa) %>%
  dplyr::summarise(medMinutes = median(activeMins,na.rm=T))

#### trap survey length ####

summary(sample_data$trapDays)
summary(sample_data$trapHrs)

#do we ever have data in both columns
nrow(subset(sample_data,!is.na(trapDays) & !is.na(trapHrs)))
#just 2

#plot as a histogram
ggplot(sample_data)+
  geom_histogram((aes(trapDays)),bins=5)+
  facet_wrap(~Taxa)

sample_data %>%
  dplyr::group_by(Taxa) %>%
  dplyr::summarise(medMinutes = median(trapDays,na.rm=T))

#### trap types ####

sample_data$trapTypes <- sample_data$Welche_Typen_von_Fallen_verwenden_Sie_Bitte_trennen_Sie_diese_jeweils_mit_einem_Komma_

unique(sample_data$trapTypes[sample_data$Taxa=="Schmetterlinge"])
unique(sample_data$trapTypes[sample_data$Taxa=="Amphibien/Reptilien"])
unique(sample_data$trapTypes[sample_data$Taxa=="Käfer"])
unique(sample_data$trapTypes[sample_data$Taxa=="Sonstiges"])


#### taxa specialism ####

sample_data$Specialism <- sample_data$Erfassen_Sie_vorrangig_Beobachtungsdaten_uber_eine_bestimmte_Untergruppe_z_B_eine_Familie_innerhalb_dieser_Artengruppe_

sample_data$Specialism_group <-
sample_data$Falls_Ja_Bitte_spezifizieren_Sie_die_Untergruppe_uber_die_Sie_vorrangig_Beobachtungsdaten_erfassen_

table(sample_data$Specialism)
table(sample_data$Specialism,sample_data$Taxa)

#which people are specialised
unique(sample_data$Specialism_group[sample_data$Taxa=="Vögel"])


### corona ####

table(sample_data$War_Ihre_Artenbeobachtung_oder_berichterstattung_im_Fruhling_Sommer_2020_aufgrund_der_Corona_Situation_anders_als_in_den_Vorjahren_)/nrow(sample_data)

# Ja, ich war aktiver als in den anderen Jahren. 
# 0.13125695 
# Ja, ich war weniger aktiv als in den anderen Jahren. 
# 0.14238042 
# Nein, es war mein erstes Jahr. 
# 0.08342603 
# Nein, ich bin weitgehend genauso vorgegangen wie in den anderen Jahren. 
# 0.64293660

#### demographics

table(sample_data$Zu_welcher_Altersklasse_gehoren_Sie_)
sample_data$Age <- ifelse(sample_data$Zu_welcher_Altersklasse_gehoren_Sie_=="19 oder jünger",15,
                          ifelse(sample_data$Zu_welcher_Altersklasse_gehoren_Sie_=="20-29",25,
                          ifelse(sample_data$Zu_welcher_Altersklasse_gehoren_Sie_=="30-39",35,
                          ifelse(sample_data$Zu_welcher_Altersklasse_gehoren_Sie_=="40-49",45,
                          ifelse(sample_data$Zu_welcher_Altersklasse_gehoren_Sie_=="50-59",55,
                          ifelse(sample_data$Zu_welcher_Altersklasse_gehoren_Sie_=="60-69",65,
                          ifelse(sample_data$Zu_welcher_Altersklasse_gehoren_Sie_=="70-79",75,
                          ifelse(sample_data$Zu_welcher_Altersklasse_gehoren_Sie_=="80+",85,NA))))))))

summary(sample_data$Age)

table(sample_data$Sind_Sie_Mitglied_in_einer_Fachgesellschaft_fur_eine_bestimmte_Artengruppe_z_B_GdO_GAC_DDA_etc_)

summary(sample_data$Wie_viele_Jahre_sind_Sie_schon_in_der_Erfassung_der_Artenbeobachtungsdaten_aktiv_)

table(sample_data$Taxa)
