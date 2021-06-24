library(tidyverse)
source('helper_functions.R', encoding = 'UTF-8')

### read in a cleaned data frame ###

sample_data <- readRDS("cleaned-data/clean_data.RDS")
sample_data$Taxa <- sample_data$Bitte_wahlen_Sie_EINE_Artengruppe_

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
unique(sample_data$trapTypes[sample_data$Taxa=="Sonstiges"])


#### taxa specialism ####

sample_data$Specialism <- sample_data$Erfassen_Sie_vorrangig_Beobachtungsdaten_uber_eine_bestimmte_Untergruppe__z_B__eine_Familie__innerhalb_dieser_Artengruppe_

table(sample_data$Specialism)

