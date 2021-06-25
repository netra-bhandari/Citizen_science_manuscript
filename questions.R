#get pca_data from the pca.R script
source('helper_functions.R', encoding = 'UTF-8')
survey_data <- readRDS("cleaned-data/clean_data.RDS")

#Questions
# (1) Taxon group differences: we used ordinal regression analysis to compare responses to the same question among participants with different focal taxonomic groups using the ordinal R package or a chi-square test. 

# Model formula: Response ~ TaxonGroup

#Explanatory variable:
#Taxon group is a factor with multiple levels: (1) birds, (2) plants, (3) amphibians/reptiles and #(4) specific insect groups (bees, butterflies, dragonflies, beetles)

# Response can be one of the following: 
# motivations PCA axis 1 and 2 (Diana to sort)

# age group: chisq.test(df$Taxa, df$Age-group)

survey_data$Bitte_wahlen_Sie_EINE_Artengruppe_[survey_data$Bitte_wahlen_Sie_EINE_Artengruppe_ %in% c("Bienen","Käfer","Libellen","Schmetterlinge")] <- "Insects"

chisq.test(survey_data$Bitte_wahlen_Sie_EINE_Artengruppe_,
           survey_data$Zu_welcher_Altersklasse_gehoren_Sie_)
## X-squared = 26.881, df = 24, p-value = 0.3101 (NS)

# number of years of experience: glm(nuYears ~ Taxon-group, data=df, family=Poisson)
shapiro.test(survey_data$Wie_viele_Jahre_sind_Sie_schon_in_der_Erfassung_der_Artenbeobachtungsdaten_aktiv_) #normal

summary(glm(Wie_viele_Jahre_sind_Sie_schon_in_der_Erfassung_der_Artenbeobachtungsdaten_aktiv_ ~ Bitte_wahlen_Sie_EINE_Artengruppe_ ,
    data = survey_data, family = poisson))

## summary of glm 
## Pfalnzen 0.078 S
## Insects   -0.11011 S
## Sonstiges -0.056 NS


# frequency of data collection: clm(frq ~ Taxon_group, data=df)
##as.ordered.factor

summary(clm(as.ordered(Wie_oft_haben_Sie_im_Fruhling_oder_Sommer_2020_Artdaten_gesammelt_)~ Bitte_wahlen_Sie_EINE_Artengruppe_,
    data = survey_data))

## summary of clm
## insects 0.39 NS
## Plants 0.47 S (0.1)
## Sonstiges 0.1 NS



# types of survey
# active/planned  clm(Likert.scale ~ Taxon_group, data=df)
# opportunistic  clm(Likert.scale ~ Taxon_group, data=df)
# trap  clm(Likert.scale ~ Taxon_group, data=df)

##as.ordered.factor
summary(clm(as.ordered(Wie_viele_der_Beobachtungen_die_Sie_im_Fruhling_und_Sommer_2020_gemeldet_haben_waren_oder_zufallige_Beobachtungen_ohne_aktive_Suche_) ~ Bitte_wahlen_Sie_EINE_Artengruppe_,
                   data = survey_data))

summary(clm(as.ordered(Wie_viele_der_Beobachtungen_die_Sie_im_Fruhling_und_Sommer_2020_gemeldet_haben_waren_das_Resultat_einer_aktiven_Suche_Sie_sind_zum_Beispiel_an_einen_bestimmten_Ort_gefahren_um_gezielt_nach_Arten_zu_suchen_)~
            Bitte_wahlen_Sie_EINE_Artengruppe_,
            data = survey_data))
                         
summary(clm(as.ordered(Wie_viele_der_Beobachtungen_die_Sie_im_Fruhling_und_Sommer_2020_gemeldet_haben_waren_oder_aus_aufgestellten_Fallen_) ~ Bitte_wahlen_Sie_EINE_Artengruppe_,
            data = survey_data))


## summary for types of survey 
##   Taxa           Active/planned          Opportunistic             Trap
##  Insects         -0.4401 (S)               0.2581 (NS)             0.6702(S)
##  Plants          -1.1864 (S)               1.0003(S)               1.4919 (S)
##  Sonstiges       -0.8401 (S)               0.5320 (S)              1.0277 (S)


# species preferences
# hist(score1) # check normality!!


lm(score1 ~ Taxon_group, data=df)
lm(score2 ~ Taxon_group, data=df)

# habitat preferences

lm(score1 ~ Taxon_group, data=df)
lm(score2 ~ Taxon_group, data=df)


### motivations analysis##############

#read and merge with pca scores for motivations
motivationsDF <- readRDS("model-outputs/motivationsDF.rds")
pca_data <- inner_join(pca_data,motivationsDF)
pca_data$Taxon_group <- pca_data$Bitte_wählen_Sie_EINE_Artengruppe__
pca_data$Taxon_group[pca_data$Taxon_group %in% c("Bienen","Käfer","Libellen","Schmetterlinge")] <- "Insects"

hist(pca_data$scores1)
unique(pca_data$Bitte_wählen_Sie_EINE_Artengruppe__)
pca_data <- subset(pca_data,Taxon_group!="")

#axis 1 - fun/outdoors
lm1 <- lm(scores1 ~ Taxon_group, data=pca_data)
summary(lm1)
anova(lm1)

#axis 2 - science/species
lm1 <- lm(scores2 ~ Taxon_group, data=pca_data)
summary(lm1)
anova(lm1)

### example data set for ordinal regression ####
#cumulative link mixed models (CLMM) for depression, anxiety and stress (ordinal package)
library(ordinal)

#example dataset: wine

#rating is specified as an ordered factor
fm1 <- clm(rating ~ temp + contact, data=wine)
summary(fm1)
confint(fm1)
plot(fm1)

#include random effects as in lmer
