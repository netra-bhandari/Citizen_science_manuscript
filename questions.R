# (1) Taxon group differences: we used ordinal regression analysis to compare responses to the same question among participants with different focal taxonomic groups using the ordinal R package or a chi-square test. 

# Model formula: Response ~ TaxonGroup

#Explanatory variable:
#Taxon group is a factor with multiple levels: (1) birds, (2) plants, (3) amphibians/reptiles and #(4) specific insect groups (bees, butterflies, dragonflies, beetles)

# Response can be one of the following: 
# motivations PCA axis 1 and 2 (Diana to sort)

# age group: chisq.test(df$Taxa, df$Age-group)

# number of years of experience: glm(nuYears ~ Taxon-group, data=df, family=Poisson)

# frequency of data collection: clm(frq ~ Taxon_group, data=df)
##as.ordered.factor

# types of survey
# active/planned  clm(Likert.scale ~ Taxon_group, data=df)
# opportunistics  clm(Likert.scale ~ Taxon_group, data=df)
# trap  clm(Likert.scale ~ Taxon_group, data=df)

##as.ordered.factor

# species preferences
# hist(score1) # check normality!!

lm(score1 ~ Taxon_group, data=df)
lm(score2 ~ Taxon_group, data=df)

# habitat preferences

lm(score1 ~ Taxon_group, data=df)
lm(score2 ~ Taxon_group, data=df)

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
