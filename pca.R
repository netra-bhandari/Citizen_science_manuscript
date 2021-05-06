# PCA ---------------------------------------------------------------------


#library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
library(dplyr)
library(tidyverse)

#load dataset

setwd("F:/leipzig/CS spatial pattern/survey_data/Survey_data/figures in english")
survey_data <- read.csv("results-survey796935.csv",encoding = "UTF-8")

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

#arrange questions per taxa
survey_data <- 
pca_data <- survey_data %>% filter(Letzte_Seite == 12 | Letzte_Seite == 13)
colnames(pca_data)[1]<- "ID"


plants <- data.frame(pca_data[,grepl("Pflanzen", names(survey_data)) ])
plants <- as.data.frame(lapply(plants, factor)) 


plants <- ordinal_fn(plants)
plants$focal_taxa <- "plants"
plants$ID <- pca_data$ID
plants <- plants %>% select(ID,focal_taxa, everything())

## PCA 

sample_data <- plants[,-c(1:9)] 
sample_data <- sample_data[,-c(4,5,18)]
sample_data <- sample_data %>% filter(!sample_data == "") #removing NAs will reduce data from 900 to 171

sample_data <- na.omit(sample_data)#removing NAs will reduce data from 171 to 136

sample_data <- mutate_all(sample_data, function(x) as.numeric(as.character(x)))

## PCA 

fit <- princomp(sample_data, cor=TRUE)
summary(fit) # print variance accounted for
loadings(fit) # pc loadings
plot(fit,type="lines") # scree plot
fit$scores # the principal components
biplot(fit)#most interesting plot!


# using ggbiplot
fit <-  prcomp(sample_data, scale = TRUE)
ggbiplot(fit, obs.scale = 1, var.scale = 1)+
  theme_classic()#can be built up further 

#@DIANA - I will add the rest of the taxa and then use ggbiplot to make groups 

