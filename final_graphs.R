#final graphs
library(tidyverse)
#load libraries in Netra's script

source('helper_functions.R', encoding = 'UTF-8')

### read in a cleaned data frame ###

#### load dataset ####

##trial with cleaned-data (keeping variable name same here so as not to make multiple changes below)
sample_data <- readRDS("cleaned-data/clean_data.RDS")


#### fig. 1: motivations ####

imp_obs <- data.frame(sample_data[ , grepl("Wie_wichtig_waren_Ihnen_die_folgenden_Aspekte_",  names(sample_data))])

imp_obs <-  imp_obs %>% 
  mutate_all(funs(case_when(. == "überhaupt nicht wichtig" ~ "not important at all",
                            . == "wenig wichtig" ~ "not very important",
                            . == "mäßig wichtig" ~ "moderately important",
                            . == "wichtig" ~ "important",
                            . == "sehr wichtig" ~ "very important")) )

# remove empty rows
imp_obs[imp_obs==""] <- NA
imp_obs <- imp_obs[complete.cases(imp_obs),]

#convert the variables to factors
imp_obs <- as.data.frame(lapply(imp_obs, factor, levels = c("not important at all","not very important",
                                                            "moderately important","important","very important")))

colnames(imp_obs) = c("Improve knowledge of species", "Contribute to scientific knowledge", 
                      "Support conservation", "Spend time outdoors", "Physical activity ",
                      "Protect/maintain/improve a specific place", "Gain local knowledge about where I live",
                      "Meet other people", "Have fun exploring/finding")

plot(likert(imp_obs), center=3, 
                    plot.percents=F, 
                    plot.percent.low=F,
                    plot.percent.high=F, 
                    plot.percent.neutral = FALSE,
                    text.size=10, ordered=TRUE,
                    colors = c("#a6611a","#dfc27d","#f5f5f5","#80cdc1","#018571"))+
  theme(strip.text=element_text(size = 10),
        axis.text.y = element_text(size = 10, color = "black"),
        axis.text.x = element_text(size = 10, color = "black"), 
        axis.title.x = element_text(size = 10, color = "black"),   
        axis.title.y = element_text(size = 10, color = "black"),
        legend.title=element_text(size = 5, color = "black"), 
        legend.text=element_text(size = 5, color = "black"),
        legend.position = c(0.25,0.75))+
  ylab("Percentage of respondents (%)")


#### active vs opportunistic ####

sample_data$Active_search <- sample_data$Wie_viele_der_Beobachtungen_die_Sie_im_Fruhling_und_Sommer_2020_gemeldet_haben_waren_das_Resultat_einer_aktiven_Suche_Sie_sind_zum_Beispiel_an_einen_bestimmten_Ort_gefahren_um_gezielt_nach_Arten_zu_suchen_

sample_data$Incidental_search <- sample_data$Wie_viele_der_Beobachtungen_die_Sie_im_Fruhling_und_Sommer_2020_gemeldet_haben_waren_oder_zufallige_Beobachtungen_ohne_aktive_Suche_

table(sample_data$Active_search,sample_data$Incidental_search)

surveytype_df <- sample_data %>%
  select(ID,Active_search,Incidental_search) %>%
  group_by(Active_search,Incidental_search) %>%
  summarise(nu_Respondents = length(unique(ID)))

surveytype_df$Active_search <- factor(surveytype_df$Active_search, 
                                      levels=c("keine","wenige","einige","die meisten","alle"))

levels(surveytype_df$Active_search) <- c("no", "few", "some", "most","all")

surveytype_df$Incidental_search <- factor(surveytype_df$Incidental_search, 
                                          levels=c("keine","wenige","einige","die meisten","alle"))

levels(surveytype_df$Incidental_search) <- c("no", "few", "some", "most","all")

plot2a <- ggplot(surveytype_df)+
  geom_tile(aes(x=Active_search,y=Incidental_search,
                fill=nu_Respondents/nrow(sample_data)*100))+
  scale_fill_gradient2("% Respondents",low="white",high="brown")+
  #coord_fixed()+
  theme(strip.text=element_text(size = 10),
        axis.text.y = element_text(size = 10, color = "black"),
        axis.text.x = element_text(size = 10, color = "black", angle=45), 
        axis.title.x = element_text(size = 10, color = "black"),   
        axis.title.y = element_text(size = 10, color = "black"),
        legend.title=element_text(size = 8, color = "black"), 
        legend.text=element_text(size = 8, color = "black"),
        legend.key.size = unit(0.3, "cm"),
        legend.position = "top")+
  xlab("Observations from active search")+ylab("Opportunistic observations")



#### opportunistic triggers #####

#https://jtr13.github.io/cc19/how-to-plot-likert-data.html
#https://search.r-project.org/CRAN/refmans/HH/html/likertColor.html

imp_obs <- data.frame(sample_data[ , grepl("Was_veranlasst_Sie_dazu",  names(sample_data))])

imp_obs <-  imp_obs %>% 
  mutate_all(funs(case_when(. == "sehr oft" ~ "very often",
                            . == "oft" ~ "often",
                            . == "manchmal" ~ "sometimes",
                            . == "selten" ~ "rarely",
                            . == "nie" ~ "never",
                            . == "weiß nicht" ~ "don't know")))

# remove empty rows
imp_obs[imp_obs==""] <- NA
imp_obs <- imp_obs[complete.cases(imp_obs),]

#convert the variables to factors
imp_obs <- as.data.frame(lapply(imp_obs, factor, levels = rev(c("don't know","never","rarely",
                                                            "sometimes","often","very often"))))

colnames(imp_obs) = c("I see a rare species", "I see many species at the same time", 
                      "I see an unexpected species", "I see a species for the first time of this year", 
                      "I see an unknown species", 
                      "I see many individuals of the same species", 
                      "I see an interesting species")


df <- imp_obs %>%
  pivot_longer(.,everything(),names_to = "subquestion",values_to = "answer") %>%
  group_by(subquestion, answer) %>%
  count() %>%
  mutate(subquestion = gsub("\\."," ", subquestion))

#order the subequestions 
orderSub <- df %>%
            filter(answer %in% c("often", "very often")) %>%
            group_by(subquestion) %>%
            summarise(n=sum(n)) %>%
            arrange(desc(n))

df$subquestion <- factor(df$subquestion, levels=rev(orderSub$subquestion))

plot2c <- ggplot(df)+
  geom_bar(aes(x = subquestion,y=n/nrow(imp_obs)*100,fill=answer),stat="identity",position="stack")+
  coord_flip()+
  scale_fill_brewer("",palette="PRGn")+
  theme(strip.text=element_text(size = 10),
        axis.text.y = element_text(size = 10, color = "black"),
        axis.text.x = element_text(size = 10, color = "black"), 
        axis.title.x = element_text(size = 10, color = "black"),   
        axis.title.y = element_text(size = 10, color = "black"),
        legend.title=element_text(size = 8, color = "black"), 
        legend.text=element_text(size = 8, color = "black"),
        legend.key.size = unit(0.3, "cm"),
        legend.position = "none")+
  guides(fill = guide_legend(reverse=T))+
  ylab("Percentage of respondents (%)")+xlab("")

plot2c

#### active search species #####

imp_obs <- data.frame(sample_data[ , grepl("Wenn_Sie_aktiv_auf_die_Suche_nach",  names(sample_data))])
imp_obs <- data.frame(imp_obs[ , grepl("wie_sind_Sie_bei_der_Sammlung",  names(imp_obs))])

imp_obs <-  imp_obs %>% 
  mutate_all(funs(case_when(. == "sehr oft" ~ "very often",
                            . == "oft" ~ "often",
                            . == "manchmal" ~ "sometimes",
                            . == "selten" ~ "rarely",
                            . == "nie" ~ "never",
                            . == "weiß nicht" ~ "don't know")))

# remove empty rows
imp_obs[imp_obs==""] <- NA
imp_obs <- imp_obs[complete.cases(imp_obs),]

#convert the variables to factors
imp_obs <- as.data.frame(lapply(imp_obs, factor, levels = rev(c("don't know","never","rarely",
                                                            "sometimes","often","very often"))))

colnames(imp_obs) = c("I use a checklist with the possible species", 
                      "I record all species that I see", 
                      "I record only the interesting species", 
                      "I record only the common species", 
                      "I record only the rare species")

df <- imp_obs %>%
  pivot_longer(.,everything(),names_to = "subquestion",values_to = "answer") %>%
  group_by(subquestion, answer) %>%
  count() %>%
  mutate(subquestion = gsub("\\."," ", subquestion))

#order the subequestions 
orderSub <- df %>%
  filter(answer %in% c("often", "very often")) %>%
  group_by(subquestion) %>%
  summarise(n=sum(n)) %>%
  arrange(desc(n))

df$subquestion <- factor(df$subquestion, levels=rev(orderSub$subquestion))

plot2b <- ggplot(df)+
  geom_bar(aes(x = subquestion,y=n/nrow(imp_obs)*100,fill=answer),stat="identity",position="stack")+
  coord_flip()+
  scale_fill_brewer("",palette="PRGn")+
  theme(strip.text=element_text(size = 10),
        axis.text.y = element_text(size = 10, color = "black"),
        axis.text.x = element_text(size = 10, color = "black"), 
        axis.title.x = element_text(size = 10, color = "black"),   
        axis.title.y = element_text(size = 10, color = "black"),
        legend.title=element_text(size = 8, color = "black"), 
        legend.text=element_text(size = 8, color = "black"),
        legend.key.size = unit(0.3, "cm"),
        legend.position = "top")+
  guides(fill = guide_legend(reverse=T))+
  ylab("Percentage of respondents (%)")+xlab("")

plot2b


#### location preferences ####

imp_obs <- data.frame(sample_data[ , grepl("haben_Sie_an_den_folgenden_Orten_nach",  names(sample_data))])

imp_obs <-  imp_obs %>% 
  mutate_all(funs(case_when(. == "sehr oft" ~ "very often",
                            . == "oft" ~ "often",
                            . == "manchmal" ~ "sometimes",
                            . == "selten" ~ "rarely",
                            . == "nie" ~ "never",
                            . == "weiß nicht" ~ "don't know")))

# remove empty rows
imp_obs[imp_obs==""] <- NA
imp_obs <- imp_obs[complete.cases(imp_obs),]

#convert the variables to factors
imp_obs <- as.data.frame(lapply(imp_obs, factor, levels = rev(c("don't know","never","rarely",
                                                            "sometimes","often","very often"))))

colnames(imp_obs) = c("Protected areas",
                      "Forest",
                      "Wetland and waterbodies",
                      "Meadows",
                      "Arable land",
                      "Urban areas (parks)",
                      "Urban areas (residential)",
                      "Remote areas (>50 km from a city)")

df <- imp_obs %>%
  pivot_longer(.,everything(),names_to = "subquestion",values_to = "answer") %>%
  group_by(subquestion, answer) %>%
  count() %>%
  mutate(subquestion = gsub("\\."," ", subquestion))

#order the subequestions 
orderSub <- df %>%
  filter(answer %in% c("often", "very often")) %>%
  group_by(subquestion) %>%
  summarise(n=sum(n)) %>%
  arrange(desc(n))

df$subquestion <- factor(df$subquestion, levels=rev(orderSub$subquestion))

plot2d <- ggplot(df)+
  geom_bar(aes(x = subquestion,y=n/nrow(imp_obs)*100,fill=answer),stat="identity",position="stack")+
  coord_flip()+
  scale_fill_brewer("",palette="PRGn")+
  theme(strip.text=element_text(size = 10),
        axis.text.y = element_text(size = 10, color = "black"),
        axis.text.x = element_text(size = 10, color = "black"), 
        axis.title.x = element_text(size = 10, color = "black"),   
        axis.title.y = element_text(size = 10, color = "black"),
        legend.title=element_text(size = 8, color = "black"), 
        legend.text=element_text(size = 8, color = "black"),
        legend.key.size = unit(0.3, "cm"),
        legend.position = "none")+
  guides(fill = guide_legend(reverse=T))+
  ylab("Percentage of respondents (%)")+xlab("")

plot2d


#### fig. 2: combine plots ####

library(cowplot)

plot_grid(plot2a,plot2b,
          plot2c,plot2d,
          nrow=2,
          labels=c("a) Active vs opportunistic","b) Active search",
                   "c) Opportunistic search","d) Locations"),
          scale = c(0.9,0.9,0.9,0.9))

