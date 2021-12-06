#helper_functions

#replace ordinal scale to a numeric scale

ordinal_fn <- function(x){
  x <- x %>%  mutate_if(is.character,funs(case_when(. == "sehr oft" ~ "5" ,
                                        . ==  "oft" ~ "4" ,
                                        . ==  "manchmal" ~ "3" ,
                                        . ==  "selten" ~ "2",
                                        . ==  "nie" ~ "1",
                                        . ==  "weiß nicht" ~ "0",
                                        . == "sehr wichtig" ~ "5",
                                        . == "wichtig" ~ "4",
                                        . == "mäßig wichtig" ~ "3",
                                        . == "wenig wichtig" ~ "2",
                                        . == "überhaupt nicht wichtig" ~ "1",
                                        . ==   "alle" ~ "5",
                                        . ==   "die meisten" ~ "4",
                                        . ==   "einige" ~ "3",
                                        . ==   "wenige" ~ "2",
                                        . ==   "keine" ~ "1",
                                        . ==  "überhaupt nicht wahrscheinlich" ~ "1",
                                        . ==  "wenig wahrscheinlich" ~ "2" ,
                                        . ==  "mäßig wahrscheinlich" ~ "3" ,
                                        . ==  "ziemlich wahrscheinlich" ~ "4",
                                        . ==  "sehr wahrscheinlich" ~ "5",
                                        . == "Täglich / fast täglich" ~ "6",
                                        . == "Wöchentlich / fast wöchentlich" ~ "5",
                                        . == "all zwei Woche/ fast all zwei Woche" ~ "4",
                                        . == "Monatlich / fast monatlich" ~ "3",
                                        . == "alle zwei Monate / fast alle zwei Monate" ~ "2",
                                        . == "Seltener" ~ "1",
                                        . ==  "Ja" ~ "1",
                                        . ==   "Nein" ~ "0"
  )))
  
  return(x)
  
}


combineTaxa <- function(x){
  x[x %in% c("Bienen","Käfer","Libellen","Schmetterlinge")] <- "Insects"
}


# funtion to  wrap the long questions for a tidy representation in plots

swr = function(string, nwrap=20) {
  paste(strwrap(string, width=nwrap), collapse="\n")
}

swr = Vectorize(swr)

#functions for pca analysis

format4PCA <- function(df){
  
  #convert likert scale to ordinal scale
  df <- ordinal_fn(df)
  df <- mutate_all(df, function(x) as.numeric(as.character(x)))
  return(df)
  
}

plotPCA <- function(pca_rotated){
  
  loadings <- as.data.frame(unclass(pca_rotated$loadings))
  loadings$Names<-rownames(loadings)

  ggplot()+
    geom_segment(data=loadings, aes(x=0, y=0, xend=RC1, yend=RC2), 
                 arrow=arrow(length=unit(0.2,"cm")),
                 colour="gray80")+
    geom_text_repel(data=loadings, aes(x=RC1, y=RC2, label=Names), 
                    size=2.5,colour="black")+
    scale_x_continuous("PC 1")+
    scale_y_continuous("PC 2")+
    theme_few()
}

plotPCA2 <- function(pca_rotated){
  
  loadings <- as.data.frame(unclass(pca_rotated$loadings))
  loadings$Names<-rownames(loadings)
  
  ggplot()+
    geom_segment(data=loadings, aes(x=0, y=0, xend=RC1, yend=RC2), 
                 arrow=arrow(length=unit(0.2,"cm")),
                 colour="gray80")+
    geom_text_repel(data=loadings, aes(x=RC1, y=RC2, label=Names), 
                    size=3,colour="black")+
    scale_x_continuous("PC 1")+
    scale_y_continuous("PC 2")+
    theme_few()
}

# identify correlations above 0.5
identifyCorrelations <- function(df){
  
  corrMatrix <- cor(df)
  corrMatrix[upper.tri(corrMatrix)] <- NA
  corrMatrixm <- reshape2::melt(corrMatrix)
  corrMatrixm <- subset(corrMatrixm,!is.na(value))
  corrMatrixm <- subset(corrMatrixm,value!=1)
  subset(corrMatrixm,abs(value)>0.5)
  
}