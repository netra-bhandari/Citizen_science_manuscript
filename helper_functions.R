#helper_functions

#replace ordinal scale to a numeric scale

ordinal_fn <- function(x){
  x <- x %>%  mutate_all(funs(case_when(. == "sehr oft" ~ "5" ,
                                        . ==  "oft" ~ "4" ,
                                        . ==  "manchmal" ~ "3" ,
                                        . ==  "selten" ~ "2",
                                        . ==  "nie" ~ "1",
                                        . ==  "wei? nicht" ~ "0",
                                        . == "sehr wichtig" ~ "5",
                                        . == "wichtig" ~ "4",
                                        . == "mÃ¤Ãig wichtig" ~ "3",
                                        . == "wenig wichtig" ~ "2",
                                        . == "Ã¼berhaupt nicht wichtig" ~ "1",
                                        . ==   "alle"~ "5",
                                        . ==   "die meisten" ~ "4",
                                        . ==   "einige" ~ "3",
                                        . ==   "wenige" ~ "2",
                                        . ==   "keine" ~ "1",
                                        . ==  "?berhaupt nicht wahrscheinlich" ~ "1",
                                        . ==  "wenig wahrscheinlich" ~ "2" ,
                                        . ==  "m??ig wahrscheinlich" ~ "3" ,
                                        . ==  "ziemlich wahrscheinlich" ~ "4",
                                        . ==  "sehr wahrscheinlich" ~ "5",
                                        . ==  "Ja" ~ "1",
                                        . ==   "Nein" ~ "0"
  )))
  
  return(x)
  
}


combineTaxa <- function(x){
  x[x %in% c("Bienen","KÃ¤fer","Libellen","Schmetterlinge")] <- "Insects"
}


# funtion to  wrap the long questions for a tidy representation in plots

swr = function(string, nwrap=20) {
  paste(strwrap(string, width=nwrap), collapse="\n")
}

swr = Vectorize(swr)