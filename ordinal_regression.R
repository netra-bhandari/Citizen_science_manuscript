#cumulative link mixed models (CLMM) for depression, anxiety and stress (ordinal package)
library(ordinal)

#example dataset: wine

#rating is specified as an ordered factor
fm1 <- clm(rating ~ temp + contact, data=wine)
summary(fm1)
confint(fm1)
plot(fm1)

#include random effects as in lmer