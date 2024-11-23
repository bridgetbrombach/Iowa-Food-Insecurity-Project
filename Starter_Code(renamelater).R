library(tidyverse) #includes ggplot2 and dplyer
library(ggthemes) #optional but classy ;)
library(logistf)#firth's penalized
library(glmnet) # for fitting lasso, ridge regressions (GLMs)
library(haven) #for reading in SAS data exports
library(knitr)

cps <- read.csv( "data/cps_00005.csv")

#first few rows of some select columns
head(cps[,c("CPSID","PERNUM","FSSTATUS", "FSSTATUSMD", "RACE", "EDUC")]) %>%kable

#look ar more descriptive labels for reference
map_chr(cps, ~attr(.x, "label")) %>%
  bind_cols(names = names(cps), question = .) %>%
  rownames_to_column(var="Variable Name") %>% kable

summary(cps_data$education)

summary(cps_data$FSTOTXPNC_perpers)