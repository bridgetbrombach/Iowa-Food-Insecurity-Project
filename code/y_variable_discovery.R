source("code/clean_cps.R") #clean CPS
#source("code/clean_acs.R") #clean ACS
library(tidyverse)

#exploring our data------
cps_data<-read.csv("data/cps_clean.csv")

summary(cps_data)

#The only potential y variable we have that is NOT binary is FSTOTXPNC_perpers and FSTOTXPNC
#There are a lot of y variables

ggplot(data=cps_temp)+
  geom_bar(aes(x=FSRAWSCRA))

median(cps_data$FSBAL)
cps_temp <- cps_data[complete.cases(cps_data), ] #temporary non-NA data to find median
cps_temp
summary(cps_temp$FSBAL) #median is 0. no clue how people are using FSBAL

#let's explore FSSTATUSMD
cps_data<-read.csv("data/cps_clean.csv")

median(cps_data$FSSTATUSMD)
cps_temp <- cps_data[complete.cases(cps_data), ] #temporary non-NA data to find median
cps_temp
summary(cps_temp)
summary(cps_temp$FSSTATUSMD) 
summary(cps_data$FSSTATUSMD)

#let's explore FSRAWDSCRA
cps_data<-read.csv("data/cps_clean.csv")

median(cps_data$FSRAWSCRA)
cps_temp <- cps_data[complete.cases(cps_data), ] #temporary non-NA data to find median
cps_temp
summary(cps_temp)
summary(cps_temp$FSRAWSCRA)
summary(cps_data$FSRAWSCRA)

ggplot(data=cps_temp)+
  geom_bar(aes(x=FSRAWSCRA))
