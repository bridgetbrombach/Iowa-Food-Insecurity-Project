rm(list=ls())
source("code/clean_cps.R") #clean CPS
source("code/clean_acs.R") #clean ACS

library(tidyverse)
library(pROC)
library(glmnet)
library(lubridate)
library(randomForest)
library(ggplot2)
library(RColorBrewer)
library(rpart)
library(rpart.plot)
library(logistf)

## --- DATA PREP ---

cps_data_imp <- cps_data %>% 
  select(c(FSSTATUS, hhsize,education,hispanic,married,female,elderly,femhispanic,donutfamily,femblack,poverty,weight))

# There are NA values in the FSSTATUSMD column, so remove those -- 
cps_data_imp <- cps_data_imp %>% na.omit(cps_data_imp)

# Splitting the data into train and testing data sets
RNGkind(sample.kind="default")
set.seed(12345)
train.idx <- sample(x=1:nrow(cps_data_imp),size=.7*nrow(cps_data_imp))
train.df <- cps_data_imp[train.idx,]
test.df <- cps_data_imp[-train.idx,]


#### TREE FITTING and INTERPRETTION -------

tree <- rpart(FSSTATUS ~ hhsize + education + femhispanic + femblack +
                poverty + donutfamily + hispanic + married + female + elderly,
               data = train.df,
               method = 'class')

rpart.plot(tree)
