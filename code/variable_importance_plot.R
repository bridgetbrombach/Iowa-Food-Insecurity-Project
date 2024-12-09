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
  select(c(FSSTATUS, hhsize,education,hispanic,black,asian,married,female,elderly,kids,
           working,femhispanic,donutfamily,femblack,poverty,
           elmar,eled,elfem,elblack,elhispanic,elasian,workeduc,weight))

# There are NA values in the FSSTATUSMD column, so remove those -- 
cps_data_imp <- cps_data_imp %>% na.omit(cps_data_imp)

# Splitting the data into train and testing data sets
RNGkind(sample.kind="default")
set.seed(12345)
train.idx <- sample(x=1:nrow(cps_data_imp),size=.7*nrow(cps_data_imp))
train.df <- cps_data_imp[train.idx,]
test.df <- cps_data_imp[-train.idx,]


#### TREE FITTING and INTERPRETTION -------

tree <- rpart(FSSTATUS ~ hhsize+education+hispanic+black+asian+married+female+elderly+kids+
                working+femhispanic+donutfamily+femblack+poverty+
                elmar+eled+elfem+elblack+elhispanic+elasian+workeduc,
               data = train.df,
               method = 'class')

rpart.plot(tree)


### --- Random Forest Fitting ----
FSSTATUSvarImpPlotForest <- randomForest(as.factor(FSSTATUS) ~ hhsize+education+hispanic+black
  +asian+married+female+elderly+kids+working+femhispanic+donutfamily+femblack+poverty+
    elmar+eled+elfem+elblack+elhispanic+elasian+workeduc, 
  data = train.df,
  ntree = 1000,
  mtry = 4,  #chosen from tuning
  weights = train.df$weight, 
  importance = TRUE)

FSWROUTYvarImpPlotForest <- randomForest(as.factor(FSWROUTY) ~ hhsize+education+hispanic+black
  +asian+married+female+elderly+kids+working+femhispanic+donutfamily+femblack+poverty+
    elmar+eled+elfem+elblack+elhispanic+elasian+workeduc, 
  data = train.df,
  ntree = 1000,
  mtry = 4,  #chosen from tuning
  weights = train.df$weight, 
  importance = TRUE)

### --- FSSTATUS Variable Importance Plot ----
varImpPlot(FSSTATUSvarImpPlotForest, type=1)
FSSTATUSvi <- as.data.frame(varImpPlot(FSSTATUSvarImpPlotForest, type=1))
FSSTATUSvi$variable <- rownames(FSSTATUSvi)

