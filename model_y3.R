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

## ---- y3 is FSSKIPYR ----
# This code is for the FSSKIPYR y variable. It is a binary variable:
# 0 if never skipped meals or cut meal size because of not enough money for food in the past year, 
# 1 if otherwise

## --- DATA PREP (same for all four models) -------
# First I will create a subset of the data that only includes the X variables

cps_data_models <- cps_data %>% 
  select(c(FSSKIPYR,
           hhsize,education,hispanic,black,asian,married,female,elderly,kids,
           working,femhispanic,donutfamily,femblack,poverty,
           elmar,eled,elfem,elblack,elhispanic,elasian,workeduc,
           weight))

# There are NA values in the FSSKIPYR column, so I will remove those
cps_data_models <- cps_data_models %>% na.omit(cps_data_models$FSSKIPYR)

# Splitting the data into train and testing data sets
RNGkind(sample.kind="default")
set.seed(12345)
train.idx <- sample(x=1:nrow(cps_data_models),size=.7*nrow(cps_data_models))
train.df <- cps_data_models[train.idx,]
test.df <- cps_data_models[-train.idx,]

# Making test/train matrices
x.train <- model.matrix(FSSKIPYR ~ hhsize+education+hispanic+black+asian+married+female+elderly+kids+
                          working+femhispanic+donutfamily+femblack+poverty+
                          elmar+eled+elfem+elblack+elhispanic+elasian+workeduc, data = train.df)[,-1]
x.test <- model.matrix(FSSKIPYR ~ hhsize+education+hispanic+black+asian+married+female+elderly+kids+
                         working+femhispanic+donutfamily+femblack+poverty+
                         elmar+eled+elfem+elblack+elhispanic+elasian+workeduc, data = test.df)[,-1]

# Preparing acs for prediction
acs_data_predict <- acs_data %>% 
  select(c(hhsize,education,hispanic,black,asian,married,female,elderly,kids,
           working,femhispanic,donutfamily,femblack,poverty,
           elmar,eled,elfem,elblack,elhispanic,elasian,workeduc))
acs_data_predict <- as.matrix(acs_data_predict)

# Create vectors for y variable
y.train <- as.vector(train.df$FSSKIPYR)
y.test <- as.vector(test.df$FSSKIPYR)

### --- Lasso Model ------------------------------------------------------------
## --- Fitting the model -------

# 1. Use cross validation to fit (LOTS OF) lasso regressions
lr_lasso_cv <- cv.glmnet(x.train, 
                         y.train, 
                         weights = train.df$weight,
                         family=binomial(link="logit"), 
                         alpha=1)

# 2. Find the best lambda value
# - Plot the sample error for each lambda value
plot(lr_lasso_cv)
# - Pick out the best optimal lambda value
best_lasso_lambda <- lr_lasso_cv$lambda.min

# 3. Fit the final Model
lasso <- final_lasso <- glmnet(x.train,
                               y.train,
                               family = binomial(link = "logit"),
                               weights = train.df$weight,
                               alpha = 1,
                               lambda = best_lasso_lambda)

## --- Quantify Prediction Performance -----------------------------------------
test.df.preds <- test.df %>% 
  mutate(
    lasso_pred = predict(lasso, x.test, type="response")[,1],
  )

FSSKIPYR_lasso_rocCurve <- roc(response = as.factor(test.df.preds$FSSKIPYR),
                               predictor = test.df.preds$lasso_pred, 
                               levels = c("0", "1")) 

plot(FSSKIPYR_lasso_rocCurve, main="ROC curve for Lasso model on FSSKIPYR", print.thres = TRUE, print.auc = TRUE)


### --- Ridge Model ------------------------------------------------------------
## --- Fitting the model ------

# ---- Use cross validation to fit ridge regressions -----
lr_ridge_cv <- cv.glmnet(x.train, 
                         y.train, 
                         weights = train.df$weight,
                         family=binomial(link="logit"), 
                         alpha=0)

# ---- Finding the best lambda value -----
# - Plotting the sample error for each lambda value
plot(lr_ridge_cv)
# - Pick out the best optimal lambda value
best_ridge_lambda <- lr_ridge_cv$lambda.min

# ---- Fitting the final Model ----
ridge <- final_ridge <- glmnet(x.train,
                               y.train,
                               family = binomial(link = "logit"),
                               weights = train.df$weight,
                               alpha = 0,
                               lambda = best_ridge_lambda)

## --- Quantify Prediction Performance ----
test.df.preds <- test.df %>% 
  mutate(
    ridge_pred = predict(ridge, x.test, type="response")[,1],
  )

FSSKIPYR_ridge_rocCurve <- roc(response = as.factor(test.df.preds$FSSKIPYR),
                               predictor = test.df.preds$ridge_pred, 
                               levels = c("0", "1")) 

plot(FSSKIPYR_ridge_rocCurve, main="ROC curve for Ridge model on FSSKIPYR",print.thres = TRUE, print.auc = TRUE)

# ---- CHOOSING OUR MODEL -----

# The highest AUC is .606, which is very low. 
# FSSKIPYR is a variable that WesleyLife could use in the future, 
# but currently there is not enough data for it to be a useful prediction.

