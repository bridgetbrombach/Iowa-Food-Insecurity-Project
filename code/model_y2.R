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

## --- DATA PREP (same for all four models) ---
# First I will create a subset of the data that only includes the X variables

cps_data_models <- cps_data %>% 
  select(c(FSWROUTY,
           hhsize,education,hispanic,black,asian,married,female,elderly,kids,
           working,femhispanic,donutfamily,femblack,poverty,
           elmar,eled,elfem,elblack,elhispanic,elasian,workeduc,
           weight))

# There are NA values in the FSWROUTY column, so I will remove those
cps_data_models <- cps_data_models %>% na.omit(cps_data_models$FSWROUTY)

# Splitting the data into train and testing data sets
RNGkind(sample.kind="default")
set.seed(12345)
train.idx <- sample(x=1:nrow(cps_data_models),size=.7*nrow(cps_data_models))
train.df <- cps_data_models[train.idx,]
test.df <- cps_data_models[-train.idx,]

# Making test/train matrices
x.train <- model.matrix(FSWROUTY ~ hhsize+education+hispanic+black+asian+married+female+elderly+kids+
                          working+femhispanic+donutfamily+femblack+poverty+
                          elmar+eled+elfem+elblack+elhispanic+elasian+workeduc, data = train.df)[,-1]
x.test <- model.matrix(FSWROUTY ~ hhsize+education+hispanic+black+asian+married+female+elderly+kids+
                         working+femhispanic+donutfamily+femblack+poverty+
                         elmar+eled+elfem+elblack+elhispanic+elasian+workeduc, data = test.df)[,-1]

# Preparing acs for prediction
acs_data_predict <- acs_data %>% 
  select(c(hhsize,education,hispanic,black,asian,married,female,elderly,kids,
           working,femhispanic,donutfamily,femblack,poverty,
           elmar,eled,elfem,elblack,elhispanic,elasian,workeduc))
acs_data_predict <- as.matrix(acs_data_predict)

# Create vectors for y variable
y.train <- as.vector(train.df$FSWROUTY)
y.test <- as.vector(test.df$FSWROUTY)

# --- Logistic regression model ------------------------------------------------------------
#this should be after random forest, using the variables it predicted there
lr_mle <- glm(FSWROUTY ~ .,
              data = train.df,
              weights = weight,
              family = binomial(link= "logit"))

### --- Lasso Model ------------------------------------------------------------
## --- Fitting the model ---

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

lasso_rocCurve <- roc(response = as.factor(test.df.preds$FSWROUTY),
                      predictor = test.df.preds$lasso_pred, 
                      levels = c("0", "1")) 

plot(lasso_rocCurve, main="ROC curve for Lasso model on FSWROUTY", print.thres = TRUE, print.auc = TRUE)


### --- Ridge Model ------------------------------------------------------------
## --- Fitting the model ---

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

ridge_rocCurve <- roc(response = as.factor(test.df.preds$FSWROUTY),
                      predictor = test.df.preds$ridge_pred, 
                      levels = c("0", "1")) 

plot(ridge_rocCurve, main="ROC curve for Ridge model on FSWROUTY",print.thres = TRUE, print.auc = TRUE)

# ---- INTERPRETATIONS FOR THE RIDGE MODEL ----
# the area under the curve is 0.791

# if we set pi* = 0.140, we can achieve a specificity of 0.813, and 
# sensitivity of 0.624. 


# in other words, the model classifies a household as food insecure if the predicted 
# probability of food insecurity is greater than or equal to 0.140.

# the model correctly identifies 81.3% of households that are not food insecure
# (those who have sufficient food resources).

# the model correctly identifies 62.4% of households that are food insecure
# (those who lack adequate food resources).



###########RANDOM FOREST##############

#split data in to train and test

RNGkind(sample.kind = "default")

#Fit a baseline forest 
tempforest <- randomForest(as.factor(FSWROUTY) ~ hhsize+education+femhispanic+femblack+
                             poverty+donutfamily+hispanic+married+female+elderly,
                           data=train.df,
                           ntree = 100,
                           mtry = 4,
                           weights = train.df$weight)


dim(train.df)

mtry <- seq(from = 1, to = 8, by = 3)

keeps <- data.frame(m = rep(NA, length(mtry)),
                    OOB_err_rate = rep(NA, length(mtry)))

for(idx in 1:length(mtry)){
  print(paste0("Trying m = ", mtry[idx]))
  
  tempforest <- randomForest(as.factor(FSWROUTY) ~ hhsize+education+femhispanic+femblack+poverty+donutfamily+hispanic+married+female+
                               elderly, 
                             data = train.df,
                             ntree = 1000,
                             mtry = mtry[idx],
                             weights = train.df$weight)
  
  keeps[idx, "m"] <- mtry[idx]
  
  keeps[idx, "OOB_err_rate"] <- mean(predict(tempforest) != train.df$FSWROUTY)
}

ggplot(data = keeps) +
  geom_line(aes(x = m, y = OOB_err_rate))+
  theme_bw() + labs(x = "m (mtry value)", y = "OOB Error Rate(minimize)") +
  scale_x_continuous(breaks = c(1:19))

# 4 looks to be the optimal number the m that minimizes the 
# OOB Error Rate

finalforest <- randomForest(as.factor(FSWROUTY) ~ hhsize+education+femhispanic+
                              femblack+poverty+donutfamily+hispanic+married+female+elderly, 
                            data = train.df,
                            ntree = 1000,
                            mtry = 4,  #chosen from tuning
                            weights = train.df$weight, 
                            importance = TRUE)

pi_hat <- predict(finalforest, test.df, type = "prob")[,"1"]

rocCurve <- roc(response = test.df$FSWROUTY,
                predictor = pi_hat,
                levels = c("0","1"))


plot(rocCurve, print.thres = TRUE, print.auc = TRUE) 

######AGGREGATING AT PUMA LEVEL##########

## Using Lasso to predict for ACS
acs.preds <- acs_data %>% 
  mutate(
    ridge_pred = predict(ridge, acs_data_predict, type="response"),
  )

acs_data_predict_agg_FSWROUTY <- acs.preds %>% 
  filter(elderly >=1) %>% 
  group_by(PUMA) %>% 
  summarise(proportion_of_population = weighted.mean(ridge_pred, weights = weights))

total_elderly <- read.csv("data/iowa_seniors_by_puma.csv")

acs_data_predict_agg_FSWROUTY <- acs_data_predict_agg_FSWROUTY %>%
  mutate(count_of_seniors = total_elderly$senior_population*proportion_of_population,
         total_senior_pop=total_elderly$senior_population)

write.csv(acs_data_predict_agg_FSWROUTY,"data/acs_pred_FSWROUTY.csv",row.names=FALSE)

