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

FSWROUTY_lasso_rocCurve <- roc(response = as.factor(test.df.preds$FSWROUTY),
                      predictor = test.df.preds$lasso_pred, 
                      levels = c("0", "1")) 

plot(FSWROUTY_lasso_rocCurve, main="ROC curve for Lasso model on FSWROUTY", print.thres = TRUE, print.auc = TRUE)


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

FSWROUTY_ridge_rocCurve <- roc(response = as.factor(test.df.preds$FSWROUTY),
                      predictor = test.df.preds$ridge_pred, 
                      levels = c("0", "1")) 

plot(FSWROUTY_ridge_rocCurve, main="ROC curve for Ridge model on FSWROUTY",print.thres = TRUE, print.auc = TRUE)

# --- CHOOSING THE MODEL -----

# Both Lasso and Ridge have the same AUC: .693.
# it's more important for us to have a higher sensitivity - true positives.
# It's more important that Wesley Life correctly predict food insecurity, 
# than for them to correctly predict when a household is NOT food insecure.

# We chose Ridge because it's specificity is 0.68 
# while for Lasso, the specificity is 0.546.

# ---- INTERPRETATIONS FOR THE Ridge MODEL ----

# the area under the curve is .693.

# if we set pi* = 0.369, we can achieve a specificity of 0.621, and 
# sensitivity of 0.68. 

# in other words, the model classifies a household as food insecure if the predicted 
# probability of food insecurity is greater than or equal to 0.369.

# the model correctly identifies 68.0% of households that are not food insecure
# (those who have sufficient food resources).

# the model correctly identifies 62.1% of households that are food insecure
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

finalforest <- randomForest(as.factor(FSWROUTY) ~ hhsize+education+hispanic+black
  +asian+married+female+elderly+kids+working+femhispanic+donutfamily+femblack+poverty+
    elmar+eled+elfem+elblack+elhispanic+elasian+workeduc, 
                            data = train.df,
                            ntree = 1000,
                            mtry = 4,  #chosen from tuning
                            weights = train.df$weight, 
                            importance = TRUE)

pi_hat <- predict(finalforest, test.df, type = "prob")[,"1"]

FSWROUTY_rocCurve <- roc(response = test.df$FSWROUTY,
                predictor = pi_hat,
                levels = c("0","1"))


plot(FSWROUTY_rocCurve, print.thres = TRUE, print.auc = TRUE) 

### --- MLE --------------------
## --- Fitting the model ------

# If all variables are included, the algorithm does not converge

# I included only the top 3 variables based on the variable importance plot below

lr_mle <- glm(as.factor(FSWROUTY) ~ poverty+black+hhsize,
              data = train.df,
              weights = weight,
              family = binomial(link= "logit"))

test.df.preds<-test.df %>% 
  mutate(
    mle_pred=predict(lr_mle,test.df,type="response"))

mle_rocCurve <- roc(response=as.factor(test.df.preds$FSWROUTY), #whatever you used as a y variable
                    predictor=test.df.preds$mle_pred, #predicted probs
                    levels=c("0","1"))

######AGGREGATING AT PUMA LEVEL##########

## Using Lasso to predict for ACS
acs.preds <- acs_data %>% 
  mutate(
    ridge_pred = predict(ridge, acs_data_predict, type="response")[,1],
  )

acs_data_predict_agg_FSWROUTY <- acs.preds %>% 
  filter(elderly >=1) %>% 
  group_by(PUMA) %>% 
  summarise(proportion_of_population = weighted.mean(ridge_pred, weights = weights))

total_elderly <- read.csv("data/tota_iowa_seniors_by_puma.csv")

acs_data_predict_agg_FSWROUTY <- acs_data_predict_agg_FSWROUTY %>%
  mutate(count_of_seniors = total_elderly$senior_population*proportion_of_population,
         total_senior_pop=total_elderly$senior_population)

write.csv(acs_data_predict_agg_FSWROUTY,"data/acs_pred_FSWROUTY.csv",row.names=FALSE)

### --- GRAPH THE ROC CURVES --------------------------------------------------------------------------------
par(mfrow=c(1,1))
plot(FSWROUTY_lasso_rocCurve, main="Lasso model", print.thres = TRUE, print.auc = TRUE)
plot(FSWROUTY_ridge_rocCurve, main="Ridge Model",print.thres = TRUE, print.auc = TRUE)
plot(mle_rocCurve, print.thres = TRUE, main="MLE", print.auc = TRUE) 

#make data frame of MLE ROC info
mle_data <- data.frame(
  Model = "MLE", 
  Specificity = mle_rocCurve$specificities,
  Sensitivity = mle_rocCurve$sensitivities,
  AUC = as.numeric(mle_rocCurve$auc)
)

#make data frame of lasso ROC info
lasso_data <- data.frame(
  Model = "Lasso",
  Specificity = FSWROUTY_lasso_rocCurve$specificities,
  Sensitivity = FSWROUTY_lasso_rocCurve$sensitivities,
  AUC = FSWROUTY_lasso_rocCurve$auc %>% as.numeric
)

#make data frame of ridge ROC info
ridge_data <- data.frame(
  Model = "Ridge",
  Specificity = FSWROUTY_ridge_rocCurve$specificities,
  Sensitivity = FSWROUTY_ridge_rocCurve$sensitivities,
  AUC = FSWROUTY_ridge_rocCurve$auc%>% as.numeric
)

# Combine all the data frames
roc_data <- rbind(lasso_data, ridge_data,mle_data)

# Plot the data
ggplot() +
  geom_line(aes(x = 1 - Specificity, y = Sensitivity, color = Model),data = roc_data) +
  geom_text(data = roc_data %>% group_by(Model) %>% slice(1), 
            aes(x = 0.75, y = c(0.75,0.70,0.65), colour = Model,
                label = paste0(Model, " AUC = ", round(AUC, 3))))+
  scale_colour_brewer(palette = "Dark2") +
  labs(title="Comparison of Models for FSWROUTY:\nWorried that food would run out before able to afford more during past year", 
       subtitle="Using Area Under Curve (AUC)",x = "1 - Specificity", y = "Sensitivity", color = "Model") +
  theme_minimal()

## Graphing the ROC curve for only lasso and ridge------

# Using only lasso and ridge (no mle)
roc_data <- rbind(lasso_data, ridge_data)

# Plot the data
ggplot() +
  geom_line(aes(x = 1 - Specificity, y = Sensitivity, color = Model),data = roc_data) +
  geom_text(data = roc_data %>% group_by(Model) %>% slice(1), 
            aes(x = 0.75, y = c(0.75,0.70), colour = Model,
                label = paste0(Model, " AUC = ", round(AUC, 3))))+
  scale_colour_brewer(palette = "Dark2") +
  labs(title="Comparison of Models for FSWROUTY:\nWorried that food would run out before able to afford more during past year", 
       subtitle="Using Area Under Curve (AUC)",x = "1 - Specificity", y = "Sensitivity", color = "Model") +
  theme_minimal()

#Here, it's very hard to see the difference between Lasso and Ridge. 

### --- Variable Importance Plot ------------------------------------------------
par(mfrow=c(1,1))
varImpPlot(finalforest, type=1)
vi <- as.data.frame(varImpPlot(finalforest, type=1))
vi$Variable <- rownames(vi)

ggplot(data = vi) +
  geom_bar(aes(x=reorder(Variable,MeanDecreaseAccuracy),
    weight=MeanDecreaseAccuracy), position="identity") +
  coord_flip() + 
  labs(x="Variable Name", y="Mean Decrease Accuracy") + 
  ggtitle("Variable Importance Plot for Variable FSWROUTY:\nWorried that food would run out before able to afford more during past year")

# Interpretations
# In predicting whether or not a household will be worried that food would run out
# before able to afford more during past year, if a household is 
# in poverty and household size are the 2 most important variables 

# Next we will figure out the direction these factors have on the probability that
# a household will be worried that food would run out before able to afford more
# using our best model (Ridge) 

summary(ridge)
coef(ridge)

## INTERPRETATIONS
# What happens if a household is in poverty?
exp(0.3522632244) #1.422283
# The odds of a household being food insecure increase by about 1.42 times if 
# a household is in poverty over one that isn't, holding all other variables constant. 
# 
# In other words, it could be smart to target households that are in poverty for 
# meals on wheels 

# What happens when a household size increases by 1
exp(-0.0189948538) #0.9811844
# The odds of a household being food insecure change by about .98 times for each
# additional person in the household, holding all other variables constant. 
# 
# In other words, the odds decrease by about 2% for each additional person in the
# household. 

### --- Graphing Important Variables ----
# Using our ridge Pi* from the ROC Curve (0.369) we can convert the predicted probabilities
# to a binary variable 

acs.preds <- acs.preds %>% 
  mutate(
    ridge_binary = ifelse(ridge_pred > 0.369, 1, 0),
  )




