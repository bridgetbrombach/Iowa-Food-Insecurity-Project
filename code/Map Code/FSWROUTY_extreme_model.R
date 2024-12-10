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
  select(c(FSWROUTY_extreme,
           hhsize,education,hispanic,black,asian,married,female,elderly,kids,
           working,femhispanic,donutfamily,femblack,poverty,
           elmar,eled,elfem,elblack,elhispanic,elasian,workeduc,
           weight))

# There are NA values in the FSWROUTY column, so I will remove those
cps_data_models <- cps_data_models %>% na.omit(cps_data_models$FSWROUTY_extreme)

# Splitting the data into train and testing data sets
RNGkind(sample.kind="default")
set.seed(12345)
train.idx <- sample(x=1:nrow(cps_data_models),size=.7*nrow(cps_data_models))
train.df <- cps_data_models[train.idx,]
test.df <- cps_data_models[-train.idx,]

# Making test/train matrices
x.train <- model.matrix(FSWROUTY_extreme ~ hhsize+education+hispanic+black+asian+married+female+elderly+kids+
                          working+femhispanic+donutfamily+femblack+poverty+
                          elmar+eled+elfem+elblack+elhispanic+elasian+workeduc, data = train.df)[,-1]
x.test <- model.matrix(FSWROUTY_extreme ~ hhsize+education+hispanic+black+asian+married+female+elderly+kids+
                         working+femhispanic+donutfamily+femblack+poverty+
                         elmar+eled+elfem+elblack+elhispanic+elasian+workeduc, data = test.df)[,-1]

# Preparing acs for prediction
acs_data_predict <- acs_data %>% 
  select(c(hhsize,education,hispanic,black,asian,married,female,elderly,kids,
           working,femhispanic,donutfamily,femblack,poverty,
           elmar,eled,elfem,elblack,elhispanic,elasian,workeduc))
acs_data_predict <- as.matrix(acs_data_predict)

# Create vectors for y variable
y.train <- as.vector(train.df$FSWROUTY_extreme)
y.test <- as.vector(test.df$FSWROUTY_extreme)


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

FSWROUTY_extreme_ridge_rocCurve <- roc(response = as.factor(test.df.preds$FSWROUTY_extreme),
                                       predictor = test.df.preds$ridge_pred, 
                                       levels = c("0", "1")) 

plot(FSWROUTY_extreme_ridge_rocCurve, main="ROC curve for Ridge model on FSWROUTY_extreme",print.thres = TRUE, print.auc = TRUE)

######AGGREGATING AT PUMA LEVEL##########

## Using Ridge Model to predict for ACS
acs.preds <- acs_data %>% 
  mutate(
    ridge_pred = predict(ridge, acs_data_predict, type="response"),
  )

acs_data_predict_agg_FSWROUTY_extreme <- acs.preds %>% 
  filter(elderly >=1) %>% 
  group_by(PUMA) %>% 
  summarise(proportion_of_population = weighted.mean(ridge_pred, weights = weights))

total_elderly <- read.csv("data/total_iowa_seniors_by_puma.csv")

acs_data_predict_agg_FSWROUTY_extreme <- acs_data_predict_agg_FSWROUTY_extreme %>%
  mutate(count_of_seniors = total_elderly$senior_population*proportion_of_population,
         total_senior_pop=total_elderly$senior_population,
         GEOID = PUMA)

write.csv(acs_data_predict_agg_FSWROUTY_extreme,"data/acs_pred_FSWROUTY_extreme.csv",row.names=FALSE)

