rm(list=ls())
source("code/clean_cps.R") #clean CPS
source("code/clean_acs.R") #clean ACS

library(tidyverse)
library(pROC)
library(glmnet)

### --- Lasso Model ------------------------------------------------------------
## --- Interaction Variables ---

## --- DATA PREP ---
# First I will create a subset of the data that only includes the X variables

cps_data_lasso <- cps_data %>% 
  select(c(FSSTATUSMD,hhsize,education,hispanic,married,female,elderly,FAMINC,weight))

# There are NA values in the FSSTATUSMD column, so I will remove those
cps_data_lasso <- cps_data_lasso %>% na.omit(cps_data_lasso)

# Splitting the data into train and testing data sets
RNGkind(sample.kind="default")
set.seed(12345)
train.idx <- sample(x=1:nrow(cps_data_lasso),size=.7*nrow(cps_data_lasso))
train.df <- cps_data_lasso[train.idx,]
test.df <- cps_data_lasso[-train.idx,]

# Fit a traditional logistic regression model
lr_mle <- glm(FSSTATUSMD ~ .,
  data = train.df,
  weights = weight,
  family = binomial(link= "logit"))

# Making test/train matrices
x.train <- model.matrix(FSSTATUSMD ~ hhsize+education+hispanic+married+female+
    elderly+FAMINC, data = train.df)[,-1]
x.test <- model.matrix(FSSTATUSMD ~ hhsize+education+hispanic+married+female+
    elderly+FAMINC, data = test.df)[,-1]

# Create vectors for y variable
y.train <- as.vector(train.df$FSSTATUSMD)
y.test <- as.vector(test.df$FSSTATUSMD)

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

lasso_rocCurve <- roc(response = as.factor(test.df.preds$FSSTATUSMD),
  predictor = test.df.preds$lasso_pred, 
  levels = c("0", "1")) 

plot(lasso_rocCurve, print.thres = TRUE, print.auc = TRUE)
















