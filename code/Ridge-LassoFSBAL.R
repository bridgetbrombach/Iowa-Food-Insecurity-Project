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

source("code/clean_cps")

##Remove NA values in for FSSTATUSMD

cps_data <- cps_data %>% na.omit(FSBAL)
###REMOVE THE UNUSED Y COLS



#Take out variables we do not want to use for prediction
cps_data <- cps_data %>% 
  select(c(elderly, married, FSBAL, black, hispanic, education, 
           female, hhsize, kids))

#split data into train/test
RNGkind(sample.kind = "default")
train.idx <- sample(x = 1:nrow(cps_data), size = .7*nrow(cps_data))
train.df <- cps_data[train.idx,]
test.df <- cps_data[-train.idx,]


#####MAY NOT NEED THIS########
#Fit a traditional logistic regression model
lr_mle <- glm(FSBAL ~ elderly + married + black + hispanic + education+
                female + hhsize + kids,
              weights = train.df$weight,
              data = train.df,
              family = binomial(link = "logit"))

#got an error, algorithm did not converge, this was expected

beta <- coef(lr_mle)
#################################


#making test/train matrices
x.train <- model.matrix(FSBAL ~ ., data = train.df)[,-1]
x.test <- model.matrix(FSBAL ~. , data = test.df)[,-1]

# y variables need to be represented as vectors
y.train <- as.vector(train.df$FSBAL)
y.test <- as.vector(test.df$FSBAL)

lr_ridge_cv <- cv.glmnet(x.train, #this is the x MATRIX (not data frame)
                         y.train, #this is the y VECTOR
                         family = binomial(link = "logit"), 
                         alpha = 0) #0 for lasso

lr_lasso_cv <- cv.glmnet(x.train, #this is the x MATRIX (not data frame)
                         y.train, #this is the y VECTOR
                         family = binomial(link = "logit"), 
                         alpha = 1) #1 for lasso

plot(lr_ridge_cv)
plot(lr_lasso_cv)

#pick out optimal lambda value
best_ridge_lambda <- lr_ridge_cv$lambda.min
best_lasso_lambda <- lr_lasso_cv$lambda.min

final_ridge <- glmnet(x.train,
                      y.train,
                      family = binomial(link = "logit"),
                      alpha = 0,
                      lambda = best_ridge_lambda)


final_lasso <- glmnet(x.train,
                      y.train,
                      family = binomial(link = "logit"),
                      alpha = 1,
                      lambda = best_lasso_lambda)



test.df.preds <- test.df %>% 
  mutate(ridge_pred = predict(final_ridge, x.test, type = "response")[,1],
         lasso_pred = predict(final_lasso, x.test, type = "response")[,1])

ridge_rocCurve <- roc(response = as.factor(test.df.preds$FSBAL),
                      predictor = test.df.preds$ridge_pred, #predicted probs
                      levels = c("0", "1"))

lasso_rocCurve <- roc(response = as.factor(test.df.preds$FSBAL),
                      predictor = test.df.preds$lasso_pred, #predicted probs
                      levels = c("0", "1"))

ridge_data <- data.frame(
  Model = "Ridge",
  Specificity = ridge_rocCurve$specificities,
  Sensitivity = ridge_rocCurve$sensitivities,
  AUC = ridge_rocCurve$auc%>% as.numeric
)

#make data frame of lasso ROC info
lasso_data <- data.frame(
  Model = "Lasso",
  Specificity = lasso_rocCurve$specificities,
  Sensitivity = lasso_rocCurve$sensitivities,
  AUC = lasso_rocCurve$auc %>% as.numeric
)

randomforest_data <- data.frame(
  Model = "Random Forest",
  Specificity = 0.614,
  Sensitivity = 0.851,
  AUC = 0.764
  
)

# Combine all the data frames
roc_data <- rbind(lasso_data, ridge_data, randomforest_data)

# Plot the data
ggplot() +
  geom_line(aes(x = 1 - Specificity, y = Sensitivity, color = Model),data = roc_data) +
  geom_text(data = roc_data %>% group_by(Model) %>% slice(1), 
            aes(x = 0.75, y = c(0.75, 0.65, 0.55), colour = Model,
                label = paste0(Model, " AUC = ", round(AUC, 3)))) +
  scale_colour_brewer(palette = "Paired") +
  labs(x = "1 - Specificity", y = "Sensitivity", color = "Model") +
  theme_minimal()  

##Lasso AUC = 0.626
##Ridge AUC = 0.626
##Random Forest AUC = 0.764




