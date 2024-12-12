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


##Remove NA values in for FSSTATUSMD

cps_data <- cps_data %>% na.omit(FSSTATUSMD)
###REMOVE THE UNUSED Y COLS

str(cps_data)
summary(cps_data)

#Imputing 4125 missing values for FSSTATUSMD

cps_data$FSSTATUSMD[is.na(cps_data$FSSTATUSMD)] <- median(cps_data$FSSTATUSMD, na.rm = TRUE)

summary(cps_data$FSSTATUSMD)

#split data in to train and test

RNGkind(sample.kind = "default")
train.idx <- sample(x = 1:nrow(cps_data), size = .7*nrow(cps_data))
train.df <- cps_data[train.idx,]
test.df <- cps_data[-train.idx, select = -c(CPSID)]


#Fit a baseline forest 


tempforest <- randomForest(as.factor(FSSTATUSMD) ~ married + education + elderly + black + 
                           female + hispanic + hhsize + kids, 
                           data = train.df,
                           ntree = 100,
                           mtry = 4,
                           weights = train.df$weight)


dim(train.df)

mtry <- seq(from = 1, to = 8, by = 3)

keeps <- data.frame(m = rep(NA, length(mtry)),
                    OOB_err_rate = rep(NA, length(mtry)))

for(idx in 1:length(mtry)){
  print(paste0("Trying m = ", mtry[idx]))
  
  tempforest <- randomForest(as.factor(FSSTATUSMD) ~ married + education + elderly + black + 
                               female + hispanic + hhsize + kids + FAMINC, 
                             data = train.df,
                             ntree = 1000,
                             mtry = mtry[idx],
                             weights = train.df$weight)
  
  keeps[idx, "m"] <- mtry[idx]
  
  keeps[idx, "OOB_err_rate"] <- mean(predict(tempforest) != train.df$FSSTATUSMD)
}

ggplot(data = keeps) +
  geom_line(aes(x = m, y = OOB_err_rate))+
  theme_bw() + labs(x = "m (mtry value)", y = "OOB Error Rate(minimize)") +
  scale_x_continuous(breaks = c(1:19))

# 4 looks to be the optimal number the m that minimizes the 
# OOB Error Rate

finalforest <- randomForest(as.factor(FSSTATUSMD) ~ married + education + elderly + black + 
                                        female + hispanic + hhsize + kids, 
                                      data = train.df,
                                      ntree = 1000,
                                      mtry = 4,  #chosen from tuning
                                      weights = train.df$weight, 
                                      importance = TRUE)

pi_hat <- predict(finalforest, test.df, type = "prob")[,"1"]



rocCurve <- roc(response = test.df$FSSTATUSMD,
                predictor = pi_hat,
                levels = c("0","1"))


plot(rocCurve, print.thres = TRUE, print.auc = TRUE) 

#AUC = 0.730
#pi* = 0.050 (wayyy off from the default of 0.5)
#Sensitivity: 0.928, we correctly predict that someone is not food insecure (by definition of FSSTATUSMD)
# 92.8% of the time
#Specificity: 0.472, we correctly predict that someone is food insecure 47.2% of the time

















