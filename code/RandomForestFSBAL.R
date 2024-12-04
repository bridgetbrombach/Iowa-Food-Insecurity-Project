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

cps_data <- cps_data %>% na.omit(FSBAL)

###REMOVE THE UNUSED Y COLS (only include cols that we are using for analysis)
cps_data <- cps_data %>% 
  select(c(elderly, married, FSBAL, black, hispanic, education, 
           female, hhsize, kids))

str(cps_data)
summary(cps_data)


summary(cps_data$FSBAL)

#split data in to train and test

RNGkind(sample.kind = "default")
train.idx <- sample(x = 1:nrow(cps_data), size = .7*nrow(cps_data))
train.df <- cps_data[train.idx,]
test.df <- cps_data[-train.idx,]


#Fit a baseline forest 


tempforest <- randomForest(as.factor(FSBAL) ~ married + education + elderly + black + 
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
  
  tempforest <- randomForest(as.factor(FSBAL) ~ married + education + elderly + black + 
                               female + hispanic + hhsize + kids, 
                             data = train.df,
                             ntree = 1000,
                             mtry = mtry[idx],
                             weights = train.df$weight)
  
  keeps[idx, "m"] <- mtry[idx]
  
  keeps[idx, "OOB_err_rate"] <- mean(predict(tempforest) != train.df$FSBAL)
}

ggplot(data = keeps) +
  geom_line(aes(x = m, y = OOB_err_rate))+
  theme_bw() + labs(x = "m (mtry value)", y = "OOB Error Rate(minimize)") +
  scale_x_continuous(breaks = c(1:19))

# 4 looks to be the optimal number the m that minimizes the 
# OOB Error Rate

finalforest <- randomForest(as.factor(FSBAL) ~ married + education + elderly + black + 
                              female + hispanic + hhsize + kids, 
                            data = train.df,
                            ntree = 1000,
                            mtry = 4,  #chosen from tuning
                            weights = train.df$weight, 
                            importance = TRUE)

pi_hat <- predict(finalforest, test.df, type = "prob")[,"1"]



rocCurve <- roc(response = test.df$FSBAL,
                predictor = pi_hat,
                levels = c("0","1"))


plot(rocCurve, print.thres = TRUE, print.auc = TRUE) 
#AUC = 0.764
#pi* = 0.215 (significantly different from default of 0.5). this means that we will
# only determine that someone is food insecure (FSBAL = 1) if the probability
# that of FSBAL = 1, is greater than 0.79. (CHECK THIS INTERPRETATION)
#Specificity: 0.614
#Sensitivity: 0.851




















