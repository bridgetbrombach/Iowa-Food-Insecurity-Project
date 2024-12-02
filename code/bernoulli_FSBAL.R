
library(ggplot2) #for professional plots
#--------------cps_elderly data--------------
cps_data<-read.csv("data/cps_clean.csv")
cps_elderly <- cps_data[complete.cases(cps_data), ]

#Bernoulli Logistic Regression fit with glm
cps.glm <- glm(FSBAL~elderly, data=cps_elderly, family=binomial(link = "logit"))
#maximum likelihood estimates of beta
beta_hat <- coef(cps.glm)
#predicted values
cps_elderly$pred <- predict(cps.glm,cps_elderly, type='response')
#note those predicted values are THE SAME AS:
cps_elderly$pred <-exp(beta_hat[1] +beta_hat[2]*cps_elderly$elderly ) /
  (1 + exp(beta_hat[1] +beta_hat[2]*cps_elderly$elderly ))
#plot them by layering point, line geoms
ggplot(data = cps_elderly) +
  geom_point(aes(x = elderly, y = FSBAL))+ #data
  geom_line(aes(x=elderly,y=pred)) + #preds
  theme_bw() +
  labs(x = "elderlyerature (F)", y = "Probability of at least one FSBAL eqal to zero")
#wald - notice these are z statistics rather than w
#p-values are same
summary(cps.glm) #check standard errors
confint(cps.glm)#conf int for beta
exp(confint(cps.glm))#conf int for exp(beta)
#maximum likelihood estimates of odds ratios (exp(beta))
exp(beta_hat)
#type 3 hypothesis tests
anova(cps.glm, test = "Chisq")
