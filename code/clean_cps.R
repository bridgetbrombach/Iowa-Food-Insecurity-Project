library(tidyverse) #includes ggplot2 and dplyer
library(ggthemes) #optional but classy ;)
library(logistf)#firth's penalized
library(glmnet) # for fitting lasso, ridge regressions
library(haven) #for reading in SAS data exports

cps <- read.csv("data/cps_00007.csv")

#each row of cps is an INDIVIDUAL within a family
cps <- cps %>%
  mutate(SEX = SEX - 1 , # Create dummy variables
    CHILD = ifelse(AGE < 18, 1, 0),
    WORKING_AGE = ifelse(AGE > 18 & AGE < 60, 1, 0),
    ELDERLY = ifelse(AGE > 59, 1, 0), 
    ASIAN = ifelse(RACE==651, 1, 0),
    BLACK = ifelse(RACE==200, 1, 0),
    HISPANIC = ifelse(HISPAN>0, 1, 0),
    EDUC = as.integer(EDUC %in% c(91,92,111,123,124,125)),
    FEMBLACK=BLACK*SEX,
    FEMHISPANIC=HISPANIC*SEX,
    FEMASIAN=ASIAN*SEX,
    EMP = as.integer(EMPSTAT %in% c(1,10,12)),
    MARRIED = as.integer(MARST %in% c(1,2)),
    ELMAR = ELDERLY*MARRIED,
    ELED = ELDERLY*EDUC,
    ELFEM = SEX*ELDERLY,
    ELBLACK= ELDERLY*BLACK,
    ELASIAN=ELDERLY*ASIAN,
    ELHISPANIC=ELDERLY*HISPANIC,
    WORKEDUC=WORKING_AGE*EDUC,
    COUNTY = as.factor(COUNTY))

#currently, one row of cps = one individual
#however, we want to make prediction on the family level
#aggregate to the family level - this is where we choose FAMILY-LEVEL traits
#that we want to calculate. For example, household size is equal to the
#number of rows for that family.
cps_data <- cps %>%
group_by(CPSID = as.factor(CPSID)) %>%
  summarise(COUNTY = first(COUNTY),
    #family level weight
    weight = first(HWTFINL),
    #household size
    hhsize = n(),
    #Y variables - i.e., measures of hunger
    #see CPS website for details
    #FSSTATUS, etc. is the same for each member -just take first value for each family
    FSTOTXPNC_perpers = FSTOTXPNC/hhsize, # In per person terms
    FSSTATUS = first(FSSTATUS),
    FSSTATUSMD = first(FSSTATUSMD),
    FSFOODS = first(FSFOODS),
    FSWROUTY = first(FSWROUTY),
    FSBAL = first(FSBAL),
    FSRAWSCRA = first(FSRAWSCRA),
    FSTOTXPNC = first(FSTOTXPNC),
    FSSTATUS = first(FSSTATUS),
    FSSKIPYR=first(FSSKIPYR),
    poverty=first(FAMINC), #NEW
    #count of family members in various categories
    working=sum(WORKING_AGE),
    workeduc=sum(WORKEDUC),
    asian=sum(ASIAN),
    elasian=sum(ELASIAN),
    female = sum(SEX),
    hispanic = sum(HISPANIC),
    black= sum(BLACK),
    femblack=sum(FEMBLACK),
    femhispanic=sum(FEMHISPANIC),
    elmar=sum(ELMAR),
    eled= sum(ELED),
    elfem=sum(ELFEM),
    elblack=sum(ELBLACK),
    elhispanic=sum(ELHISPANIC),
    kids= sum(CHILD),
    elderly= sum(ELDERLY),
    education= sum(EDUC),
    donutfamily=ifelse(kids>0 & kids+elderly==hhsize,1,0),
    married= sum(MARRIED)) %>% ungroup()

#each row of cps_data is a FAMILY
#note... we just calculated the number of people in each family that belong
#to the above groups. perhaps that isn't the best way? Would proportions be good
#in addition or instead of sums?!
#summary(cps_data) # see extremes for food security variables
#https://cps.ipums.org/cps-action/variables/search
cps_data <- cps_data %>%
  mutate(FSSTATUS = ifelse(FSSTATUS %in% c(98,99), NA, FSSTATUS),
    FSSTATUSMD = ifelse(FSSTATUSMD %in% c(98,99), NA, FSSTATUSMD),
    FSFOODS = ifelse(FSFOODS %in% c(98,99), NA, FSFOODS),
    FSWROUTY = ifelse(FSWROUTY %in% c(96,97,98,99), NA, FSWROUTY),
    FSBAL = ifelse(FSBAL %in% c(96,97,98,99), NA, FSBAL),
    FSSKIPYR = ifelse(FSSKIPYR %in% c(96,97,98,99), NA, FSSKIPYR),
    FSRAWSCRA = ifelse(FSRAWSCRA %in% c(98,99), NA, FSRAWSCRA),#raw score
    FSTOTXPNC = ifelse(FSTOTXPNC %in% c(999), NA, FSTOTXPNC),
    poverty=ifelse(poverty %in% c(999), NA, poverty)) %>%
  mutate(FSSTATUS = ifelse(FSSTATUS > 1, 1, 0),
         poverty=case_when(
           poverty < 500 & hhsize == 1 ~ 1,
           poverty < 600 & hhsize == 2 ~ 1,
           poverty < 710 & hhsize == 3 ~ 1,
           poverty < 720 & hhsize == 4 ~ 1,
           poverty < 730 & hhsize == 5 ~ 1,
           poverty < 740 & hhsize == 6 ~ 1,
           poverty < 820 & hhsize == 7 ~ 1,
           poverty < 820 & hhsize == 8 ~ 1,
           poverty < 830 & hhsize == 9 ~ 1,
           poverty < 841 & hhsize == 10 ~ 1,
           poverty < 841 & hhsize == 11 ~ 1,
           poverty < 841 & hhsize == 12 ~ 1,
           poverty < 841 & hhsize == 13 ~ 1,
           poverty < 841 & hhsize == 14 ~ 1,
           TRUE ~ poverty), 
         #NEW - approximate whether a household is in poverty based on 100%-125% poverty levels in the US
         poverty=ifelse(poverty == 1, 1, 0), #poverty is an x variable
    FSSTATUSMD = ifelse(FSSTATUSMD > 1, 1, 0),
    FSFOODS = ifelse(FSFOODS > 1, 1, 0),
    FSSKIPYR = ifelse(FSSKIPYR > 1, 1, 0),
    FSWROUTY = ifelse(FSWROUTY > 1, 1, 0), #more missings
    FSSKIPYRandFSWROUTY=ifelse(FSWROUTY==1|FSSKIPYR==1, 1, 0),
    FSBAL = ifelse(FSBAL > 1, 1, 0),
    FSRAWSCRA=ifelse(FSRAWSCRA > 1, 1, 0))
cps_clean<-cps_data
#str(cps_data)
summary(cps_data)
#Note: many of our y variables contain some NA values.
#Do not use complete.cases or na.omit on the whole dataset.

write.csv(cps_data,"data/cps_data.csv",row.names=FALSE)




