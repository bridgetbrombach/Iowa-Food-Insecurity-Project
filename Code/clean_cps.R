#save this code into a src folder named clean_cps.R
#you'll also save code called clean_acs.R...
#and hopefully multiple analysis files - one for each of the Y variables you choose to model.
#your analysis files can source clean_cps.R using the source() function
#each row of cps is an iNDIVIDUAL within a family
library(tidyverse)


cps <- cps %>%
  mutate(SEX = SEX - 1 , # Create dummy variables
         CHILD = ifelse(AGE < 18, 1, 0),
         ELDERLY = ifelse(AGE > 59, 1, 0), #NOTE DEFINITION
         BLACK = ifelse(RACE==200, 1, 0),
         HISPANIC = ifelse(HISPAN>0, 1, 0),
         EDUC = as.integer(EDUC %in% c(91,92,111,123,124,125)),
         EMP = as.integer(EMPSTAT %in% c(1,10,12)),
         MARRIED = as.integer(MARST %in% c(1,2)),
         DIFF = ifelse(DIFFANY==2, 1, 0),
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
            #count of family members in various categories
            female = sum(SEX),
            hispanic = sum(HISPANIC),
            black= sum(BLACK),
            kids= sum(CHILD),
            elderly= sum(ELDERLY),
            education= sum(EDUC),
            married= sum(MARRIED)) %>% ungroup()
## `summarise()` has grouped output by 'CPSID'. You can override using the
## `.groups` argument.
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
         FSRAWSCRA = ifelse(FSRAWSCRA %in% c(98,99), NA, FSRAWSCRA),#raw score
         FSTOTXPNC = ifelse(FSTOTXPNC %in% c(999), NA, FSTOTXPNC)) %>%
  mutate(FSSTATUS = ifelse(FSSTATUS > 1, 1, 0),
         FSSTATUSMD = ifelse(FSSTATUSMD > 1, 1, 0),
         FSFOODS = ifelse(FSFOODS > 1, 1, 0),
         FSWROUTY = ifelse(FSWROUTY > 1, 1, 0),#more missings
         FSBAL = ifelse(FSBAL > 1, 1, 0),
         FSRAWSCRA=ifelse(FSRAWSCRA > 1, 1, 0))
#str(cps_data)
#summary(cps_data)
#Note: many of our y variables contain some NA values.
#Do not use complete.cases or na.omit on the whole dataset.


         