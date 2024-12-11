acs <- read_sas("data/spm_pu_2022.sas7bdat")
#to calculate weights (go ahead and just copy/paste this):
acs <- acs%>%
  filter(st == "19")%>%
  group_by(serialno = as.factor(serialno)) %>% arrange(desc(Sex), desc(Age)) %>%
  mutate(weight = first(wt)) %>% select(-wt) %>% ungroup()

# create same variables as in CPS
acs <- acs %>%
  mutate(SEX = Sex - 1 , # since female = 2
         CHILD = ifelse(Age < 18, 1, 0), #SAME as cps definition
<<<<<<< Updated upstream
         WORKING=ifelse(Age > 17 & Age <60, 1, 0),
         ELDERLY = ifelse(Age > 59, 1, 0), #SAME as cps definition
         ASIAN=ifelse(Race==1,1,0),#SAME as cps definition
         BLACK = ifelse(Race==2, 1, 0), #SAME as cps definition (see data dictionary) 
         HISPANIC = ifelse(Hispanic>0, 1, 0), #SAME as cps definition (see data dictionary) 
         EDUC = as.integer(Education %in% c(3,4)), #above college level education
=======
         ELDERLY = ifelse(Age > 64, 1, 0), #SAME as cps definition
         BLACK = ifelse(Race==2, 1, 0), #SAME as cps definition (see data dictionary) 
         HISPANIC = ifelse(Hispanic>0, 1, 0), #SAME as cps definition (see data dictionary 
         EDUC = as.integer(Education %in% c(3,4)),
>>>>>>> Stashed changes
         MARRIED = as.integer(Mar %in% c(1)),
         poverty=OFFPoor,
         ELMAR = ELDERLY*MARRIED,
         ELED = ELDERLY*EDUC,
         ELFEM = SEX*ELDERLY,
         ELBLACK= ELDERLY*BLACK,
         ELHISPANIC=ELDERLY*HISPANIC,
         ELASIAN=ELDERLY*ASIAN,
         WORKEDUC=WORKING*EDUC,
         FEMBLACK=SEX*BLACK,
         FEMASIAN=SEX*ASIAN,
         FEMHISPANIC=SEX*HISPANIC,
         PUMA = as.factor(PUMA))
#aggregate up to family level
acs_data <- acs %>%
  group_by(serialno = as.factor(serialno)) %>% summarise(PUMA = first(PUMA),
                                                         poverty=first(poverty),
                                                         hhsize = as.numeric(length(serialno)),
                                                         #counts of people with various features - just like for CPS female = sum(SEX),
                                                         hispanic = sum(HISPANIC),
                                                         asian=sum(ASIAN),
                                                         working=sum(WORKING),
                                                         workeduc=sum(WORKEDUC),
                                                         black= sum(BLACK),
                                                         kids= sum(CHILD), 
                                                         elderly= sum(ELDERLY),
                                                         female=sum(SEX),
                                                         education= as.numeric(sum(EDUC)), 
                                                         married= as.numeric(sum(MARRIED)),
                                                         femblack=sum(FEMBLACK),
                                                         femhispanic=sum(FEMHISPANIC),
                                                         elmar=sum(ELMAR),
                                                         eled= sum(ELED),
                                                         elfem=sum(ELFEM),
                                                         elblack=sum(ELBLACK),
                                                         elhispanic=sum(ELHISPANIC),
                                                         elasian=sum(ELASIAN),
                                                         donutfamily=ifelse(kids>0 & kids+elderly==hhsize,1,0),
                                                         weight = weight[1],)
#each row of acs_data is a FAMILY