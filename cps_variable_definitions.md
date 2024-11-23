#Cleaned Variable Definitions(CPS):
##IPUMS CPS Link: https://cps.ipums.org/cps-action/variables

**SEX**: Is this person a female? 1 if yes, 0 if no

**CHILD**: If age < 18, 1 if yes, 0 if no

**ELDERLY**: If age > 59 (given 60 cutoff by Wesely Life) 1 if yes, 0 if no

**BLACK**: Race code 200 from RACE IPUMS CPS Website, 1 if yes, 0 if no

**HISPANIC**: If HISPAN code from IPUMS > 0, 1 if yes, 0 if no

**EDUC**: IF EDUC code is one of the following:
                      91 (Associate's degree, occupational/vocational program), 
                      92 (Associate's degree, academic program), 
                      111 (Bachelor's degree), 
                      123 (Master's degree),
                      124 (Professional school degree)
                          ("Professional school programs help prepare students
                          for careers in specific fields. Examples include medical, 
                          law, pharmacy, business, library, and social work schools. 
                          The length of these programs vary. Professional degrees 
                          are often required by law before an individual can 
                          begin working in a particular occupation.")
                          (https://sas.uaa.uw.edu/husky-experience/know-the-world/considering-graduate-or-professional-school/#:~:text=Professional%20school%20programs%20help%20prepare,working%20in%20a%20particular%20occupation.), 
                      125 (Doctorate degree)
            1 if yes, 0 if no
          
          In other words, this is looking whether or not a person has at least
          an associates degree
      

**EMP**: If EMPSTAT (Employment Status) code is 1,10,12
        
        ----Levels-----
        1: Armed Forces
        10: Employed (currently working)
        12: Employed (has job, not at work last week)
        
        Essentially, if a person is employed 1, if not 0

**MARRIED**: MARST (Marital Status) code is 1 or 2
          1: Married, spouse present
          2: Married, spouse absent
          
**DIFFANY** (do not use in model):  Unable to be used in modeling as it is not 
            used in APS data

**COUNTY**(may not be needed (all of midwest used for CPS but only Iowa counties 
          for ACS)):
        "COUNTY is a five-digit numeric variable. The first two digits give the 
        FIPS state code; the last three digits give the FIPS county code. 
        For a list of counties identified in each state, follow the following 
        links for the appropriate years:"
        https://cps.ipums.org/cps-action/variables/COUNTY#codes_section
        **00000 = Not Identified
        
**weight**: Household weight, Basic Monthly [preselected] (to address uneven 
            sampling)
        
**hhsize**: Household size (number of people in household)
            this is calculated based on the # of rows with the same CPSID


##Options for Y Variables (measures of hunger)

**FXTOTXPNC**: Indicates the total amount the entire household spent on food 
                last week.

**FSTOTXPNC_perper**: Indicates the total amount the household spent on food last 
                        week per person.  (Calculated using FSTOTPNC/hhsize)
                
                
**FSSTATUS**: Household food security scale

                ----Levels----
                01: Food Secure
                02: Low food secure
                03: Very Low Food Secure
            
            If food status > 1, then 1, if not, 0
            
            In other words, if a family **is** food secure, 0, if **not** food 
            secure 1
            
**FSSTATUSMD**: Detailed Household food security scale, 30-day
                
                ------Levels-------
                01: High Food Security
                02: Marginal Food Security
                03: Low Food security
                04: Very Low Food Security
                
                If status > 1, cell gets a 1, 0 otherwise
                
                In other words, if a family is High Food Security, they get a 0,
                if family is anything but High Food Security, they get a 1. 
                
                "Households are classified as high food secure if they report no food 
                insecure conditions than three food insecure conditions in the past 30 days. 
                Households with incomes greater than 185% of the poverty line are asked two
                screening questions about food insecurity and if they report no instances of
                food insecurity are also categorized as high food secure. Households
                classified with marginal food security are those reporting one or 
                two food insecure conditions."

                "Households without children are classified as low food security if they 
                report between 3-5 food insecure conditions and as very low food insecure 
                if they report 6 or more food insecure conditions. Households with 
                children are classified as low food insecure if they report 3-7 food 
                insecure conditions, and very low food insecure if they report 8 
                or more food insecure conditions in the past 30 days." (IPUMS)
                
                
**FSFOODS**: Indicates whether the household had enough to eat or enough of the 
            kinds of foods they wanted to eat in the past twelve months.
            
            Levels:
            01: Enough of the kinds of food we want to eat
            02: Enough but not always the kinds of food we want to eat
            03: Sometimes not enough to eat
            04: Often not enough to eat
            96: Refused
            97: Don't Know
            
            If level is > 1, then cell gets 0, 1 otherwise
            
            In other words, if the household has anything but "Enough of the 
            Kinds of food we want to eat", they get a 1 (classified as an 
            indication food insecurity), otherwise, 0
            
**FSWROUTY**: Indicates if, in the past year, the household was worried that 
              they would run out of food and not be able to afford more.
              
              ------Levels------
              01: Never True
              02: Sometimes True
              03: Often True
              
              If level > 1, then cell gets 1, otherwise 0
              
              In other words, if the household responds anything except "Never 
              True", this is an indication of food insecurity and given a 1.
              
**FSBAL**: Indicates whether or not the respondent(s) could not afford to eat 
            balanced meals at any time in the last 12 months.
            
            -----Levels-----
            01: Never True
            02: Sometimes True
            03: Often True
            
            If level > 1, then cell gets 1, otherwise 0
              
            In other words, if the household responds anything excpet "Never True",
            this is an indication of food insecurity and given a 1.
            
**FSRAWSCRA**: reports the total number of affirmative answers the household 
                provided to the 10 item household/adult food security 
                questionnaire. This raw score is used to determine the food 
                security of the adults in a household in the past year (FSSTATUS).
                
                -----Levels----
                00: No Affirmative Responses or Did not Pass Initial Screen
                01: 1
                02: 2
                .
                .
                .
                09: 9
                10: 10
                
                
                
              
            
            
            
          
            
            

    
    
    
    
    