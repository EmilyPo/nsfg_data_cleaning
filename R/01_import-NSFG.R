# Emily Pollock
# Converting & Cleaning NSFG dataset
# Updated August 2018, Switched to Tidyverse Oct 2018, acasi function Nov 2018

# this file imports full combined NSFG sav file, creates variables, removes unwanted variables, saves as smaller file 

library(foreign)
library(here)
library(tidyverse)

##### read in cleaned NSFG dataset & add variables #####
nsfg <- read.spss(here("Full", "FinalNSFG_EP.sav"), to.data.frame = T, use.value.labels = F, use.missings = F)

##### salvage acasi partner age for current partners of under-18 respondents ####
acasi <- function(x){
  # goal of function is to salvage age of current partners of under-18 year old respondents from acasi
  # based on MM's spss cleaning, they will have partner age already specified if they are married or have a non-active partner
  # this picks up the vast majority of partner ages for egos < 18
  
  # condition 1: most recent partner is first current partner 
  cond1 <- which(x$page1==95 & x$active1 == 1 & x$CURRPAGE < 90)
  x$page1[cond1] <- x$CURRPAGE[cond1]
  
  #condition 2: second most recent partner is first current partner
  cond2 <- which(x$page1 > 0 & x$page2==95 & x$active2 == 1 & x$CURRPAGE < 90)
  x$page2[cond2] <- x$CURRPAGE[cond2]
  
  #condition 3: third most recent partner is first current partner 
  cond3 <- which(x$page1 > 0 & x$page2 > 0 & x$page3==95 & x$active3 == 1 & x$CURRPAGE < 90)
  x$page3[cond3] <- x$CURRPAGE[cond3]
  
  # condition 4: second most recent partner is second current partner 
  cond4 <- which(x$page1 > 0 & x$page2==95 & x$active2 == 1 & x$CURRPAGE2 < 90)
  x$page2[cond4] <- x$CURRPAGE2[cond4]
  
  # condition 5: third most recent partner is second current partner 
  cond5 <- which(x$page1 > 0 & x$page2 > 0 & x$page3==95 & x$active3 == 1 & x$CURRPAGE2 < 90)
  x$page3[cond5] <- x$CURRPAGE2[cond5]
  
  # condition 6: third most recent partner is third current partner 
  cond6 <- which(x$page1 > 0 & x$page2 > 0 & x$page3==95 & x$active3 == 1 & x$CURRPAGE3 < 90)
  x$page3[cond6] <- x$CURRPAGE3[cond6]
  
  return(x)  
}

nsfg_complete <- acasi(nsfg)


########## new / altered variables ##############

nsfg_complete <- nsfg_complete %>%
  
          # covert all age 45 to age 44
          mutate(age = ifelse(age==45, 44, age)) %>%
  
          filter(age >= 15) %>%
  
          mutate(sqrtage = sqrt(age)) %>%
          mutate(agesquared = age^2) %>%
  
          #age cat variable to match census groups
          mutate(agecat = ifelse(age %in% 15:19, "15-19",
                          ifelse(age %in% 20:24, "20-24", 
                          ifelse(age %in% 25:29, "25-29", 
                          ifelse(age %in% 30:34, "30-34", 
                          ifelse(age %in% 35:39, "35-39", 
                          ifelse(age %in% 40:44, "40-44", NA))))))) %>%
          # birth year 
          mutate(birthyear = (1900 + trunc((CMBIRTH - 1)/12))) %>%
          
          # birth cohort 
          mutate(birthcohort = ifelse(birthyear %in% 1960:1964, "1960",
                               ifelse(birthyear %in% 1965:1969, "1965",
                               ifelse(birthyear %in% 1970:1974, "1970", 
                               ifelse(birthyear %in% 1975:1979, "1975",
                               ifelse(birthyear %in% 1980:1984, "1980",
                               ifelse(birthyear %in% 1985:1989, "1985",
                               ifelse(birthyear %in% 1990:1994, "1990",
                               ifelse(birthyear %in% 1995:2000, "1995", NA))))))))) %>%
  
          # agecat variable for alters
          mutate(agecat1 = ifelse(page1 < 15, "14 & under", 
                           ifelse(page1 %in% 15:19, "15-19",
                           ifelse(page1 %in% 20:24, "20-24", 
                           ifelse(page1 %in% 25:29, "25-29", 
                           ifelse(page1 %in% 30:34, "30-34", 
                           ifelse(page1 %in% 35:39, "35-39", 
                           ifelse(page1 %in% 40:44, "40-44", 
                           ifelse(page1 > 44 & page1 < 95, "45 & above", NA))))))))) %>% 
  
          mutate(agecat2 = ifelse(page2 < 15, "14 & under", 
                            ifelse(page2 %in% 15:19, "15-19",
                            ifelse(page2 %in% 20:24, "20-24", 
                            ifelse(page2 %in% 25:29, "25-29", 
                            ifelse(page2 %in% 30:34, "30-34", 
                            ifelse(page2 %in% 35:39, "35-39", 
                            ifelse(page2 %in% 40:44, "40-44", 
                            ifelse(page2 > 44 & page2 < 95, "45 & above", NA))))))))) %>% 
  
          mutate(agecat3 = ifelse(page3 < 15, "14 & under", 
                            ifelse(page3 %in% 15:19, "15-19",
                            ifelse(page3 %in% 20:24, "20-24", 
                            ifelse(page3 %in% 25:29, "25-29", 
                            ifelse(page3 %in% 30:34, "30-34", 
                            ifelse(page3 %in% 35:39, "35-39", 
                            ifelse(page3 %in% 40:44, "40-44", 
                            ifelse(page3 > 44 & page3 < 95, "45 & above", NA))))))))) %>% 
  
          #convert HISPRACE into categorical race4 (4-category race variable) (1-hispanic, 2-nh white, 3-nh black, 4-nh other)
          mutate(race = ifelse(HISPRACE %in% 1, "h",
                        ifelse(HISPRACE %in% 2, "w",
                        ifelse(HISPRACE %in% 3, "b", 
                        ifelse(HISPRACE %in% 4, "o", NA))))) %>%
  
          #convert partner races into above 4-race cats         
          # NOTE: partner race is missing for one-off partners of women 
          mutate(prace1 = ifelse(prace1 %in% 1, "h",
                          ifelse(prace1 %in% 2, "w",
                          ifelse(prace1 %in% 3, "b", 
                          ifelse(prace1 %in% 4, "o", 
                          ifelse(prace1 %in% 5, "n.ask",
                          ifelse(prace1 %in% 6, "fp", 
                          ifelse(prace1 %in% 7, "ns", 
                          ifelse(prace1 %in% 9, "miss/ref/dk", 
                          NA))))))))) %>%
  
          mutate(prace2 = ifelse(prace2 %in% 1, "h",
                          ifelse(prace2 %in% 2, "w",
                          ifelse(prace2 %in% 3, "b", 
                          ifelse(prace2 %in% 4, "o",
                          ifelse(prace2 %in% 5, "n.ask",
                          ifelse(prace2 %in% 6, "fp", 
                          ifelse(prace2 %in% 7, "ns", 
                          ifelse(prace2 %in% 9, "miss/ref/dk", 
                          NA))))))))) %>%
      
          mutate(prace3 = ifelse(prace3 %in% 1, "h",
                          ifelse(prace3 %in% 2, "w",
                          ifelse(prace3 %in% 3, "b", 
                          ifelse(prace3 %in% 4, "o", 
                          ifelse(prace3 %in% 5, "n.ask",
                          ifelse(prace3 %in% 6, "fp", 
                          ifelse(prace3 %in% 7, "ns", 
                          ifelse(prace3 %in% 9, "miss/ref/dk", 
                          NA))))))))) %>%
  
          # make sex categorial
          mutate(sex = ifelse(sex==1, "F", ifelse(sex==2, "M", NA))) %>% 
  
          # alter sex
          mutate(sex1 = ifelse(sex %in% "F", "M", ifelse(sex %in% "M", "F", NA))) %>% 
          mutate(sex2 = ifelse(sex %in% "F", "M", ifelse(sex %in% "M", "F", NA))) %>% 
          mutate(sex3 = ifelse(sex %in% "F", "M", ifelse(sex %in% "M", "F", NA))) %>% 
          
          # percent of times a condom was used in last 4 weeks
          mutate(p.cond = ifelse(cond4wk/sex4wk > 1, 1, cond4wk/sex4wk)) %>%
  
          # absolute value of the difference in the square root of age between partners 
          mutate(diff.sqrt.age1 = abs(sqrt(age) - sqrt(page1))) %>%
          mutate(diff.sqrt.age2 = abs(sqrt(age) - sqrt(page2))) %>%
          mutate(diff.sqrt.age3 = abs(sqrt(age) - sqrt(page3))) %>%
  
          # degree based on all partners (excluding one-times)
          mutate(deg.parts1 = ifelse(is.na(active1), 0, ifelse((active1==1), 1, 0))) %>%
          mutate(deg.parts2 = ifelse(is.na(active2), 0, ifelse((active2==1), 1, 0))) %>%
          mutate(deg.parts3 = ifelse(is.na(active3), 0, ifelse((active3==1), 1, 0))) %>%
          mutate(deg.parts = deg.parts1 + deg.parts2 + deg.parts3) %>%
  
          # degrees based on married or cohab (for counterfactual model)
          mutate(deg.marcoh1 = ifelse(is.na(active1), 0, ifelse((active1==1 & (rel1==1|rel1==2)), 1, 0))) %>%
          mutate(deg.marcoh2 = ifelse(is.na(active2), 0, ifelse((active2==1 & (rel2==1|rel2==2)), 1, 0))) %>%
          mutate(deg.marcoh3 = ifelse(is.na(active3), 0, ifelse((active3==1 & (rel3==1|rel3==2)), 1, 0))) %>%
          mutate(deg.marcoh =  deg.marcoh1 + deg.marcoh2 + deg.marcoh3) %>%
  
          # degrees based on married (for transition model)
          mutate(deg.mar1 = ifelse(is.na(active1), 0, ifelse((active1==1 & rel1==1), 1, 0))) %>%
          mutate(deg.mar2 = ifelse(is.na(active2), 0, ifelse((active2==1 & rel2==1), 1, 0))) %>%
          mutate(deg.mar3 = ifelse(is.na(active3), 0, ifelse((active3==1 & rel3==1), 1, 0))) %>%
          mutate(deg.mar =  deg.mar1 + deg.mar2 + deg.mar3) %>%
          
          # degrees based on cohab (for transition model)
          mutate(deg.cohab1 = ifelse(is.na(active1), 0, ifelse((active1==1 & rel1==2), 1, 0))) %>%
          mutate(deg.cohab2 = ifelse(is.na(active2), 0, ifelse((active2==1 & rel2==2), 1, 0))) %>%
          mutate(deg.cohab3 = ifelse(is.na(active3), 0, ifelse((active3==1 & rel3==2), 1, 0))) %>%
          mutate(deg.cohab =  deg.cohab1 + deg.cohab2 + deg.cohab3) %>%
  
          # degrees based on other (for both models)
          mutate(deg.other1 = ifelse(is.na(active1), 0, ifelse((active1==1 & rel1==3), 1, 0))) %>%
          mutate(deg.other2 = ifelse(is.na(active2), 0, ifelse((active2==1 & rel2==3), 1, 0))) %>%
          mutate(deg.other3 = ifelse(is.na(active3), 0, ifelse((active3==1 & rel3==3), 1, 0))) %>%
          mutate(deg.other = deg.other1 + deg.other2 + deg.other3) %>%
          
          # inst rels (all and within young pop)
          mutate(instyr1 = ifelse(active1==0 & once1==1, 1, 0)) %>%
          mutate(instyr2 = ifelse(active2==0 & once2==1, 1, 0))%>%
          mutate(instyr3 = ifelse(active3==0 & once3==1, 1, 0)) %>%
          mutate(instyr = instyr1 + instyr2 + instyr3) %>%
  
          mutate(instyr1y = ifelse(active1==0 & once1==1 & page1 >15 & page1 <30, 1, 0)) %>%
          mutate(instyr2y = ifelse(active2==0 & once2==1 & page1 >15 & page1 <30, 1, 0))%>%
          mutate(instyr3y = ifelse(active3==0 & once3==1 & page1 >15 & page1 <30, 1, 0)) %>%
          mutate(instyr.young = instyr1y + instyr2y + instyr3y) %>%
          
  
          # make sex groups into categorical vars (nres = no recent or ever sex)
          mutate(sexgrp = ifelse(sexgrp==1, "F", 
                          ifelse(sexgrp==2, "MSF",
                          ifelse(sexgrp==3, "MSMF",
                          ifelse(sexgrp==4, "MSM", 
                          ifelse(sexgrp==7, "no p 1yr",
                          ifelse(sexgrp==8, "never sex", NA))))))) %>%

         mutate(sexgrp.yr = ifelse(sexgrp.yr==1, "F", 
                            ifelse(sexgrp.yr==2, "MSF",
                            ifelse(sexgrp.yr==3, "MSMF",
                            ifelse(sexgrp.yr==4, "MSM",  
                            ifelse(sexgrp.yr==7, "no p 1yr",
                            ifelse(sexgrp.yr==8, "never sex", NA))))))) %>%
        # cap parts in last year at 3 
        # vaginal sex parts
        mutate(partsyr3 = ifelse(PARTS1YR > 3, 3, PARTS1YR)) %>%
        # all opposite sex parts (oral, anal, vaginal etc)
        mutate(osnpyr3 = ifelse(osnpyr > 3, 3, osnpyr)) %>%
        # max os estimate from multiple sources
        mutate(maxospyr3 = ifelse(maxospyr > 3, 3, maxospyr)) %>%
  
        # new var for century month of initial cohabitation current marriage or current cohab
        # cmcurmar = original variable for males
        # cmstrthp = females
        # valid skips 9997-9999
        mutate(cmcohab = ifelse(sex %in% "M" & CMCURCOH < 9997, CMCURCOH, 
                        ifelse(sex %in% "F" & CMSTRTHP < 9997, CMSTRTHP, NA))) %>% 
        # make NA all Fs who began cohab at marriage (Ms in this situation excluded by original question universe)
        mutate(cmcohab = ifelse(sex %in% "F" & MARSTAT ==1 & CMSTRTHP >= CMMARRCH, NA, cmcohab)) %>% 
  
        # cm of current marriage
        mutate(cmmarr = ifelse(sex %in% "M", CMCURMAR, ifelse(sex %in% "F" & MARSTAT ==1, CMMARRCH, NA))) %>%
        
        # change name of partdur imputation flag variable for ease of alter list generation
  
        mutate(partdurflag1 = PARTDUR1_I,
               partdurflag2 = PARTDUR2_I, 
               partdurflag3 = PARTDUR3_I) %>%
  
        # change ego id into row number (so they will be in order)
        mutate(ego = row_number()) %>%
        
        # convert population weight to sample weight
        mutate(weight = weight*(nrow(nsfg_complete)/sum(weight)))
        




vars <- c("ego", "weight", "intyr", "sex", "age", "agecat", "sqrtage", "agesquared",
          "birthyear", "birthcohort", "race", 
          "HADSEX", "vsnplife", "vsnplife3", "LIFPRTNR", "partsyr3", "PARTS1YR", "osnpyr", "maxospyr", "osnpyr3", "maxospyr3",
          "sexgrp", "sexgrp.yr", "vsnpyr", "samlifenum", "samyearnum", "SAMESEXANY", "EVMARCOH", "MARSTAT",
          "pdeg.direct", "pdeg.active",
          "deg.parts", "deg.marcoh", "deg.mar", "deg.cohab", "deg.other", 
          "sex4wk","cond4wk", "p.cond",
          "active1", "active2", "active3", "sex1", "sex2", "sex3",
          "len1", "len2", "len3", 
          "PARTDUR1", "PARTDUR2", "PARTDUR3", "partdurflag1", "partdurflag2", "partdurflag3",
          "optype1", "optype2", "optype3", "rel1", "rel2", "rel3", 
          "page1", "page2", "page3", "agecat1", "agecat2", "agecat3",
          "prace1", "prace2", "prace3", 
          "condls1", "condfs1", "condls2", "condfs2", "condls3", "condfs3",
          "dfs1", "dls1", "dfs2", "dls2", "dfs3", "dls3",
          "diff.sqrt.age1", "diff.sqrt.age2", "diff.sqrt.age3",
          "once1", "once2", "once3", "instyr", "agefsex",
          "CMINTVW", "instyr.young", "cmcohab", "cmmarr")


nsfg_complete <- nsfg_complete %>% select(vars)

saveRDS(nsfg_complete, file = here("Full", "nsfg_complete.rds"))

