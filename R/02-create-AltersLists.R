##########################################################################
# Prep for JKB's EpiModel EDA & Survival Analysis Datasets 
# https://github.com/statnet/EpiModelEDA/blob/master/README.md
# creating new object function based on "reshape_edgelist": 
# https://github.com/statnet/SHAMP/blob/master/egonet/R/formatting.R 
##########################################################################

library(tidyverse)

# load nsfg rds
nsfg_complete <- readRDS("~/nsfg_data_cleaning/Full/nsfg_complete.rds")

# source JKB's functions that we will need 
source("~/nsfg_data_cleaning/R/egodata_functions.R")

# we will need:
#  1. ego object 
#  2. alter object (one line for every partnership with ego)
#  3. egos & alters edgelist 

########################################################################
##### EGO OBJECT 
########################################################################

ego_vars <- c("ego", "intyr",
              "weight", "sex", "age", "agecat", "sqrtage", "agesquared",
              "birthyear", "birthcohort", "race", 
              "vsnplife", "vsnplife3", "LIFPRTNR", "partsyr3", "osnpyr", "maxospyr", "osnpyr3", "maxospyr3",
              "agefsex", "sexgrp", "sexgrp.yr", "vsnpyr", "samlifenum", "samyearnum", "SAMESEXANY", "EVMARCOH", "MARSTAT",
              "sex4wk","cond4wk", "HADSEX", "p.cond", 
              "pdeg.direct", "pdeg.active", 
              "deg.parts", "deg.marcoh", "deg.mar", "deg.cohab", "deg.other", 
              "instyr", "CMINTVW", "instyr.young", 
              "cmcohab", "cmmarr")

egos <- nsfg_complete %>% select(ego_vars)

saveRDS(egos, file="~/nsfg_data_cleaning/Objects/egos.rds")

########################################################################
##### ALTER OBJECTS
########################################################################

# this section uses JKB's code to build two sets of interest

# (1) "alters_all" creates the full alter dataset (3 obs per ego), then pares it down to all 
#     non-empty alters, then adds back in ego characteristics - for SURVIVAL ANALYSIS 

# (2) "alters" follows JKB's instructions for Epimodel EDA usage - only active partnersships, 
#     and separate "egos4alters" dataset

alter_vars <- c("ego", "weight",
                "active1", "active2", "active3",
                "sex1", "sex2", "sex3",
                "len1", "len2", "len3",
                "dfs1", "dls1", "dfs2", "dls2", "dfs3", "dls3",
                "rel1", "rel2", "rel3", "optype1", "optype2", "optype3",
                "page1", "page2", "page3", "agecat1", "agecat2", "agecat3",
                "prace1", "prace2", "prace3", 
                "diff.sqrt.age1", "diff.sqrt.age2", "diff.sqrt.age3",
                "once1", "once2", "once3",
                "condls1", "condfs1", "condls2", "condfs2", "condls3", "condfs3", 
                "partdurflag1", "partdurflag2", "partdurflag3")

alters <- nsfg_complete %>% select(alter_vars) 

alters <- reshape_edgelist(alters)

##################################################################
#### STEP 1: ACTIVE & INACTIVE PARTNERSHIPS FOR SURVIVAL ANALYSIS
##################################################################

alters_all <- alters %>%
  filter(optype<6) %>%
  rename(race = prace, edge_age_month = len, age=page) %>% 
  filter(age < 45) %>%
  filter(age > 14) %>%
  mutate(sqrtage = sqrt(age)) %>%
  mutate(agesquared = age^2) %>%
  mutate(reltype = ifelse(optype == 1, "Cur/Fmr Spouse",
                   ifelse(optype == 2, "Cur/Fmr Cohab",
                   ifelse(optype == 3, "Cur/Fmr Spouse", 
                   ifelse(optype == 4, "Cur/Fmr Cohab", 
                   ifelse(optype == 5, "Other", NA)))))) %>%
  mutate(network1 = ifelse((reltype %in% "Cur/Fmr Spouse" | reltype %in% "Cur/Fmr Cohab"), "marcoh", 
                    ifelse(reltype %in% "Other", "other", "inst"))) %>%
  mutate(network2 = ifelse(once==1 & active==0, "inst", "main")) %>%
  mutate(edge_id = row_number()) 


alters_allactivepartners <- alters %>%
  filter(optype<6) %>%
  rename(race = prace, edge_age_month = len, age=page) %>% 
  mutate(sqrtage = sqrt(age)) %>%
  mutate(agesquared = age^2) %>%
  mutate(reltype = ifelse(optype == 1, "Cur/Fmr Spouse",
                          ifelse(optype == 2, "Cur/Fmr Cohab",
                                 ifelse(optype == 3, "Cur/Fmr Spouse", 
                                        ifelse(optype == 4, "Cur/Fmr Cohab", 
                                               ifelse(optype == 5, "Other", NA)))))) %>%
  mutate(network1 = ifelse((reltype %in% "Cur/Fmr Spouse" | reltype %in% "Cur/Fmr Cohab"), "marcoh", 
                           ifelse(reltype %in% "Other", "other", "inst"))) %>%
  mutate(network2 = ifelse(once==1 & active==0, "inst", "main")) %>%
  filter(active==1)  %>% 
  mutate(edge_id = row_number()) 


##### SAVE OUT ACTIVE PARTS OBJECT TO BE USED FOR ERGM EGO ANALYSIS
# and also one-times
alters_once <- alters_all %>% filter(active==0, once==1)
saveRDS(alters_once, file="~/nsfg_data_cleaning/Objects/alters_once.rds")

alters_active <- alters_all %>% filter(active==1)
alters_inactive <- alters_all %>% filter(active==0)
saveRDS(alters_active , file="~/nsfg_data_cleaning/Objects/alters_active.rds")
saveRDS(alters_inactive, file="~/nsfg_data_cleaning/Objects/alters_inactive.rds")
saveRDS(alters_allactivepartners, file="~/nsfg_data_cleaning/Objects/alters_allactivepartners.rds")

########################################################################
##### EGO + ALTER MERGES FOR EPIMODEL EDA
########################################################################

# (1) For EPImodel EDA: 
# There is a 3rd data object required, "egos4alters.rds". 
# It's basically an edge list that contains the ego weights and attributes and an edge_id variable, 
# but no alter characteristics. 
# The file input/data/prep_data.Rmd shows how I created it, as well as how I created "alters.rds".

# Now, construct the corresponding egos data frame using a quick merge
egos4alters <- merge(egos, data.frame(ego=alters_active$ego,
                                      edge_id=alters_active$edge_id), all.y=TRUE)
egos4alters <- egos4alters[order(egos4alters$edge_id),]

sum(egos4alters$edge_id!=alters_active$edge_id)

saveRDS(egos4alters, file="~/nsfg_data_cleaning/Objects/egos4alters.rds")

########################################################################
####### ALTERS + EGOS MERGES FOR SURVIVAL ANALYSIS ##########
########################################################################

## DUE TO ADDING VARIABLES (after survival analysis project) THIS IS ALMOST CERTAINLY FUCKED
## NEED TO FIX WHICH EGO VARS AND COLUMNS I AM SELECTING 


# limit ego set to demographics, then rename cols to make sure no overalpping vars between alters and egos
# (except "ego", which merge will merge on)

new_egos <- egos[,c(1:10,14, 17:19)]
colnames(new_egos) <- c("ego", "e.intyr", "e.weight", "e.sex", "e.age", "e.agecat", 
                        "e.birthyear", "e.birthcohort",  "e.race", 
                        "e.deg.main", "e.partsyr3", "e.osnpyr3", "e.maxospyr3", "e.agefsex") 

alters_egos <- merge(alters_all, new_egos, all.x = T)

# add variable for dyad race cat 
alters_egos <- alters_egos %>%
  filter(active < 2) %>%
  filter(!(active==0 & once==1)) %>% 
  filter(edge_age_month < 996) %>% 

  # because age is not continuous in this data (i.e. age = 16, not 16.5), and rel dur is in months, 
  mutate(e.age.initial = round(e.age-(edge_age_month/12), 1)) %>%
  
  # set up age at beginning of reported relationship
  mutate(e.agecat.initial = ifelse(e.age.initial >= 15 & e.age.initial < 20, "15-19",
                            ifelse(e.age.initial >= 20 & e.age.initial < 25, "20-24", 
                            ifelse(e.age.initial >= 25 & e.age.initial < 30, "25-29", 
                            ifelse(e.age.initial >= 30 & e.age.initial < 35, "30-34", 
                            ifelse(e.age.initial >= 35 & e.age.initial < 40, "35-39", 
                            ifelse(e.age.initial >= 40 & e.age.initial < 45, "40-44", NA))))))) %>%
  
  # set up survival anaysis markers
  # set all rels that started this month dur=0.5
  mutate(edge_age_month = ifelse(edge_age_month==0, 0.5, edge_age_month)) %>%
  # time open, time closed, and exact dur flag
  mutate(t_o = ifelse((edge_age_month - 12 >= 0), (edge_age_month-12), 0)) %>%
  mutate(t_c = edge_age_month) %>%
  mutate(t_o_years = t_o/12) %>%
  mutate(t_c_years = t_c/12) %>%
  mutate(censored = ifelse(active==1, 0, 1)) %>% 
  # dummy vars for age category
# set up dummy vars for ego age cat
 mutate(e.15 = ifelse(e.agecat %in% "15-19", 1, 0)) %>% 
 mutate(e.20 = ifelse(e.agecat %in% "20-24", 1, 0)) %>% 
 mutate(e.25 = ifelse(e.agecat %in% "25-29", 1, 0)) %>% 
 mutate(e.30 = ifelse(e.agecat %in% "30-34", 1, 0)) %>% 
 mutate(e.35 = ifelse(e.agecat %in% "35-39", 1, 0)) %>% 
 mutate(e.40 = ifelse(e.agecat %in% "40-44", 1, 0)) %>% 

# salvage weird instances where partsyr3 ==0 but deg.main >0
 mutate(e.partsyr3 = ifelse(e.partsyr3 == 0 & alter > 0, alter, e.partsyr3)) 

saveRDS(alters_egos, file="~/nsfg_data_cleaning/Objects/altersegos_survdat.rds")

########################################################################
#### SAVE OUT ABOVE BUT ONLY ACTIVE PARTNERS FOR DESCRIPTIVES BOOK 
########################################################################

alters_egos_active <- alters_egos %>% filter(active==1)
saveRDS(alters_egos_active, file="~/nsfg_data_cleaning/Objects/altersegos_active.rds")


