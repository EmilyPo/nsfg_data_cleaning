# same as 02, but pare down dataset and objects for egos and alters aged 15-29
# include new variables for 'in relationship but alter is older'
# trying to get at boundary issues  


library(tidyverse)
library(here)

# load nsfg rds
nsfg_complete <- readRDS("~/NSFG_DATA/Full/nsfg_complete.rds")

# source JKB's functions that we will need 
source("~/NSFG_DATA/R/egodata_functions.R")

# pare down to egos and alters aged < 30

nsfg_young_egos <- nsfg_complete %>%
                # remove egos first 
                filter(age < 30) %>%
                # new alter variable for if inactive within age range
                mutate(active.y1 = ifelse((page1 < 30 & page1 > 14), active1, 0)) %>%
                mutate(active.y2 = ifelse((page2 < 30 & page2 > 14), active2, 0)) %>%
                mutate(active.y3 = ifelse((page3 < 30 & page3 > 14), active3, 0)) %>%
                # degrees based on married/cohab & pers within age range
                mutate(deg.cohab.y1 = ifelse(is.na(active.y1), 0, ifelse((active.y1==1 & (rel1==1|rel1==2)), 1, 0))) %>%
                mutate(deg.cohab.y2 = ifelse(is.na(active.y2), 0, ifelse((active.y2==1 & (rel2==1|rel2==2)), 1, 0))) %>%
                mutate(deg.cohab.y3 = ifelse(is.na(active.y3), 0, ifelse((active.y3==1 & (rel3==1|rel3==2)), 1, 0))) %>%
                mutate(deg.cohab.y =  deg.cohab.y1 + deg.cohab.y2 + deg.cohab.y3) %>%
                
                mutate(deg.pers.y1 = ifelse(is.na(active.y1), 0, ifelse((active.y1==1 & rel1==3), 1, 0))) %>%
                mutate(deg.pers.y2 = ifelse(is.na(active.y2), 0, ifelse((active.y2==1 & rel2==3), 1, 0))) %>%
                mutate(deg.pers.y3 = ifelse(is.na(active.y3), 0, ifelse((active.y3==1 & rel3==3), 1, 0))) %>%
                mutate(deg.pers.y = deg.pers.y1 + deg.pers.y2 + deg.pers.y3)

# egos < 30 and all alters 
saveRDS(nsfg_young_egos, file = here("Objects", "nsfg_young_egos.rds"))


########################################################################
### EGO OBJECT
########################################################################

ego_vars <- c("ego", "weight", "sex", "age", "agecat", "sqrtage", "agesquared",
              "birthyear", "birthcohort", "intyr", "race", 
              "vsnplife", "vsnplife3", "LIFPRTNR", "partsyr3",
              "sexgrp", "sexgrp.yr", "vsnpyr", "samlifenum", "samyearnum", "SAMESEXANY", "EVMARCOH", "MARSTAT",
              "sex4wk","cond4wk", "HADSEX", "p.cond",
              "pdeg.direct", "pdeg.active", 
              "deg.parts", "deg.marcoh", "deg.mar", "deg.cohab", "deg.other", "deg.cohab.y", "deg.pers.y", "instyr",
              "CMINTVW", "instyr.young", 
              "cmcohab", "cmmarr")

egos <- nsfg_young_egos %>% select(ego_vars)

saveRDS(egos, file="~/NSFG_DATA/Objects/egos_young.rds")

# 2. alter objects

# need alter list of all alters who correspond to egos < 30 
# and another for alters < 30 who correspond with egos < 30

#### alter sets ####
alter_vars <- c("ego", "weight",
                "active1", "active2", "active3", "active.y1", "active.y2", "active.y3",
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

alters_all <- nsfg_young_egos %>%
              select(alter_vars) 
  
alters_all <- reshape_edgelist(alters_all)


##################################################################

alters_all <- alters_all %>%
  rename(race = prace, edge_age_month = len, age=page) %>% 
  mutate(network = ifelse(once==1 & active==0, "inst", "main")) %>%
  mutate(network2 = ifelse((rel==1|rel==2), "cohab", 
                    ifelse(rel==3, "pers", 
                    ifelse(once==1 & active==0, "inst", NA)))) %>%
  mutate(reltype = ifelse(optype == 1, "Cur/Fmr Spouse",
                          ifelse(optype == 2, "Cur/Fmr Cohab",
                                 ifelse(optype == 3, "Cur/Fmr Spouse", 
                                        ifelse(optype == 4, "Cur/Fmr Cohab", 
                                               ifelse(optype == 5, "Other", NA)))))) %>%
  mutate(edge_id = row_number()) %>%
  filter(active==1)

alters_young <- alters_all %>%
  filter(active.y==1)

##### SAVE OUT ACTIVE PARTS OBJECT TO BE USED FOR ERGM EGO ANALYSIS
saveRDS(alters_all, file="~/NSFG_DATA/Objects/alters_all_for_young_egos.rds")
saveRDS(alters_young , file="~/NSFG_DATA/Objects/alters_young_for_young_egos.rds")


