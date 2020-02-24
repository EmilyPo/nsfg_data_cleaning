# This file takes the ego list and alter lists and combines them into one big list (so I don't have to load each alter list separately every time)
# I will still need to make the egodata objects prior to using ergm.ego
# this script also removes those egos who are <45 but had alters who were > 45
# and removs all egos who report more than 1 marriage/cohab

library(tidyverse)

### Full Dataset ###
# load files
egos <- readRDS("~/NSFG_DATA/Objects/egos.rds")
alters_active <- readRDS("~/NSFG_DATA/Objects/alters_active.rds")
alters_inactive <- readRDS("~/NSFG_DATA/Objects/alters_inactive.rds")
alters_once <- readRDS("~/NSFG_DATA/Objects/alters_once.rds")

# make all "miss/dk/ref" and "not asked" alter race == NA

alters_active <- alters_active %>%
                      mutate(race = ifelse(race %in% "miss/ref/dk", NA,
                       ifelse(race %in% "n.ask", NA, race)))

alters_inactive <- alters_inactive %>%
  mutate(race = ifelse(race %in% "miss/ref/dk", NA,
                       ifelse(race %in% "n.ask", NA, race)))

alters_once <- alters_once %>%
  mutate(race = ifelse(race %in% "miss/ref/dk", NA,
                       ifelse(race %in% "n.ask", NA, race)))

#### separate out mar/cohs and others (note that this eliminates all active former marriages and cohabs)
altersMarCoh <- alters_active %>% filter(active == 1, optype %in% 1:2)
altersMarriages <- alters_active %>% filter(active == 1, optype == 1)
altersCohabs <- alters_active %>% filter(active == 1, optype == 2)
altersOther <- alters_active %>% filter(active == 1, optype == 5)
altersMarCohInactive <- alters_inactive %>% filter(active == 0, optype %in% 1:4)
altersOtherInactive <- alters_inactive %>% filter(active == 0, optype == 5)

#### delete the alters in the MarCoh network that have an ego reporting more than 1 marriage or cohab 
w <- altersMarCoh$ego # (egos who have alters in alter list)
y <- table(w) # tabulate
z <- as.numeric(names(y[y>=2])) # pull ego ids of those who have more than 1 marriage/cohab
d <- altersMarCoh[altersMarCoh$ego %in% z,] 

altersMarCoh <- altersMarCoh[-as.integer(rownames(d)),] # remove those alters from alters list

#### update ego degree based on alters lists #######
# this is necessary because some alters were removed above, and were removed earlier because they were outside the age range

# get new ego degree
egosMarcohDegree <- as.integer(egos$ego %in% altersMarCoh$ego)
egosOtherDegree <- as.integer(egos$ego %in% altersOther$ego) 
# assign new ego degree
egos$deg.marcoh <- egosMarcohDegree
egos$deg.other <- egosOtherDegree

## check 
mars <- nrow(altersMarCoh)
deg <- sum(egos$deg.marcoh)
mars; deg # SHOULD BE THE SAME

pers <- nrow(altersOther)
degO <- sum(egos$deg.other)
pers;degO # SHOULD BE CLOSE, NOT IDENTICAL DUE TO CONCURRENCY (this is a binary indicator)

#### make the egodata object ###### WILL NEED TO UPDATE FOR ALL ALTERS IN TRANSITIONAL NETWORK LATER
fullEgodata <- list("egos" = egos, 
                    #"altersAllActive" = alters_active,
                    "altersMarCoh" = altersMarCoh, 
                    #"altersMarriages" = altersMarriages,
                    #"altersCohabs" = altersCohabs,
                    "altersOther" = altersOther 
                    #"altersMarCohInactive" = altersMarCohInactive, 
                    #"altersOtherInactive" = altersOtherInactive, 
                    #"altersOnce" = alters_once
                    )

saveRDS(fullEgodata, "~/NSFG_DATA/Objects/fullEgodata.rds")

### Young Egos ### IGNORE THIS FOR NOW-- will need to update degree as above if we want to use this again 
# just the actives and inst

egosYoung <- egos %>% filter(age<30)
alters_activeYoung <- alters_active %>% filter(age<30)
alters_onceYoung <- alters_once %>% filter(age<30)
altersMarCohYoung <- altersMarCoh %>% filter(age <30)
altersMarriagesYoung <- altersMarriages %>% filter(age<30)
altersCohabsYoung <- altersCohabs %>% filter(age<30)
altersOtherYoung <- altersOther %>% filter(age<30)


fullEgodataYoung <- list("egos" = egosYoung, 
                    "altersAllActive" = alters_activeYoung,
                    "altersMarCoh" = altersMarCohYoung, 
                    "altersMarriages" = altersMarriagesYoung,
                    "altersCohabs" = altersCohabsYoung,
                    "altersOther" = altersOtherYoung, 
                    "altersOnce" = alters_onceYoung)

saveRDS(fullEgodataYoung, "~/NSFG_DATA/Objects/fullEgodataYoung.rds")