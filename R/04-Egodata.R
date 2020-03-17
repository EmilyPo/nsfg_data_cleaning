# This file takes the ego list and alter lists and combines them into one big list (so I don't have to load each alter list separately every time)
# I will still need to make the egodata objects prior to using ergm.ego
# this script also removes those egos who are <45 but had alters who were > 45
# and removs all egos who report more than 1 marriage/cohab

library(tidyverse)

### Full Dataset ###
# load files
egos <- readRDS("~/nsfg_data_cleaning/Objects/egos.rds")
alters_active <- readRDS("~/nsfg_data_cleaning/Objects/alters_active.rds")
alters_inactive <- readRDS("~/nsfg_data_cleaning/Objects/alters_inactive.rds")
alters_once <- readRDS("~/nsfg_data_cleaning/Objects/alters_once.rds")

######### generate egodata for active partners of all ages #################
allpartners <- readRDS("~/nsfg_data_cleaning/Objects/alters_allactivepartners.rds") 
allpartners <- allpartners %>% mutate(race = ifelse(race %in% "miss/ref/dk", NA,ifelse(race %in% "n.ask", NA, race)))
allMarcoh <- allpartners %>% filter(optype %in% 1:2)
allOther <- allpartners %>% filter(optype == 5)

w <- allMarcoh$ego # (egos who have alters in alter list)
y <- table(w) # tabulate
z <- as.numeric(names(y[y>=2])) # pull ego ids of those who have more than 1 marriage/cohab
d <- allMarcoh[allMarcoh$ego %in% z,] 
allMarcoh <- allMarcoh[-as.integer(rownames(d)),] # remove those alters from alters list

# update ego degree based on alters lists ##
# this is necessary because some alters were removed above, and were removed earlier because they were outside the age range
# update degree for "others"
egos2 <- egos
egos2$deg.other[egos2$ego %in% setdiff(egos2$ego, allOther$ego)] <- 0
tab <- as.numeric(table(allOther$ego))
others <- intersect(egos2$ego, allOther$ego)

egos2$deg.other[egos2$ego %in% others] <- tab
egos2$deg.other.binary <- as.integer(egos2$ego %in% allOther$ego) 
egos2$deg.marcoh <- as.integer(egos2$ego %in% allMarcoh$ego)

EgodataAllAltersAges <- list("egos" = egos2, 
                    "altersMarcoh" = allMarcoh, 
                    "altersOther" = allOther 
)

saveRDS(EgodataAllAltersAges, "~/nsfg_data_cleaning/Objects/EgodataAllAltersAges.rds")


######## return to regularly scheduled programming ###############
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

# get new ego degree for marcoh 
egosMarcohDegree <- as.integer(egos$ego %in% altersMarCoh$ego)
egosOtherDegree <- as.integer(egos$ego %in% altersOther$ego) 
# assign new ego degree
egos$deg.marcoh <- egosMarcohDegree
egos$deg.other <- egosOtherDegree

# get new ego degree for casual (true degree and binary)
egos$deg.other[egos$ego %in% setdiff(egos$ego, altersOther$ego)] <- 0
tab <- as.numeric(table(altersOther$ego))
others <- intersect(egos$ego, altersOther$ego)

egos$deg.other[egos$ego %in% others] <- tab
egos$deg.other.binary <- as.integer(egos$ego %in% altersOther$ego) 

## check 
mars <- nrow(altersMarCoh)
deg <- sum(egos$deg.marcoh)
mars; deg # SHOULD BE THE SAME

pers <- nrow(altersOther)
degO <- sum(egos$deg.other)
pers;degO # SHOULD BE THE SAME

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

saveRDS(fullEgodata, "~/nsfg_data_cleaning/Objects/fullEgodata.rds")

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

saveRDS(fullEgodataYoung, "~/nsfg_data_cleaning/Objects/fullEgodataYoung.rds")