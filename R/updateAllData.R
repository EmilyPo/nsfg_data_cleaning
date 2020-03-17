# run all scripts 
# do this when you update the data in order to get correctly updated objects

source("~/nsfg_data_cleaning/R/01_import-NSFG.R")
source("~/nsfg_data_cleaning/R/02-create-AltersLists.R")
source("~/nsfg_data_cleaning/R/03-youngNSFG.R") # can skip this step
source("~/nsfg_data_cleaning/R/04-Egodata.R")
