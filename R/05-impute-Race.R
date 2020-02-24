# ------------------------------------------------------------
# lookup.ego (function JKB wrote for SHAMP)
# ------------------------------------------------------------
#' Look up an ego value for an alter
#'
#' @param egodata Egodata object
#' @param var Variable to look up
#' @param subset.rows Row numbers of alters to keep before doing the lookup

#' @export

lookup.ego <- function(egodata, var, subset.rows=NULL) {
  egoIDcol <- egodata$egoIDcol
  # Subset the alters first?
  if (!is.null(subset.rows)) egodata$alters <- egodata$alters[subset.rows,]
  # Alters' ego IDs
  xdat <- data.frame(egoID=egodata$alters[,egoIDcol])
  xdat$sort.orig <- 1:nrow(xdat)
  # Ego data
  if (!var%in%colnames(egodata$egos)) stop('Variable not found in ego data')
  ydat <- data.frame(egoID=egodata$egos[,egoIDcol], var=egodata$egos[,var])
  # Merge
  findit <- merge(xdat, ydat, all.x=TRUE, sort=FALSE)
  findit <- findit[order(findit$sort.orig),]
  
  if (sum(findit$egoID==xdat$egoID)!=nrow(xdat)) stop('In lookup.ego, merge did not work')
  return(subset(findit, select=-sort.orig))
}


# ------------------------------------------------------------
# impute_race 
# ------------------------------------------------------------

#' Function to impute missing alter race given ego race-specific probabilities 
#' 
#' Targets based on non-missing alter race data (see Descriptives Book / Alter Race Issues)
#' 
#' Based on JKB's "impute_immigration" function for SHAMP
#' 
#' @param egodata ergm.ego egodata object
#' @param alter.rows row numbers of alters for whom race needs to be imputed
#' @param targets matrix where ego race is rows and alter race is columns, and cells indicate P(alter race|ego race)
#' @param name of race variable in egodata$egos, levels should match names of targets 
#' 
#' @example 
#' 
#' 
#' @export

impute_race <- function(egodata, alter.rows, targets, racevar){
  
  # Grab race of egos
  ego.race <- as.character(lookup.ego(egodata, racevar, subset.rows=alter.rows)$var)
  
  # sample with target 
  
}





