#' Add Column for Last Birth
#' Find the last birth for all mothers and flag it in the column "last" as TRUE
#' 
#' @param dbm data in the same format as from \code{\link[twinR]{data_births_all}}
#'
#' @return original data with the column \strong{last} added 
#'
#' @examples 
#' bld <- twinR::filter_data(twinR::data_births_all)
#' bld_with_last <- add_last_birth(bld)
add_last_birth <- function(dbm){
  rows <- nrow(dbm)
  #get the maximum birth year and mother age for all mothers
  LBY <- aggregate(cbind(birth_year,maternal_age)  ~  maternal_id, dbm, FUN = max)
  LBY$last <- TRUE
  dbm <- merge(dbm, LBY, by= c("birth_year","maternal_age", "maternal_id"), all.x=T)
  
  #those that were not maximum set as not last
  dbm$last <- !is.na(dbm$last)
  
  #check for possible duplicates for each mother
  if(max(table(dbm[dbm$last, c("maternal_id")])) > 1){stop("Aggregate problem!")}
 
  if(nrow(dbm)!=rows){stop("Aggregate problem row mismatch!")}
  
  #return in expected order and class
  dbm <- tibble::as_tibble(dbm)
  dbm[order(dbm$maternal_id, dbm$birth_year),]
} 