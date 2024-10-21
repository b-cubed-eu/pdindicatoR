
#' Aggregate datacube over grid cell to create new dataframe with species list per grid
#'
#' This function aggregates a provided datacube over grid cell id, so that a new datacube
#' is outputted with 3 variables that contain the lists of species that are observed for
#' each grid cell (as speciesKeys, ott_id's and names).
#'
#' @param mcube An occurence datacube with appended ott_id's, as produced by the append_ott_id function
#' @param cube A dataframe with for selected taxa, the number of occurrences per
#' taxa and per grid cell
#' @return A dataframe with for each grid cell
#' @example
#' @export

# aggregate_cube <- function(mcube){
#   simpl_cube <- mcube[,c("year", "eeaCellCode","speciesKey","ott_id", "unique_name")]
#   simpl_cube$eeaCellCode <- factor(simpl_cube$eeaCellCode)
#   simpl_cube$year <- factor(simpl_cube$year)
#   aggr_cube <- simpl_cube %>% group_by(eeaCellCode, year) %>%
#   summarize(speciesKeys = list(speciesKey), ott_ids = list(ott_id), names = list(unique_name)) %>%
#   mutate(unique_spkeys = lapply(speciesKeys, unique)) %>%
#   mutate(unique_ott_ids = lapply(ott_ids, unique)) %>%
#   mutate(unique_names = lapply(names, unique))
#   return(aggr_cube)
# }

aggregate_cube <- function(mcube){
  simpl_cube <- mcube[,c("year", "eeaCellCode","speciesKey","ott_id", "unique_name")]
  simpl_cube$eeaCellCode <- factor(simpl_cube$eeaCellCode)
  simpl_cube$year <- factor(simpl_cube$year)
  aggr_cube <- simpl_cube %>% group_by(eeaCellCode, year) %>%
    summarize(speciesKeys = list(speciesKey), ott_ids = list(ott_id), names = list(unique_name)) %>%
    mutate(unique_spkeys = lapply(speciesKeys, unique)) %>%
    mutate(unique_ott_ids = lapply(ott_ids, unique)) %>%
    mutate(unique_names = lapply(names, unique))
  return(aggr_cube)
}
