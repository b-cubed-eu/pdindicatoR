
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

aggregate_cube <- function(mcube, timegroup=NULL) {
  columns_to_select <- c("year", "eeaCellCode", "speciesKey", "ott_id", "unique_name", "orig_tiplabel")
  simpl_cube <- mcube[, intersect(columns_to_select, colnames(mcube))]
  min_year <- min(simpl_cube$year)

  # When occurrences are already aggregated over time or when no timegroup is
  # specified
  if (!("year" %in% colnames(simpl_cube)) || missing(timegroup)) {
    aggr_cube <- simpl_cube %>%
      group_by(eeaCellCode) %>%
      reframe(
        speciesKeys = list(unique(speciesKey)),
        ott_ids = list(unique(ott_id)),
        unique_names = list(unique(unique_name)),
        orig_tiplabels = list(unique(orig_tiplabel))
      )

  # When timegroup ==1
  } else if(timegroup==1){
      aggr_cube <- simpl_cube %>%
      group_by(eeaCellCode, year) %>%
      reframe(
        speciesKeys = list(unique(speciesKey)),
        ott_ids = list(unique(ott_id)),
        unique_names = list(unique(unique_name)),
        orig_tiplabels = list(unique(orig_tiplabel))
      )%>%
      rename(period = year)
  } else {

  # Calculate the 5-year period for each row
   aggr_cube <- simpl_cube %>%
    mutate(period = min_year + 5 * ((year - min_year) %/% 5)) %>%
     mutate(period = paste(period, period + 4, sep = "-")) %>%
    group_by(period, eeaCellCode) %>%
     reframe(
       speciesKeys = list(unique(speciesKey)),
       ott_ids = list(unique(ott_id)),
       unique_names = list(unique(unique_name)),
       orig_tiplabels = list(unique(orig_tiplabel))
     )
  }
  return(aggr_cube)

  }


