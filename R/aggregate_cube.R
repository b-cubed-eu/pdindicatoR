#' Aggregate datacube over grid cell to create new dataframe with species list
#' per grid
#'
#' This function aggregates a provided datacube over grid cell id, so that a new
#' datacube is outputted with 3 variables that contain the lists of species that
#' are observed for each grid cell (as `speciesKeys`, `ott_id`'s and names).
#'
#' @param mcube An occurrence datacube with appended `ott_id`'s, as produced by
#' the `append_ott_id()` function
#' @param timegroup An integer, representing the number of years by which you
#' want to group
#' your occurrence data
#' @return A dataframe with for each grid cell
#' @importFrom rlang .data
#' @import dplyr
#' @examples
#' ex_data <- retrieve_example_data()
#' mcube <- append_ott_id(ex_data$tree, ex_data$cube, ex_data$matched_nona)
#' mcube <- dplyr::filter(mcube, !is.na(ott_id))
#' aggr_cube <- aggregate_cube(mcube)
#' @export

aggregate_cube <- function(mcube, timegroup = NULL) {

  columns_to_select <- c("year", "eeacellcode", "specieskey", "ott_id",
                         "unique_name", "orig_tiplabel")
  simpl_cube <- mcube[, intersect(columns_to_select, colnames(mcube))]
  min_year <- min(simpl_cube$year)

  # When occurrences are already aggregated over time or when no timegroup is
  # specified
  if (!("year" %in% colnames(simpl_cube)) ||
      is.null(timegroup) ||
      missing(timegroup)
      ) {
    aggr_cube <- simpl_cube %>%
      group_by(.data$eeacellcode) %>%
      reframe(
        specieskeys = list(unique(.data$specieskey)),
        ott_ids = list(unique(.data$ott_id)),
        unique_names = list(unique(.data$unique_name)),
        orig_tiplabels = list(unique(.data$orig_tiplabel))
      )

  # When timegroup ==1
  } else if (timegroup == 1) {
      aggr_cube <- simpl_cube %>%
        arrange(.data$year) %>%
      group_by(.data$eeacellcode, .data$year) %>%
      reframe(
        specieskeys = list(unique(.data$specieskey)),
        ott_ids = list(unique(.data$ott_id)),
        unique_names = list(unique(.data$unique_name)),
        orig_tiplabels = list(unique(.data$orig_tiplabel))
      ) %>%
      rename(period = .data$year)
  } else {

  # Calculate the 5-year period for each row
   period <- NULL
   aggr_cube <- simpl_cube %>%
     arrange(.data$year) %>%
    mutate(period = min_year + 5 * ((.data$year - min_year) %/% 5)) %>%
     mutate(period = paste(period, period + 4, sep = "-")) %>%
    group_by(.data$period, .data$eeacellcode) %>%
     reframe(
       specieskeys = list(unique(.data$specieskey)),
       ott_ids = list(unique(.data$ott_id)),
       unique_names = list(unique(.data$unique_name)),
       orig_tiplabels = list(unique(.data$orig_tiplabel))
     )
  }
  return(aggr_cube)

  }
