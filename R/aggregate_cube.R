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
#' @return A dataframe with for each grid cell a list of observed species
#' @importFrom rlang .data
#' @importFrom assertthat noNA
#' @import dplyr
#' @examples
#' ex_data <- retrieve_example_data()
#' mcube <- append_ott_id(ex_data$tree, ex_data$cube, ex_data$matched_nona)
#' mcube <- dplyr::filter(mcube, !is.na(ott_id))
#' aggr_cube <- aggregate_cube(mcube)
#' @export

aggregate_cube <- function(mcube, timegroup = NULL) {

  # Check that mcube is a dataframe or tibble
  stopifnot("Error: 'mcube' must be a dataframe or tibble." =
              inherits(mcube, "data.frame"))

  # Check that mcube contains the required columns
  required_columns <- c("year", "eeacellcode", "specieskey", "ott_id",
                        "unique_name", "orig_tiplabel")
  missing_columns <- setdiff(required_columns, colnames(mcube))
  if (length(missing_columns) > 0) {
    stop(paste("Error: 'mcube' is missing the following required columns:",
               paste(missing_columns, collapse = ", ")))
  }

  # Check that the 'year' column is numeric and does not contain NA
  if (!is.numeric(mcube$year) || any(is.na(mcube$year))) {
    stop("Error: The 'year' column in 'mcube' must be numeric and free of NA
         values.")
  }

  # Check that the 'eeacellcode' column exists and does not contain NA
  stopifnot(
  "Error: The 'eeacellcode' column in 'mcube' must not contain NA values." =
    assertthat::noNA(mcube$eeacellcode))

  # Check that timegroup is either NULL or a positive integer
  if (!is.null(timegroup)) {
    if (!is.numeric(timegroup) || timegroup <= 0 || length(timegroup) != 1 ||
        timegroup != as.integer(timegroup)) {
      stop("Error: 'timegroup' must be a single positive integer or NULL.")
    }
  }

  # Function logic starts here
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
  return(aggr_cube)}

