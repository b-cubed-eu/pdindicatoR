
#' Find MRCA for data cube and call function to calculate PD metrics
#'
#' This function determines the MRCA of all species in the datacube
#' and calls the function(s) to calculate PD metrics
#'
#' @param mcube An occurrence data cube with matched names appended,
#' product of function taxonmatch()
#' @param tree A phylogenetic tree with branch lengths
#' @param timegroup Optional, an integer which represents the number of years
#' over which occurrences need to be aggregated and the PD value calculated
#' @param metric Name of the PD metric to be calculated
#' @return Calculated PD value
#' @import dplyr
#' @examples
#' library(dplyr)
#' ex_data <- retrieve_example_data()
#' mcube <- append_ott_id(ex_data$tree, ex_data$cube, ex_data$matched_nona)
#' mcube <- dplyr::filter(mcube, !is.na(ott_id))
#' PD_cube <- get_pd_cube(mcube, ex_data$tree, metric="faith")
#' @export

get_pd_cube <- function(mcube, tree, timegroup = NULL, metric = "faith") {

  # Aggregate cube
  aggr_cube <- aggregate_cube(mcube, timegroup)

  # Get all species in matched cube
  all_matched_sp <- unique(mcube[["orig_tiplabel"]])

  # Find most recent common ancestor
  MRCA <- ape::getMRCA(tree, all_matched_sp)

  # Calculate PD metric
  if (metric == "faith") {
    PD_cube <- aggr_cube %>%
      mutate(PD = unlist(purrr::map(aggr_cube$orig_tiplabels,
                                    ~ calculate_faithpd(tree, unlist(.x), MRCA))
                         )
             )
  }
}
