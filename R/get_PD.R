
#' Find MRCA for data cube and call function to calculate PD metrics
#'
#' This function determines the MRCA of all species in the datacube
#' and calls the function(s) to calculate PD metrics
#'
#' @param tree A phylogenetic tree with branch lengths
#' @param species A character vector with species names
#' @param metric Name of the PD metric to be calculated
#' @return Calculated PD value
#' @examples
#' ex_data <- retrieve_example_data()
#' mcube <- append_ott_id(ex_data$tree, ex_data$cube, ex_data$matched_nona)
#' mcube <- mcube %>% dplyr::filter(!is.na(ott_id))
#' aggr_cube <- aggregate_cube(mcube)
#' PD_cube <- aggr_cube %>% mutate(PD = unlist(purrr::map(orig_tiplabels, ~ get_pd(ex_data$tree, unlist(.x)))))
#' @export

get_pd <- function(tree, species, metric="faith"){

# get all species in matched cube

all_matched_sp<-unique(mcube[["orig_tiplabel"]])

# find most recent common ancestor
MRCA <- ape::getMRCA(tree, all_matched_sp)

# calculate PD metric
if (metric=="faith"){
calculate_faithpd(tree, species, MRCA)
}
}

