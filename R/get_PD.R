
#' Find MRCA for data cube and call function to calculate PD metrics
#'
#' This function determines the MRCA of all species in the datacube
#' and calls the function(s) to calculate PD metrics
#'
#' @param tree A phylogenetic tree with branch lengths
#' @param species A character vector with species names
#' @return Calculated PD value
#' @example get_pd(tree, species)
#'

get_pd <- function(tree, species){

# get all species in matched cube

all_matched_sp<-unique(mcube[["orig_tiplabel"]])

# find most recent common ancestor
MRCA <- ape::getMRCA(tree, all_matched_sp)

# calculate PD metric
calculate_faithpd(tree, species, MRCA)
}
