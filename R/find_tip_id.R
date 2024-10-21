#' Find corresponding tip id's of tip labels
#'
#' This function finds the tip id's (numbers) that correspond to the tip labels
#' (species names, ott id's,..) of a phylogenetic tree.
#'
#' @param species A vector of species names
#' @param my_tree A phylogenetic tree object, read in by ape::read_tree()
#' @return a vector with tip id's
#' @example tip_ids <- find_tip_ids(species, rtree)

find_node_id <- function(species,my_tree){
  vector <- c()
  for (i in species){
    x <- which(my_tree$tip.label==i)
    vector <- c(vector, x)}
  return(vector)
}
