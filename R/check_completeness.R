#' Check if provided phylogenetic tree is complete and covers all species in
#' occurence cube
#'
#' This function calculates which number of species in the provided occurence
#' cube, is not a tip label of the provided phylogenetic tree.
#' @param mcube A dataframe which is returned by the function append_ott_id(),
#' and contains the occurence datacube with ott_id variable appended.
#' format that was parsed by ape::read_tree()
#' @return a list - first element is the total number of species in the
#' occurence cube, second element is the number of species lacking in the
#' phylogenetic tree.
#' @example
#' @export

check_completeness <- function(mcube){

  mcube_dist <- distinct(mcube, speciesKey, .keep_all=TRUE)
  sp_na <- mcube_dist %>% dplyr::filter(is.na(ott_id)) %>% select(speciesKey)
  sp_miss <- paste(sp_na,collapse=",")
  missing <- print(paste("The following species are not part of the provided
  phylogenetic tree:", sp_miss))
}
