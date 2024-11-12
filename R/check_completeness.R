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
#' @importFrom dplyr group_by reframe arrange rename mutate distinct
#' @importFrom magrittr %>%
#' @examples
#' ex_data <- retrieve_example_data()
#' mcube<- append_ott_id(ex_data$tree, ex_data$cube, ex_data$matched_nona)
#' check_completeness(mcube)
#' @export

check_completeness <- function(mcube){

  mcube_dist <- distinct(mcube, .data$specieskey, .keep_all=TRUE)
  sp_na <- mcube_dist %>% dplyr::filter(is.na(.data$ott_id)) %>% dplyr::select(.data$specieskey,
                                                                  .data$species)
  # sp_miss <- paste(sp_na,collapse=",")
  cat("The following species are not part of the provided phylogenetic tree:\n")
  print(sp_na)
}
