#' Append ott id's to cube
#'
#' This function retrieves the gbif id's for the species in a supplied tree
#' (using helper function taxonmatch.R), and then uses the produced linking
#' table to append the ott_id's as a new variable for the species in the
#' occurence cube.
#' @param tree An object of class 'phylo', iow a phylogenetic tree in Newick
#' format that was parsed by ape::read_tree()
#' @param cube A dataframe with for selected taxa, the number of occurrences per
#' taxa and per grid cell
#' @result A dataframe which consist of all the data in the original datacube,
#' appended with column ott_id
#' @example
#'
#'
#'
#'
append_ott_id <- function(tree, cube){
  # Match tree labels with gbif id's
  matched <- taxonmatch(tree)

  # Append OTT id's to occurrence cube
  speciesKeys <- cube["speciesKey"] %>% distinct()

  mtable <- speciesKeys %>% left_join(matched[,c("ott_id","gbif_id", "unique_name")],
                                      by = join_by(speciesKey == gbif_id))

  mcube <- cube %>% left_join(mtable[,c("speciesKey", "ott_id", "unique_name")],
                              by = join_by(speciesKey == speciesKey))
  return(mcube)
  }
