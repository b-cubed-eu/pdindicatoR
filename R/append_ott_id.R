#' Append ott id's to cube
#'
#' This function uses the table produced by the taxonmatch() function to create
#' a linking table, and then append the ott_id's as a new field to the occurence
#' cube.
#' @param tree An object of class 'phylo', iow a phylogenetic tree in Newick
#' format that was parsed by ape::read_tree()
#' @param cube A dataframe with for selected taxa, the number of occurrences per
#' taxa and per grid cell
#' @param matched A dataframe, returned by running the function taxonmatch() on
#' a phylogenetic tree, which contains the tip labels of the tree and their
#' corresponding gbif_id's
#' @return A dataframe which consist of all the data in the original datacube,
#' appended with column ott_id
#' @example
#' @export

append_ott_id <- function(tree, cube, matched){

    # Append OTT id's to occurrence cube
  speciesKeys <- cube["speciesKey"] %>% distinct()

  mtable <- speciesKeys %>% left_join(matched[,c("ott_id","gbif_id", "unique_name")],
                                      by = join_by(speciesKey == gbif_id))

  mcube <- cube %>% left_join(mtable[,c("speciesKey", "ott_id", "unique_name")],
                              by = join_by(speciesKey == speciesKey))
  return(mcube)
  }
