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
#' @examples append_ott_id(tree, cube, matched)
#' @export

append_ott_id <- function(tree, cube, matched){

    # Append OTT id's to occurrence cube
  speciesKeys <- cube["specieskey"] %>% distinct()

  mtable <- speciesKeys %>% left_join(matched[,c("ott_id","gbif_id", "unique_name", "orig_tiplabel")],
                                      by = join_by(specieskey == gbif_id))

  mcube <- cube %>% left_join(mtable[,c("specieskey", "ott_id", "unique_name", "orig_tiplabel")],
                              by = join_by(specieskey == specieskey))
  return(mcube)
  }
