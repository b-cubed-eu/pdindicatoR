#' Taxon matching
#'
#' This function matches the tip labels of a phylogenetic tree (Taxon names or
#' OTT id's) with corresponding GBIF id's.
#' @param tree An object of class 'phylo', iow a phylogenetic tree in Newick
#' format that was parsed by ape::read_tree()
#' @returns A dataframe with columns ott_id and gbif_id
#' @import dplyr
#' @examples
#' \dontrun{ex_data <- retrieve_example_data()
#' # This can take a while!
#' mtable <- taxonmatch(ex_data$tree)}
#' @export
#'


taxonmatch <- function(tree) {
  tree_labels <- tree$tip.label

  if (any(stringr::str_detect(tree_labels, "ott\\d+")) == FALSE) {
    taxa <- rotl::tnrs_match_names(tree_labels)
  } else {
    taxa <- data.frame(tree_labels)
    colnames(taxa)[1] <- "ott_id"
  }


  taxa[, "gbif_id"] <- NA
  i <- 1
  for (id in taxa$ott_id) {
    if (is.na(id) == FALSE) {
      tax_info <- rotl::taxonomy_taxon_info(id)
      for (source in tax_info[[1]]$tax_sources) {
        if (grepl("gbif", source, fixed = TRUE)) {
          gbif <- stringr::str_split(source, ":")[[1]][2]
          taxa[i, ]$gbif_id <- gbif
        }
      }
    }
    i <- i + 1
  }
  taxa$gbif_id <- as.integer(taxa$gbif_id)

  original_df <- data.frame(
    orig_tiplabel = unique(tree_labels),
    search_string = tolower(unique(tree_labels)))

  matched_result <- merge(taxa, original_df, by = "search_string", all.x = TRUE)
  return(matched_result)
}
