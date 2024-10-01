#' Taxon matching
#'
#' This function matches the tip labels of a phylogenetic tree (Taxon names or
#' OTT id's) with corresponding GBIF id's.
#' @param tree An object of class 'phylo', iow a phylogenetic tree in Newick
#' format that was parsed by ape::read_tree()
#' @result A dataframe with columns ott_id and gbif_id
#' @example matched <- taxonmatch(tree_musceloidea)
#' head(matched)
#'
#'
# TO DO:
# - After name matching, some quality measurements should be outputted eg.
# number of matches with a score under a certain threshold, number of species
# with >1 match (what does this mean in practice?), number of NA's for
# gbif id match
#
#
taxonmatch <- function(tree) {
tree_labels <- tree$tip.label

if (any(stringr::str_detect(tree_labels,'ott\\d+')) == FALSE){
  taxa <- rotl::tnrs_match_names(tree_labels)
} else {
  taxa <- data.frame(tree_labels)
  colnames(taxa)[1] <- "ott_id"
}


taxa[,"gbif_id"] <- NA
i=1
for(id in taxa$ott_id){
  tax_info <- rotl::taxonomy_taxon_info(id)
  for(source in tax_info[[1]]$tax_sources){
    if (grepl('gbif', source, fixed=TRUE)){
      gbif <- stringr::str_split(source,":")[[1]][2]
      taxa[i,]$gbif_id <- gbif
    }}
  i = i + 1}
taxa$gbif_id <- as.integer(taxa$gbif_id)
return(taxa)
}



