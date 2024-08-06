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

taxonmatch <- function(tree) {
tree_labels <- tree$tip.label

# Hack to work with ToF
labels_split <- str_split(tree_labels, " ")
sci_names <- sapply(labels_split, function(x) paste(tail(x, 2), collapse = " "))

if (any(stringr::str_detect(sci_names,'ott\\d+')) == FALSE){
  
  taxa <- rotl::tnrs_match_names(sci_names)
} else {
  taxa <- data.frame(sci_names)
  colnames(taxa)[1] <- "ott_id"
}


taxa[,"gbif_id"] <- NA
i=1
for(id in taxa$ott_id){
  if (!is.na(id)) {
    tax_info <- rotl::taxonomy_taxon_info(id)
    for(source in tax_info[[1]]$tax_sources){
      if (grepl('gbif', source, fixed=TRUE)){
        gbif <- stringr::str_split(source,":")[[1]][2]
        taxa[i,]$gbif_id <- gbif
      }} 
  }
  else {
    taxa[i,]$gbif_id <- NA
  }
  i = i + 1}
taxa$gbif_id <- as.integer(taxa$gbif_id)
return(taxa)
}


