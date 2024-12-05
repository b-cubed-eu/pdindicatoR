#' Calculation of Faith's PD
#'
#' This function calculates Faith's PD, based on a provided list of species
#' and a phylogenetic tree.
#'
#' @param tree An object of class `'phylo'`, a phylogenetic tree in Newick
#' format that was parsed by `ape::read_tree()`
#' @param species A character vector where each element is a species, and more
#' specifically, matches a tip label of the phylogenetic tree exactly
#' @param mrca_node_id Node id of the taxon that represents the most recent common
#' ancestor of the set of species under study
#' @return A string that combines "Phylogenetic diversity:" and the calculated
#' value
#' @import dplyr
#' @examples
#' ex_data <- retrieve_example_data()
#' # determine the most recent common ancestor of all species under study
#' # (not necessarily all species in the tree!)
#' mrca_node_id <- ape::getMRCA(ex_data$tree, ex_data$tree$tip.label)
#' species <- c("Fagus lucida", "Castanopsis fabri", "Quercus_robur")
#' calculate_faithpd(ex_data$tree, species, mrca_node_id)
#' @export

calculate_faithpd <- function(tree, species, mrca_node_id) {

  # get tip id's from tip labels
  tip_ids <- vector(mode = "integer", length = length(species))
  for (i in seq_along(species)) {
    x <- which(tree$tip.label == species[i])
    if (length(x) > 0) {
     tip_ids[i] <- x
    } else {
     # Optionally, print a warning or assign a default value for missing matches
     warning(paste("Species", species[i], "not found in tree$tip.label"))
     tip_ids[i] <- NA  # Assign NA if the species is not found
    }
  }
   tip_ids <- tip_ids[!is.na(tip_ids)]

  # determine spanning paths (nodes) from species to mrca_node_id

  nodepath <- vector(mode = "list", length(tip_ids))
  for (i in seq_along(tip_ids)) {
    x <- ape::nodepath(tree, mrca_node_id, tip_ids[i])
    nodepath[[i]] <- x
  }

  # get the branches/edges along the spanning paths

  edge_ids <- vector(mode = "list", length(tip_ids))
  for (i in seq_along(tip_ids)) {
    edges <- which(tree$edge[, 1] %in% nodepath[[i]][-length(nodepath[[i]])] &
                     tree$edge[, 2] %in% nodepath[[i]][-1])
    edge_ids[[i]] <- edges
  }

  # Count shared branches only once

  edge_ids_unique <- unique(unlist(edge_ids))

  # Sum the length of the branches

  edge_lengths <- tree$edge.length[edge_ids_unique]
  pd <- sum(edge_lengths)
  return(pd)
}


