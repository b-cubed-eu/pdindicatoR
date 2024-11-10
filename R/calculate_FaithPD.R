#' Calculation of Faith's PD
#'
#' This function calculates Faith's PD, based on a provided list of species
#' and a phylogenetic tree.
#'
#' @param tree An object of class 'phylo', iow a phylogenetic tree in Newick
#' format that was parsed by ape::read_tree()
#' @param species A character vector where each element is a species, and more
#' specifically, matches a tip label of the phylogenetic tree exactly
#' @return A string that combines "Phylogenetic diversity:" and the calculated
#' value
#' @examples calculate_faithpd(tree, c("F_alessandri", "C_kawakamii", MRCA)
#' @export

calculate_faithpd <- function(tree, species, MRCA){

  # get tip id's from tip labels

   tip_ids <- vector(mode="integer", length=length(species))
   for (i in seq_along(species)) {
       x <- which(tree$tip.label == species[i])
       tip_ids[i] <- x
   }

  # determine spanning paths (nodes) from species to MRCA

  nodepath <- vector(mode="list", length(tip_ids))
  for (i in seq_along(tip_ids)){
    x <- ape::nodepath(tree, MRCA, tip_ids[i])
    nodepath[[i]] <- x
  }

  # get the branches/edges along the spanning paths

  edge_ids <- vector(mode="list", length(tip_ids))
  for (i in seq_along(tip_ids)){
    edges <- which(tree$edge[, 1] %in% nodepath[[i]][-length(nodepath[[i]])] &
                     tree$edge[, 2] %in% nodepath[[i]][-1])
    edge_ids[[i]] <- edges
  }

  # Count shared branches only once

  edge_ids_unique <- unique(unlist(edge_ids))

  # Sum the length of the branches

  edge_lengths <- tree$edge.length[edge_ids_unique]
  pd <- sum(edge_lengths)
  # print(paste("Phylogenetic diversity:", pd))
}

# This is an alternative method using the existing ape::which.edge function,
# seems to do the same thing but should be tested for different (rooted and
# unrooted) trees to make sure methodology is sound

# calculate_pd_easy <- function(tree, species){
#  y<-which.edge(rtree, trio)
#  edge_lengths <- tree$edge.length[y]
#  pd <- sum(edge_lengths)
#  return(pd)
#  print(paste("Phylogenetic diversity:", pd))
# }



