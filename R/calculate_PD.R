# install.packages("ape")         # for phylogenetic tree calculation and manipulation
# install.packages("phytools")    # for advanced phylogenetic analysis and plotting
# install.packages("phangorn")    # for handling phylogenetic data and diverse analysis

library(ape)
# library(phytools)
# library(phangorn)

calculate_pd <- function(tree, species){
  # get node id for leaf labels
  # x <- vector("list", length(species))
  # j = 1
  # for (i in species){
  # x[[j]]-> which(tree$tip.label == i)
  # j = j+1
  # }

  # get MRCA for observed species

  MRCA <- getMRCA (tree, species)

  # determine spanning paths (nodes) from species to MRCA

  nodepath <- vector("list", length(species))
  j=1
  for (i in species){
    x <- ape::nodepath(tree, MRCA, i)
    nodepath[[j]] <- x
    j = j+1
  }

  # get the branches/edges along the spanning paths

  edge_indices <- vector("list", length(species))
  k=1
  for (i in seq_along(species)){
    edges <- which(tree$edge[, 1] %in% nodepath[[i]][-length(nodepath[[i]])] &
                     tree$edge[, 2] %in% nodepath[[i]][-1])
    edge_indices[[i]] <- edges
  }

  edge_indices_vector <- unique(unlist(edge_indices))

  # sum the length of the branches

  edge_lengths <- tree$edge.length[edge_indices_vector]
  pd <- sum(edge_lengths)
  return(pd)
  print(paste("Phylogenetic diversity:", pd))
}

# This is an alternative method using the existing ape::which.edge function,
# seems to do the same thing but should be tested for different (rooted and
# unrooted) trees to make sure methodology is sound

calculate_pd_easy <- function(tree, species){
  y<-which.edge(rtree, trio)
  edge_lengths <- tree$edge.length[y]
  pd <- sum(edge_lengths)
  return(pd)
  print(paste("Phylogenetic diversity:", pd))
}


# -----example---------

# # Create random tree
#
# # rtree <- rtree(10, rooted=TRUE)
# # str(rtree) # a tree is a list with elements edge, tip.label, Nnode and edge.length
# rtree$tip.label
# tree$node.label
# tree$edge
#
# plot(rtree)
# nodelabels()
# edgelabels()
# tiplabels()
#
# # Create vector with selected leaves/species for which to calculate the PD
# trio <- c(9, 7, 3)
#
# # Calculate PD for trio of observed species
# calculate_pd(rtree, trio)

