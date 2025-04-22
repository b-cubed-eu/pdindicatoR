#' Taxon matching using rgbif
#'
#' This function matches the tip labels of a phylogenetic tree (Taxon names)
#' with corresponding GBIF id's through the rgbif package
#' @param phyloTree An object of class `'phylo'`, a phylogenetic tree in Newick
#' format that was parsed by `ape::read_tree()`
#' @param nmbCores an integer indicating the amount of CPU cores to be used in the
#' computation. Serialized computation is performed when the core is 1 which is
#' chosen as the standard setting
#' @returns A dataframe that retrieves all relevant taxonomic information from the
#' GBIF backbone. Matchtype is included as well and can be utilized in verification
#' of the quality of matching
#' @import dplyr
#' @import parallel
#' @import rgbif
#' @examples
#' \dontrun{ex_data <- retrieve_example_data()
#' # Parallelized computation with 4 cores
#' mtable <- taxonmatch(ex_data$tree, nmbCores=4)}
#' @export
#'

taxonmatchGBIF <- function(phyloTree, nmbCores=1){
  #Verify if nmbCores is a valid option
  if(nmbCores>detectCores()-1){
    cat("The number of requested cores exceeds the number of available cores. The maximum number of cores is ",detectCores()-1)
    return(invisible(NULL))
  }
  if(nmbCores>1){
    #Generate indices to split the list so that we can process the fragments
    chunkIndices <- cut(seq_along(phyloTree$tip.label), breaks = nmbCores, labels = FALSE)
    #Split up the data based on the indices
    splitList <- split(phyloTree$tip.label, chunkIndices)
    #Depending on the OS different parallelisation is employed
    if (Sys.info()['sysname']=="Windows"){
      #setup computation cluster using the specified number of cores
      computeCluster<-makeCluster(nmbCores, type = "PSOCK")
      #Apply the name_backbone_checklist to each sublist
      tipMatches <- parLapply(computeCluster,
                              splitList,
                              name_backbone_checklist)
      #Combine the separate sublists into a dataframe
      tipMatches<-do.call(bind_rows, tipMatches)
      return(tipMatches)
    } else {
      #setup computation cluster using the specified number of cores
      computeCluster<-makeCluster(nmbCores, type = "FORK")
      #Apply the name_backbone_checklist to each sublist
      tipMatches <- parLapply(computeCluster,
                              splitList,
                              name_backbone_checklist)
      #Combine the separate sublists into a dataframe
      tipMatches<-do.call(bind_rows, tipMatches)
      return(tipMatches)
    }
  } else {
    tipMatches <- name_backbone_checklist(phyloTree$tip.label)
    return(tipMatches)
  }
}
