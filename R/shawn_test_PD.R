library(b3gbi)
library(rotl)
library(datelife)
library(datelifeplot)
library(taxize)
library(dplyr)

#-------------------------------------------------------------------------------

get_distance_matrices_from_cube <- function(cube, context) {

  gbif_tax_hier <- taxize::classification(unique(cube$data$scientificName), db = "gbif", rows = 1)
  gbif_spec_names <- unlist(lapply(gbif_tax_hier, function(x) x[7,1]))
  gbif_taxon_keys <- unlist(lapply(gbif_tax_hier, function(x) x[7,3]))
  taxon_search <- rotl::tnrs_match_names(names = gbif_spec_names, context_name = context)
  taxon_search_cleaned <- taxon_search[!is.na(taxon_search$ott_id) &
                                         (taxon_search$is_synonym!=TRUE) &
                                         !grepl("(?i)incertae_sedis(?i)", taxon_search$flags) &
                                         !grepl("(?i)hidden(?i)", taxon_search$flags) &
                                         !grepl("(?i)unplaced(?i)", taxon_search$flags),]

  my_tree <- rotl::tol_induced_subtree(ott_ids = c(taxon_search_cleaned$ott_id), label_format = "name")
  my_dm <- datelife::get_datelife_result(my_tree)

}

#-------------------------------------------------------------------------------

get_distance_matrices_from_species_names <- function(spec_names, context) {

  taxon_search <- rotl::tnrs_match_names(names = spec_names, context_name = context)
  my_tree <- rotl::tol_induced_subtree(ott_ids = c(taxon_search$ott_id), label_format = "name")
  my_dr <- datelife::get_datelife_result(my_tree)

}

#-------------------------------------------------------------------------------

get_pd_from_distance_matrix <- function(dist_matrix) {

  # Remove duplicates and self-connections
  dist_matrix <- as.data.frame(dist_matrix)
  dist_matrix[upper.tri(dist_matrix, diag = TRUE)] <- NA
  dist_matrix$species1 <- rownames(dist_matrix)
  dist_matrix_long <-
    dist_matrix %>%
    as.data.frame() %>%
    tidyr::pivot_longer(!species1,
                        names_to = "species2",
                        values_to = "distance")
  dist_matrix_cleaned <- dist_matrix_long[!is.na(dist_matrix_long$distance),]

  # Sort by size and take the LARGEST as our first distance
  dm_sorted <- dist_matrix_cleaned[order(dist_matrix_cleaned$distance, decreasing = TRUE),]
  values <- dm_sorted[1,3]

  # Add both species to our current species list
  species <- unlist(c(dm_sorted[1,1], dm_sorted[1,2]))
  names(species) <- NULL

  # Remove distance from the matrix
  dm_sorted <- dm_sorted[-1,]

  # Repeating steps (while matrix not empty):
  while(nrow(dm_sorted) >= 1) {

    # 1. Select distances involving the species on our list
    dm_selected <- dm_sorted %>%
      filter(species1 %in% species | species2 %in% species)

    # 2. Sort by size and find the SMALLEST
    smallest <- unlist(dm_selected[nrow(dm_selected), 3])

    # 3. Divide by two and add it cumulatively to our running distance value
    values <- unlist(c(values + (smallest / 2)))

    # 4. Add the connected species to our current species list
    if (!dm_selected[nrow(dm_selected), 1] %in% species) {
      species <- unlist(c(species, dm_selected[nrow(dm_selected), 1]))
    }
    if (!dm_selected[nrow(dm_selected), 2] %in% species) {
      species <- unlist(c(species, dm_selected[nrow(dm_selected), 2]))
    }
    names(species) <- NULL

    # 5. Remove distance from the matrix
    dm_sorted <- dm_sorted %>%
      filter(!(species1==dm_selected$species1[nrow(dm_selected)]
               & species2==dm_selected$species2[nrow(dm_selected)]
               & distance==dm_selected$distance[nrow(dm_selected)]))

    # 6. Remove all distances for the new species that are greater than or equal to the one we just removed
    new_species <- species[length(species)]
    dm_sorted <- dm_sorted %>%
      filter(!((species1==new_species
                | species2==new_species)
               & distance >= smallest))
  }
  # Take cumulative distance value as PD
  names(values) <- NULL
  PD <- values
  print(PD)

}


#-------------------------------------------------------------------------------

# Remove disconnected chronograms
remove_disconnected_chronograms <- function(dist_matrices, datelife_result) {

  # check for disconnected chronograms
  tree_connect <- list()
  for (i in 1:length(datelife_result)) {
    species_list <- datelife_result[[i]][[3]]
    match_total <- vector()
    for (j in 1:length(datelife_result)){
      match_all <- all(datelife_result[[j]][[3]] %in% species_list)
      match_yesno <- any(datelife_result[[j]][[3]] %in% species_list)
      if (match_all) {match_yesno <- FALSE}
      match_total[j] <- match_yesno
    }

    if (any(match_total)) {tree_connect[[i]] <- TRUE}
    else {tree_connect[[i]] <- FALSE}
  }

  # Remove disconnected trees
  connected <- list()
  counter <- 1
  for (i in 1:length(dist_matrices)) {
    if(tree_connect[[i]]) {
      connected[[counter]] <- dist_matrices[[i]]
      counter <- counter + 1
    }
  }

  return(connected)

}
