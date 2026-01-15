#' Append ott id's to cube
#'
#' This function uses the table produced by the `taxonmatch()` function to
#' create a linking table, and then append the `ott_id`'s as a new field to the
#' occurrence cube.
#' @param tree An object of class `'phylo'`, a phylogenetic tree in Newick or
#' Nexus format that was parsed by `ape::read_tree()`
#' @param cube A dataframe with for selected taxa, the number of occurrences per
#' taxa and per grid cell
#' @param matched A dataframe, returned by running the function `taxonmatch()`
#' on a phylogenetic tree, which contains the tip labels of the tree and their
#' corresponding `gbif_id`'s
#' @return A dataframe which consist of all the data in the original datacube,
#' appended with columns `ott_id`, `unique_name` and `orig_tiplabel`
#' @import dplyr
#' @examples
#' ex_data <- retrieve_example_data()
#' mcube <- append_ott_id(ex_data$tree, ex_data$cube, ex_data$matched_nona)
#' mcube <- dplyr::filter(mcube, !is.na(ott_id))
#' aggr_cube <- aggregate_cube(mcube)
#' @export

append_ott_id <- function(tree, cube, matched) {

  # Check if inputs are of required class

  if (!inherits(tree, "phylo")) {
    stop("Error: 'tree' must be an object of type 'Phylo'")
  }

  if (!is.data.frame(cube)) {
    stop("Error: 'cube' must be a dataframe")
  }

  if (!is.data.frame(matched)) {
    stop("Error: 'matched' must be a dataframe")
  }

  # Check that cube contains the required columns
  required_columns <- c("specieskey")
  missing_columns <- setdiff(required_columns, colnames(cube))
  if (length(missing_columns) > 0) {
    stop(paste("Error: 'cube' is missing the following required columns:",
               paste(missing_columns, collapse = ", ")))
  }

  # Check that matched contains the required columns
  required_columns <- c("usageKey", "verbatim_name")
  missing_columns <- setdiff(required_columns, colnames(matched))
  if (length(missing_columns) > 0) {
    stop(paste("Error: 'matched' is missing the following required columns:",
               paste(missing_columns, collapse = ", ")))
  }

  # Function logic starts here
  # Append OTT id's to occurrence cube
  species_keys <- cube %>% distinct(.data$specieskey)

## TEMPORSRILY ONLY KEEPS FIRST MATCH, RESOLVE PROPERLY WHAT HAPPENS IF MULTIPLE SYNONYMS
##  ARE IN THE TREE AND THUS MULTIPLE MATCHES FOR ONE speciesKey (always accepted taxon in cube)
  matched_unique <- matched %>%
    distinct(acceptedUsageKey, .keep_all = TRUE)

  mtable <- species_keys %>%
    left_join(matched_unique[, c("acceptedUsageKey", "verbatim_name")],
              by = join_by("specieskey" == "acceptedUsageKey"))

  mcube <- cube %>%
    left_join(
      mtable[, c("specieskey", "verbatim_name")],
      by = join_by("specieskey" == "specieskey")
    )
  return(mcube)
}

