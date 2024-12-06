#' Check if provided phylogenetic tree is complete and covers all species in
#' occurrence cube
#'
#' This function calculates which number of species in the provided occurrence
#' cube, is not a tip label of the provided phylogenetic tree.
#' @param mcube A dataframe which is returned by the function append_ott_id(),
#' and contains the occurrence datacube with `ott_id` variable appended.
#' format that was parsed by `ape::read_tree()`
#' @return a list - first element is the total number of species in the
#' occurrence cube, second element is the number of species lacking in the
#' phylogenetic tree.
#' @import dplyr
#' @examples
#' ex_data <- retrieve_example_data()
#' mcube<- append_ott_id(ex_data$tree, ex_data$cube, ex_data$matched_nona)
#' check_completeness(mcube)
#' @export

check_completeness <- function(mcube) {

  # Check that mcube is a dataframe or tibble
  if (!is.data.frame(mcube)) {
    stop("Error: 'mcube' must be a dataframe or tibble.")
  }

  # Check that mcube contains the required columns
  required_columns <- c("specieskey", "ott_id", "species")
  missing_columns <- setdiff(required_columns, colnames(mcube))
  if (length(missing_columns) > 0) {
    stop(paste("Error: 'mcube' is missing the following required columns:",
               paste(missing_columns, collapse = ", ")))
  }

  # Function logic starts here

  mcube_dist <- distinct(mcube, .data$specieskey, .keep_all = TRUE)
  sp_na <- mcube_dist %>%
    dplyr::filter(is.na(.data$ott_id)) %>%
    dplyr::select(.data$specieskey, .data$species)

  cat("The following species are not part of the provided phylogenetic tree:\n")
  print(sp_na)
}
