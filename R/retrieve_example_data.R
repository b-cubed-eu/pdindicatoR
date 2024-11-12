#' Retrieve example data
#'
#' This function specifies the paths to the example data and reads the example
#' data files in and processes them so they are ready to be used in the workflow.
#'
#' @return Objects tree (a phylogenetic tree of the order Fagales), cube (an
#' occurrence datacube, see query specifications:
#' https://www.gbif.org/occurrence/download/0004018-241107131044228), grid
#' (EEA 1km grid for study area) and pa (Natura2000 protected area polygons for
#'study area
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by reframe arrange rename mutate join_by left_join distinct
#' @examples
#' ex_data <- retrieve_example_data()
#' print(ex_data$tree)
#' print(ex_data$cube)
#' print(ex_data$grid)
#' print(ex_data$pa)
#' print(ex_data$matched_nona)
#' @export




retrieve_example_data <- function() {
  tree_path <- system.file("extdata", "Fagales_species.nwk",  package = "pdindicatoR")
  if (tree_path == "") {
    stop("File not found. Please check that 'Fagales_species.nwk' is in inst/extdata/")
  }
  cube_path <-  system.file("extdata", "0004018-241107131044228_Fagales1km.csv", package = "pdindicatoR")
  if (cube_path == "") {
    stop("File not found. Please check that '0004018-241107131044228_Fagales1km.csv' is in inst/extdata/")
  }
  grid_path <- system.file("extdata", "EEA_1km_NPHogeKempen", "EEA_1km_HK.shp",  package = "pdindicatoR")
  if (grid_path == "") {
    stop("File not found. Please check that 'EEA_1km_HK.shp' is in inst/extdata/EEA_1km_NPHogeKempen")
  }
  pa_path <- system.file("extdata", "PA_NPHogeKempen", "protected_areas_NPHogeKempen.shp", package = "pdindicatoR")
  if (pa_path == "") {
    stop("File not found. Please check that 'PA_NPHogeKempen/protected_areas_NPHogeKempen.shp' is in inst/extdata/")
  }
  matched_nona_path <- system.file("extdata", "matched_nona.csv", package = "pdindicatoR")
  if (matched_nona_path == "") {
    stop("File not found. Please check that 'matched_nona.shp' is in inst/extdata/")
  }


  tree <- ape::read.tree(tree_path)
  tree$tip.label <- gsub("_", " ", tree$tip.label)
  cube <- utils::read.csv(cube_path, stringsAsFactors = FALSE, sep="\t")
  grid <- sf::st_read(grid_path)
  pa <- sf::st_read(pa_path)
  matched_nona <- read.csv(matched_nona_path, stringsAsFactors = FALSE, sep=",")
  return(list(tree = tree, cube = cube, grid = grid, pa = pa, matched_nona=matched_nona))
}
