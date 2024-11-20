#' Retrieve example data
#'
#' This function specifies the paths to the example data and reads the example
#' data files in and processes them so they are ready to be used in the
#' workflow.
#'
#' @return Objects tree (a phylogenetic tree of the order Fagales), cube (an
#' occurrence datacube, see query specifications:
#' https://www.gbif.org/occurrence/download/0004018-241107131044228), grid
#' (EEA 1km grid for study area) and pa (Natura2000 protected area polygons for
#'study area
#' @importFrom magrittr %>%
#' @importFrom utils read.csv
#' @import dplyr
#' @examples
#' ex_data <- retrieve_example_data()
#' print(ex_data$tree)
#' print(ex_data$cube)
#' print(ex_data$grid)
#' print(ex_data$pa)
#' print(ex_data$matched_nona)
#' @export




retrieve_example_data <- function(data = "all") {
  # Initialize paths for each data type
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

  # Define a list to store the loaded data
  result <- list()

  # Load data based on the specified 'data' argument
  if ("all" %in% data || "tree" %in% data) {
    tree <- ape::read.tree(tree_path)
    tree$tip.label <- gsub("_", " ", tree$tip.label)
    result$tree <- tree
  }

  if ("all" %in% data || "cube" %in% data) {
    cube <- utils::read.csv(cube_path, stringsAsFactors = FALSE, sep = "\t")
    result$cube <- cube
  }

  if ("all" %in% data || "grid" %in% data) {
    grid <- sf::st_read(grid_path)
    result$grid <- grid
  }

  if ("all" %in% data || "pa" %in% data) {
    pa <- sf::st_read(pa_path)
    result$pa <- pa
  }

  if ("all" %in% data || "matched_nona" %in% data) {
    matched_nona <- read.csv(matched_nona_path, stringsAsFactors = FALSE, sep = ",")
    result$matched_nona <- matched_nona
  }
  
# Return only the specified variables
return(result)
}
