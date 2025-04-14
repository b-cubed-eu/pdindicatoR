#' Retrieve example data
#'
#' This function specifies the paths to the example data and reads the example
#' data files in and processes them so they are ready to be used in the
#' workflow.
#' @param data a list with the names of the datasets to
#' be retrieved. Can be one or multiple of the following: "all", "tree", "cube",
#' "grid", "pa"
#' @return Objects tree (a phylogenetic tree of the order Fagales), cube (an
#' occurrence datacube, see query specifications:
#' https://www.gbif.org/occurrence/download/0004018-241107131044228), grid
#' (EEA 1km grid for study area) and pa (Natura2000 protected area polygons for
#'study area
#' @importFrom utils read.csv
#' @import dplyr
#' @importFrom stats setNames
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
  paths <- list(
  tree = system.file("extdata", "Fagales_species.nwk",
                           package = "pdindicatoR"),
  cube =  system.file("extdata", "0004018-241107131044228_Fagales1km.csv",
                            package = "pdindicatoR"),
  grid = system.file("extdata", "EEA_1km_NPHogeKempen", "EEA_1km_HK.shp",
                           package = "pdindicatoR"),
  pa = system.file("extdata", "PA_NPHogeKempen",
                         "protected_areas_NPHogeKempen.shp",
                         package = "pdindicatoR"),
  matched_nona = system.file("extdata", "matched_nona.csv",
                                   package = "pdindicatoR")
  )

  # Verify all files exist
  missing_files <- sapply(paths, function(path) path == "")
  if (any(missing_files)) {
    missing_names <- names(missing_files[missing_files])
    stop("File(s) not found: ", paste(missing_names, collapse = ", "))
  }

  # Define loader functions for each data type
  loaders <- list(
    tree = function(path) {
      tree <- ape::read.tree(path)
      tree$tip.label <- gsub("_", " ", tree$tip.label)
      tree
    },
    cube = function(path) {
      utils::read.csv(path, stringsAsFactors = FALSE, sep = "\t")
    },
    grid = sf::st_read,
    pa = sf::st_read,
    matched_nona = function(path) {
      read.csv(path, stringsAsFactors = FALSE, sep = ",")
    }
  )

  # Initialize result list
  result <- list()

  for (type in names(paths)) {
    if ("all" %in% data || type %in% data) {
      result[[type]] <- loaders[[type]](paths[[type]])
    }
  }


  # Return only the specified variables
  return(result)
}
