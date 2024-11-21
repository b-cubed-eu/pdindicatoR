#' Convert multisurface object to multipolygon object
#'
#' @param object An object of class multisurface
#' @return An object of class multipolygon
#' @importFrom dplyr group_by reframe arrange rename mutate join_by left_join distinct
#' @importFrom magrittr %>%
#' @examples
#' library(dplyr)
#' ex_data <- retrieve_example_data()
#' mcube <- append_ott_id(ex_data$tree, ex_data$cube, ex_data$matched_nona)
#' mcube <- dplyr::filter(mcube, !is.na(ott_id))
#' PD_cube <- get_pd_cube(mcube, ex_data$tree)
#' PD_cube_geo <- right_join(ex_data$grid, PD_cube,
#'                           by = join_by(CELLCODE == eeacellcode))
#' cutoff <- 150
#' PD_cube_geo$PD_high <- as.factor(ifelse((PD_cube_geo$PD > cutoff), 1, 0))
#' cube_highPD <- PD_cube_geo[PD_cube_geo$PD_high == 1,
#'     c("OBJECTID", "CELLCODE", "PD", "geometry", "PD_high")]
#' cube_mp <- convert_multipolygons(cube_highPD)
#' @export


convert_multipolygons <- function(object) {
  tmp1 <- tempfile(fileext = ".gpkg")
  tmp2 <- tempfile(fileext = ".gpkg")
  sf::st_write(object, tmp1)
  gdalUtilities::ogr2ogr(tmp1, tmp2, f = "GPKG", nlt = "MULTIPOLYGON")
  Y <- sf::st_read(tmp2)
  sf::st_sf(sf::st_drop_geometry(object), geom = sf::st_geometry(Y))
}
