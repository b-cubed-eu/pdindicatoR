#' Convert multisurface object to multipolygon object
#'
#' @param object An object of class multisurface
#' @return An object of class multipolygon
#' @import dplyr
#' @examples
#' library(dplyr)
#' ex_data <- retrieve_example_data()
#' mcube <- append_ott_id(ex_data$tree, ex_data$cube, ex_data$matched_nona)
#' mcube <- dplyr::filter(mcube, !is.na(ott_id))
#' pd_cube <- get_pd_cube(mcube, ex_data$tree)
#' pd_cube_geo <- right_join(ex_data$grid, pd_cube,
#'                           by = join_by(CELLCODE == eeacellcode))
#' cutoff <- 150
#' pd_cube_geo$pd_high <- as.factor(ifelse((pd_cube_geo$pd > cutoff), 1, 0))
#' cube_highpd <- pd_cube_geo[pd_cube_geo$pd_high == 1,
#'     c("OBJECTID", "CELLCODE", "pd", "geometry", "pd_high")]
#' cube_mp <- convert_multipolygons(cube_highpd)
#' @export


convert_multipolygons <- function(object) {
  tmp1 <- tempfile(fileext = ".gpkg")
  tmp2 <- tempfile(fileext = ".gpkg")
  sf::st_write(object, tmp1)
  gdalUtilities::ogr2ogr(tmp1, tmp2, f = "GPKG", nlt = "MULTIPOLYGON")
  y <- sf::st_read(tmp2)
  sf::st_sf(sf::st_drop_geometry(object), geom = sf::st_geometry(y))
}
