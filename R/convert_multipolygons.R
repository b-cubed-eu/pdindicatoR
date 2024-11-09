#' Convert multisurface object to multipolygon object
#'
#' @param object An object of class multisurface
#' @return An object of class multipolygon
#' @example convert_multipolygons(cube_highPD)
#' @export


convert_multipolygons <- function(object) {
  tmp1 <- tempfile(fileext = ".gpkg")
  tmp2 <- tempfile(fileext = ".gpkg")
  sf::st_write(X, tmp1)
  gdalUtilities::ogr2ogr(tmp1, tmp2, f = "GPKG", nlt = "MULTIPOLYGON")
  Y <- sf::st_read(tmp2)
  sf::st_sf(sf::st_drop_geometry(X), geom = sf::st_geometry(Y))
}
