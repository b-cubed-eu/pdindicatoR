#' Mapping PD
#'
#' This function creates, for a geographic area defined by the user, a map with
#' the calculated PD metric for each grid cell and the location of protected
#' nature areas.
#'
#' @param cube An sf dataframe containing the calculated PD metrics (column name
#' 'PD') for each grid cell with occurrences of a selected higher taxon, and the
#' geometries of those grid cells.
#' @param taxon A selected higher taxon, for which the PD was calculated
#' @param xmin minimum cartesian x-coordinate of map view
#' @param xmax maximum cartesian x-coordinate of map view
#' @param ymin minimum cartesian y-coordinate of map view
#' @param ymax maximum cartesian y-coordinate of map view
#' @return
#' @example map_PD <- PD_map(cube2, "Musceloidea", 3885477, 3929441, 3103857,
#' 3126672)
#' print(map_PD)
#' ggsave("PDmap.png", map_PD)
#'
#' Use https://epsg.io/ to determine coordinates
#' coordinates Belgium: xmin = 3760000, xmax= 4090000, ymin = 2910000,
#'  ymax = 3190000
#' coordinates Vlaams-Brabant: xmin = 3885477, xmax = 3973406, ymin = 3080962,
#'  ymax = 3126672
#'
#' to  do: conversion function between crs of cube and lat/lon or some kind of
#' interactive area selector
#'
#' @returns A map for the selected geographic area, which visualizes the PD
#' value per grid cell with a colour scale, and the boundaries of protected
#' nature areas.
#'

pdmap <- function(cube,taxon,xmin,xmax,ymin,ymax) {
  ## Read in country borders EU
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  world_3035 <- sf::st_transform(world, crs= 3035)
  map <- (ggplot() +
  geom_sf(data = world_3035, fill="antiquewhite") +
  geom_sf(data = cube, mapping = aes(fill = PD)) +
  scale_fill_viridis_c(option="B") +
  geom_sf(data = pa_natura, fill = NA, color = "darkgreen", linewidth = 0.1) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("PD indicator", subtitle = paste(taxon)) +
  theme(panel.grid.major = element_line(color = gray(0.5), linewidth = 0.5),
        panel.background = element_rect(fill = "aliceblue")))
  return(map)
}
