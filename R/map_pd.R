#' Mapping PD
#'
#' This function creates, for a geographic area defined by the user, a map with
#' the calculated PD metric for each grid cell and the location of protected
#' nature areas.
#'
#' @param PD_cube An sf dataframe containing the calculated PD metrics (column name
#' 'PD') for each grid cell with occurrences of a selected higher taxon, and the
#' geometries of those grid cells.
#' @param grid An sf object with variable detailing grid cell codes and a
#' geometry column
#' @param taxon A selected higher taxon, for which the occurrence cube was
#' generated. Used to generate the map's title only.
#' @param bbox_custom Optional, numeric vector with custom bounding box
#' coordinates as c(xmin, xmax, ymin, ymax)
#' @return
#' @example map_PD <- PD_map(PD_cube, grid, "Musceloidea", 3885477, 3929441,
#' 3103857, 3126672)
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

map_pd <- function(PD_cube, grid, taxon = NULL, bbox_custom = NULL, cutoff = NULL) {

  # Merge grid with cube

  PD_cube_geo <- right_join(grid, PD_cube, by = join_by(CELLCODE == eeaCellCode))

  # Set bounding box

  if (is.null(bbox_custom)) {
    bbox <- st_bbox(PD_cube_geo)
  } else {
    # Expect bbox_custom to be a numeric vector of length 4:
    # c(xmin, xmax, ymin, ymax)
    if (length(bbox_custom) != 4) {
      stop("bbox_custom must be a numeric vector of length 4:
           c(xmin, xmax, ymin, ymax).")
    }
    bbox <- c(xmin = bbox_custom[1], xmax = bbox_custom[2],
              ymin = bbox_custom[3], ymax = bbox_custom[4])
  }

  expansion_factor <- 0.20  # Example: expand by 10% of bounding box range
  bbox_expanded <- c(
  xmin = as.numeric(bbox["xmin"]) - (as.numeric(bbox["xmax"]) -
    as.numeric(bbox["xmin"])) * expansion_factor,
  xmax = as.numeric(bbox["xmax"]) + (as.numeric(bbox["xmax"]) -
    as.numeric(bbox["xmin"])) * expansion_factor,
  ymin = as.numeric(bbox["ymin"]) - (as.numeric(bbox["ymax"]) -
    as.numeric(bbox["ymin"])) * expansion_factor,
  ymax = as.numeric(bbox["ymax"]) + (as.numeric(bbox["ymax"]) -
    as.numeric(bbox["ymin"])) * expansion_factor
)

  # Read in country borders

  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  world_3035 <- sf::st_transform(world, crs= 3035)

  # Plot map

  map <- (ggplot() +
  geom_sf(data = world_3035, fill="antiquewhite") +
  geom_sf(data = PD_cube_geo, mapping = aes(fill = PD)) +
  scale_fill_viridis_c(option="B") +
  geom_sf(data = pa, fill = NA, color = "darkgreen", linewidth = 0.1) +
  coord_sf(xlim = c(bbox_expanded["xmin"], bbox_expanded["xmax"]),
           ylim = c(bbox_expanded["ymin"], bbox_expanded["ymax"]), expand = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("PD indicator", subtitle = paste(taxon)) +
  theme(panel.grid.major = element_line(color = gray(0.5), linewidth = 0.5),
        panel.background = element_rect(fill = "aliceblue")))
  print(map)
  return(map)
}
