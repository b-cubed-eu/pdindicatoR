#' Calculate PD indicator
#'
#' This function calculates the PD indicator, which is the percentage of high PD
#' cells that fall within the boundaries of a protected area. The centroid of
#' the grid cell is used to determine the intersection. The cut-off for low/high
#' PD is determined by the user (function parameter).
#'
#' @param cube An sf dataframe containing the calculated PD metrics (column name
#' 'PD') for each grid cell with occurrences of a selected higher taxon, and the
#' geometries of those grid cells.
#' @param cutoff A variable of type numeric which determines the cut-off point
#' between low PD and high PD
#' @return
#' @example indicator <- pdindicator(cube_musceloidea, 6)

pdindicator <- function(cube,cutoff) {
  # Subset dataset for only high PD cells
  cube$PD_high <- ifelse((cube$PD > cutoff), 1 ,0)
  cube_highPD <- cube[cube$PD_high==1,]
  # Convert multisurface object to multipolygon object
  cube_mp <- convert_multipolygons(cube_highPD)
  # Determine centerpoints of high PD grid cells
  centroids <- sf::st_centroid(cube_mp)
  # Calculate % of high PD grid cell centroids that fall within (intersect)
  # with the PA polygons
  intersecting <- sf::st_intersects(pa_natura, centroids)
  n_intersecting<- sum(lengths(intersecting))
  n_total <- nrow(centroids)
  PD_indicator <- (n_intersecting / n_total) * 100
  return(PD_indicator)
  # to do: uncertainty parameter
}


