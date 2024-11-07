#' Mapping PD and calculating indicator
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
#' @param cutoff A variable of type numeric which determines the cut-off point
#' between low PD and high PD
#' @return
#' @example map_PD <- PD_map(PD_cube, grid, "Musceloidea", 3885477, 3929441,
#' 3103857, 3126672)
#' print(map_PD[[1]])
#'
#'

generate_map_and_indicator <- function(PD_cube, grid, taxon = NULL, bbox_custom = NULL, cutoff = NULL) {

# Merge grid with cube
PD_cube_geo <- right_join(grid, PD_cube, by = join_by(CELLCODE == eeacellcode))

# Set bounding box
if (is.null(bbox_custom)) {
  bbox <- st_bbox(PD_cube_geo)
} else {
  if (length(bbox_custom) != 4) {
    stop("bbox_custom must be a numeric vector of length 4: c(xmin, xmax, ymin, ymax).")
  }
  bbox <- c(xmin = bbox_custom[1], xmax = bbox_custom[2],
            ymin = bbox_custom[3], ymax = bbox_custom[4])
}

# Expand bounding box
expansion_factor <- 0.20
bbox_expanded <- c(
  xmin = as.numeric(bbox["xmin"]) - (as.numeric(bbox["xmax"]) - as.numeric(bbox["xmin"])) * expansion_factor,
  xmax = as.numeric(bbox["xmax"]) + (as.numeric(bbox["xmax"]) - as.numeric(bbox["xmin"])) * expansion_factor,
  ymin = as.numeric(bbox["ymin"]) - (as.numeric(bbox["ymax"]) - as.numeric(bbox["ymin"])) * expansion_factor,
  ymax = as.numeric(bbox["ymax"]) + (as.numeric(bbox["ymax"]) - as.numeric(bbox["ymin"])) * expansion_factor
)

# Read in country borders
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world_3035 <- sf::st_transform(world, crs = 3035)

# Initialize lists to store maps and indicators
plots <- list()
indicators <- list()

# Calculate global min and max PD values for consistent color scale
pd_min <- min(PD_cube_geo$PD, na.rm = TRUE)
pd_max <- max(PD_cube_geo$PD, na.rm = TRUE)

# Check for 'period' column in PD_cube
if ("period" %in% colnames(PD_cube_geo)) {
  unique_periods <- unique(PD_cube_geo$period)

  for (p in unique_periods) {
    # Subset data for the current period
    current_period_data <- PD_cube_geo %>% filter(period == p)

    # Create the map for the current period
    map <- ggplot() +
      geom_sf(data = world_3035, fill = "antiquewhite") +
      geom_sf(data = current_period_data, mapping = aes(fill = PD)) +
      scale_fill_viridis_c(option = "B", limits = c(pd_min, pd_max)) +
      geom_sf(data = pa, fill = NA, color = "darkgreen", linewidth = 0.05) +
      coord_sf(xlim = c(bbox_expanded["xmin"], bbox_expanded["xmax"]),
               ylim = c(bbox_expanded["ymin"], bbox_expanded["ymax"]), expand = FALSE) +
      xlab("Longitude") + ylab("Latitude") +
      labs(title = paste("Taxon:", taxon),
           subtitle = paste("Phylogenetic Diversity for period:", p)) +
      theme(panel.grid.major = element_line(color = gray(0.5), linewidth = 0.5),
            panel.background = element_rect(fill = "aliceblue"))

    # Store the plot in the list
    plots[[as.character(p)]] <- map


    # Calculate indicator for the current period if cutoff is provided
    if (!is.null(cutoff)) {
      current_period_data$PD_high <- ifelse((current_period_data$PD > cutoff), 1, 0)
      cube_highPD <- current_period_data[current_period_data$PD_high == 1, c("OBJECTID", "CELLCODE", "PD", "geom", "PD_high")]

      # Convert to multipolygon object
      cube_mp <- convert_multipolygons(cube_highPD)

      # Determine centerpoints of high PD grid cells
      centroids <- sf::st_centroid(cube_mp)

      # Calculate % of high PD grid cell centroids that intersect with protected areas
      intersecting <- sf::st_intersects(pa, centroids)
      n_intersecting <- sum(lengths(intersecting))
      n_total <- nrow(centroids)
      PD_indicator <- (n_intersecting / n_total) * 100
      indicators[[as.character(p)]] <- PD_indicator

      print(paste("The percentage of high PD grid cells within protected areas for period", p, "is", PD_indicator, "%"))
    }
  }

 } else {
  # If 'period' column is not present, create a single map and calculate the indicator for all data
  map <- ggplot() +
    geom_sf(data = world_3035, fill = "antiquewhite") +
    geom_sf(data = PD_cube_geo, mapping = aes(fill = PD)) +
    scale_fill_viridis_c(option = "B") +
    geom_sf(data = pa, fill = NA, color = "darkgreen", linewidth = 0.05) +
    coord_sf(xlim = c(bbox_expanded["xmin"], bbox_expanded["xmax"]),
             ylim = c(bbox_expanded["ymin"], bbox_expanded["ymax"]), expand = FALSE) +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle(paste("Taxon:", taxon, "\n Phylogenetic Diversity")) +
    theme(panel.grid.major = element_line(color = gray(0.5), linewidth = 0.5),
          panel.background = element_rect(fill = "aliceblue"))

  plots <- map

  # Calculate indicator if cutoff is provided
  if (!is.null(cutoff)) {
    PD_cube_geo$PD_high <- ifelse((PD_cube_geo$PD > cutoff), 1, 0)
    cube_highPD <- PD_cube_geo[PD_cube_geo$PD_high == 1, c("OBJECTID", "CELLCODE", "PD", "geom", "PD_high")]

    # Convert to multipolygon object
    cube_mp <- convert_multipolygons(cube_highPD)

    # Determine centerpoints of high PD grid cells
    centroids <- sf::st_centroid(cube_mp)

    # Calculate % of high PD grid cell centroids that intersect with protected areas
    intersecting <- sf::st_intersects(pa, centroids)
    n_intersecting <- sum(lengths(intersecting))
    n_total <- nrow(centroids)
    PD_indicator <- (n_intersecting / n_total) * 100
    indicators[["Overall"]] <- PD_indicator

    print(paste("The percentage of high PD grid cells that fall within protected areas is", PD_indicator, "%"))
  }

 }

# Return the list of maps and indicators
if (!is.null(cutoff)) {
  return(list(plots, indicators))  # Return the combined map and indicators for each period or overall
} else {
  return(plots)  # Return only the combined map if no cutoff
}
 }
