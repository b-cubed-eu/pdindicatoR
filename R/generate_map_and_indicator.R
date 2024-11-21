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
#' @return a list PDindicator, which contains one or more maps in it's first element.
#' and possibly one or more indicator values in it's second element
#' @importFrom dplyr group_by reframe arrange rename mutate left_join right_join join_by filter
#' @importFrom magrittr %>%
#' @importFrom grDevices gray
#' @import ggplot2
#' @examples
#' library(dplyr)
#' ex_data <- retrieve_example_data()
#' mcube <- append_ott_id(ex_data$tree, ex_data$cube, ex_data$matched_nona)
#' mcube <- dplyr::filter(mcube, !is.na(ott_id))
#' PD_cube <- get_pd_cube(mcube, ex_data$tree)
#' PDindicator <- generate_map_and_indicator(
#'   PD_cube,
#'   ex_data$grid,
#'   taxon="Fagales",
#'   cutoff=150)
#' map <- PDindicator[[1]]
#  indicator <- PDindicator[[2]]
#' @export

generate_map_and_indicator <- function(PD_cube, grid, taxon = NULL, bbox_custom = NULL, cutoff = NULL) {

ex_data <- retrieve_example_data(data="pa")
pa <- ex_data$pa

# Merge grid with cube
PD_cube_geo <- right_join(grid, PD_cube,
                          by = join_by("CELLCODE" == "eeacellcode"))

# Set bounding box
if (is.null(bbox_custom)) {
  bbox <- sf::st_bbox(PD_cube_geo)
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
    current_period_data <- PD_cube_geo %>% filter(.data$period == p)

    # Create the map for the current period
    map <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = world_3035, fill = "antiquewhite") +
      ggplot2::geom_sf(data = current_period_data, mapping = ggplot2::aes(fill = .data$PD)) +
      ggplot2::scale_fill_viridis_c(option = "B", limits = c(pd_min, pd_max)) +
      ggplot2::geom_sf(data = pa, fill = NA, color = "darkgreen", linewidth = 0.05) +
      ggplot2::coord_sf(xlim = c(bbox_expanded["xmin"], bbox_expanded["xmax"]),
               ylim = c(bbox_expanded["ymin"], bbox_expanded["ymax"]),
               expand = FALSE) +
      ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude") +
      ggplot2::labs(title = paste("Taxon:", taxon),
           subtitle = paste("Phylogenetic Diversity for period:", p)) +
      ggplot2::theme(panel.grid.major = ggplot2::element_line(color = gray(0.5), linewidth = 0.5),
            panel.background = ggplot2::element_rect(fill = "aliceblue"))

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
  map_PD <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = world_3035, fill = "antiquewhite") +
    ggplot2::geom_sf(data = PD_cube_geo, mapping = ggplot2::aes(fill = .data$PD)) +
    ggplot2::scale_fill_viridis_c(option = "B") +
    ggplot2::geom_sf(data = pa, fill = NA, color = "lightblue", linewidth = 0.03) +
    ggplot2::coord_sf(xlim = c(bbox_expanded["xmin"], bbox_expanded["xmax"]),
             ylim = c(bbox_expanded["ymin"], bbox_expanded["ymax"]), expand = FALSE) +
    ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude") +
    ggplot2::ggtitle(paste("Taxon:", taxon, "\n Phylogenetic Diversity")) +
    ggplot2::theme(panel.grid.major = ggplot2::element_line(color = grDevices::gray(0.5), linewidth = 0.5),
          panel.background = ggplot2::element_rect(fill = "aliceblue"))

  # Calculate indicator if cutoff is provided and produce a high/low PD map
  if (!is.null(cutoff)) {
    PD_cube_geo$PD_high <- as.factor(ifelse((PD_cube_geo$PD > cutoff), 1, 0))
    cube_highPD <- PD_cube_geo[PD_cube_geo$PD_high == 1, c("OBJECTID", "CELLCODE", "PD", "geometry", "PD_high")]

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


    map_hiloPD <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = world_3035, fill = "antiquewhite") +
      ggplot2::geom_sf(data = PD_cube_geo, mapping = ggplot2::aes(fill = .data$PD_high)) +
      #scale_fill_viridis_c(option = "B") +
      ggplot2::geom_sf(data = pa, fill = NA, color = "lightblue", linewidth = 0.03) +
      ggplot2::coord_sf(xlim = c(bbox_expanded["xmin"], bbox_expanded["xmax"]),
               ylim = c(bbox_expanded["ymin"], bbox_expanded["ymax"]), expand = FALSE) +
      ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude") +
      ggplot2::ggtitle(paste("Taxon:", taxon, "\n Phylogenetic Diversity")) +
      ggplot2::theme(panel.grid.major = ggplot2::element_line(color = gray(0.5), linewidth = 0.5),
            panel.background = ggplot2::element_rect(fill = "aliceblue"))

    plots <- list(map_PD, map_hiloPD)
 }
  else if (is.null(cutoff)){plots <- map_PD}

# Return the list of maps and indicators
if (!is.null(cutoff)) {
  return(list(plots, indicators))  # Return the combined map and indicators for each period or overall
} else {
  return(plots)  # Return only the combined map if no cutoff
}}}
