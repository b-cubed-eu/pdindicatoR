#' Mapping PD and calculating indicator
#'
#' This function creates, for a geographic area defined by the user, a map with
#' the calculated PD metric for each grid cell and the location of protected
#' nature areas.
#'
#' @param pd_cube A dataframe with a variable containing grid cell codes and
#' a variable with calculated pd values (output of the function get_pd_cube())
#' @param grid An sf object with variable detailing grid cell codes and a
#' geometry column
#' @param taxon A selected higher taxon, for which the occurrence cube was
#' generated. Used to generate the map's title only.
#' @param bbox_custom Optional, numeric vector with custom bounding box
#' coordinates as c(xmin, xmax, ymin, ymax)
#' @param cutoff A variable of type numeric which determines the cut-off point
#' between low PD and high PD
#' @return a list `PDindicator`, which contains one or more maps in it's first
#' element, and possibly one or more indicator values in it's second element
#' @import dplyr
#' @importFrom grDevices gray
#' @import ggplot2
#' @examples
#' library(dplyr)
#' ex_data <- retrieve_example_data()
#' mcube <- append_ott_id(ex_data$tree, ex_data$cube, ex_data$matched_nona)
#' mcube <- dplyr::filter(mcube, !is.na(ott_id))
#' pd_cube <- get_pd_cube(mcube, ex_data$tree)
#' PDindicator <- generate_map_and_indicator(
#'   pd_cube,
#'   ex_data$grid,
#'   taxon="Fagales",
#'   cutoff=150)
#' map <- PDindicator[[1]]
#' indicator <- PDindicator[[2]]
#' @export

generate_map_and_indicator <- function(
    pd_cube,
    grid,
    taxon = NULL,
    bbox_custom = NULL,
    cutoff = NULL) {

  # pd_cube checks
  stopifnot(
    "'pd_cube' must be a dataframe or tibble." = is.data.frame(pd_cube),
    "'pd_cube' must contain a column named 'pd' representing Phylogenetic Diversity." = assertthat::has_name(pd_cube, "pd")
  )

  # grid checks
  stopifnot(
    "'grid' must be provided." = !missing(grid),
    "'grid' must be an 'sf' object." = inherits(grid, "sf"),
    "Some geometries in 'grid' are invalid. Please fix them before proceeding." = all(sf::st_is_valid(grid)),
    "'grid' must contain a column named 'CELLCODE' for grid cell codes." = assertthat::has_name(grid, "CELLCODE"),
    "'grid' must contain a 'geometry' column." = assertthat::has_name(grid, "geometry")
  )

  # taxon check
  stopifnot(
    "'taxon' must be a character string or NULL." = is.null(taxon) || is.character(taxon)
  )

  # cutoff check
  if (!is.null(cutoff)) {
    stopifnot(
      "'cutoff' must be numeric." = is.numeric(cutoff),
      "'cutoff' must be a single value." = length(cutoff) == 1,
      "'cutoff' must be greater than zero." = cutoff > 0
    )
  }

  # Function logic starts here
  ex_data <- retrieve_example_data(data = "pa")
  pa <- ex_data$pa

  # Merge grid with cube
  pd_cube_geo <- right_join(grid, pd_cube,
                            by = join_by("CELLCODE" == "eeacellcode"))

  # Set bounding box
  if (is.null(bbox_custom)) {
    bbox <- sf::st_bbox(pd_cube_geo)
  } else {
    if (length(bbox_custom) != 4) {
      stop(paste("bbox_custom must be a numeric vector of length 4:",
                 "c(xmin, xmax, ymin, ymax)."))
    }
    bbox <- c(xmin = bbox_custom[1], xmax = bbox_custom[2],
              ymin = bbox_custom[3], ymax = bbox_custom[4])
  }

  # Expand bounding box
  expansion_factor <- 0.20
  bbox_expanded <- c(
    xmin = as.numeric(bbox["xmin"]) -
      (as.numeric(bbox["xmax"]) - as.numeric(bbox["xmin"])) * expansion_factor,
    xmax = as.numeric(bbox["xmax"]) +
      (as.numeric(bbox["xmax"]) - as.numeric(bbox["xmin"])) * expansion_factor,
    ymin = as.numeric(bbox["ymin"]) -
      (as.numeric(bbox["ymax"]) - as.numeric(bbox["ymin"])) * expansion_factor,
    ymax = as.numeric(bbox["ymax"]) +
      (as.numeric(bbox["ymax"]) - as.numeric(bbox["ymin"])) * expansion_factor
  )

  # Read in country borders
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  world_3035 <- sf::st_transform(world, crs = 3035)

  # Initialize lists to store maps and indicators
  plots <- list()
  indicators <- list()

  # Calculate global min and max PD values for consistent color scale
  pd_min <- min(pd_cube_geo$pd, na.rm = TRUE)
  pd_max <- max(pd_cube_geo$pd, na.rm = TRUE)

  # Check for 'period' column in pd_cube
  if ("period" %in% colnames(pd_cube_geo)) {
    unique_periods <- unique(pd_cube_geo$period)

    for (p in unique_periods) {
      # Subset data for the current period
      current_period_data <- pd_cube_geo %>% filter(.data$period == p)

      # Create the map for the current period
      map <- ggplot2::ggplot() +
        ggplot2::geom_sf(data = world_3035, fill = "antiquewhite") +
        ggplot2::geom_sf(data = current_period_data,
                         mapping = ggplot2::aes(fill = .data$pd)) +
        ggplot2::scale_fill_viridis_c(option = "B",
                                      limits = c(pd_min, pd_max)) +
        ggplot2::geom_sf(data = pa, fill = NA, color = "darkgreen",
                         linewidth = 0.05) +
        ggplot2::coord_sf(xlim = c(bbox_expanded["xmin"],
                                   bbox_expanded["xmax"]),
                 ylim = c(bbox_expanded["ymin"], bbox_expanded["ymax"]),
                 expand = FALSE) +
        ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude") +
        ggplot2::labs(title = paste("Taxon:", taxon),
             subtitle = paste("Phylogenetic Diversity for period:", p)) +
        ggplot2::theme(
          panel.grid.major = ggplot2::element_line(color = gray(0.5),
                                                   linewidth = 0.5),
          panel.background = ggplot2::element_rect(fill = "aliceblue"))

      # Store the plot in the list
      plots[[as.character(p)]] <- map

      # Calculate indicator for the current period if cutoff is provided
      if (!is.null(cutoff)) {
        current_period_data$pd_high <- as.numeric(
          current_period_data$pd > cutoff)
        cube_highpd <- current_period_data[
          current_period_data$pd_high == 1,
          c("OBJECTID", "CELLCODE", "pd", "geom", "pd_high")
          ]

        # Convert to multipolygon object
        cube_mp <- convert_multipolygons(cube_highpd)

        # Determine centerpoints of high PD grid cells
        centroids <- sf::st_centroid(cube_mp)

        # Calculate % of high PD grid cell centroids that intersect with
        # protected areas
        intersecting <- sf::st_intersects(pa, centroids)
        n_intersecting <- sum(lengths(intersecting))
        n_total <- nrow(centroids)
        pd_indicator <- (n_intersecting / n_total) * 100
        indicators[[as.character(p)]] <- pd_indicator

        print(paste("The percentage of high PD grid cells within protected",
                    "areas for period", p, "is", pd_indicator, "%"))
      }
    }
  } else {
    # If 'period' column is not present, create a single map and calculate the
    # indicator for all data
    map_pd <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = world_3035, fill = "antiquewhite") +
      ggplot2::geom_sf(data = pd_cube_geo,
                       mapping = ggplot2::aes(fill = .data$pd)) +
      ggplot2::scale_fill_viridis_c(option = "B") +
      ggplot2::geom_sf(data = pa, fill = NA, color = "lightblue",
                       linewidth = 0.03) +
      ggplot2::coord_sf(xlim = c(bbox_expanded["xmin"], bbox_expanded["xmax"]),
               ylim = c(bbox_expanded["ymin"], bbox_expanded["ymax"]),
               expand = FALSE) +
      ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude") +
      ggplot2::ggtitle(paste("Taxon:", taxon, "\n Phylogenetic Diversity")) +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(color = grDevices::gray(0.5),
                                                 linewidth = 0.5),
        panel.background = ggplot2::element_rect(fill = "aliceblue"))

    # Calculate indicator if cutoff is provided and produce a high/low PD map
    if (!is.null(cutoff)) {
      pd_cube_geo$pd_high <- as.factor(ifelse((pd_cube_geo$pd > cutoff), 1, 0))
      cube_highpd <- pd_cube_geo[
        pd_cube_geo$pd_high == 1,
        c("OBJECTID", "CELLCODE", "pd", "geometry", "pd_high")
        ]

      # Convert to multipolygon object
      cube_mp <- convert_multipolygons(cube_highpd)

      # Determine centerpoints of high PD grid cells
      centroids <- sf::st_centroid(cube_mp)

      # Calculate % of high PD grid cell centroids that intersect with
      # protected areas
      intersecting <- sf::st_intersects(pa, centroids)
      n_intersecting <- sum(lengths(intersecting))
      n_total <- nrow(centroids)
      pd_indicator <- (n_intersecting / n_total) * 100
      indicators[["Overall"]] <- pd_indicator

      print(paste("The percentage of high PD grid cells that fall within",
                  "protected areas is", round(indicators$Overall, digits = 2),
                  "%"))

      map_hilo_pd <- ggplot2::ggplot() +
        ggplot2::geom_sf(data = world_3035, fill = "antiquewhite") +
        ggplot2::geom_sf(data = pd_cube_geo,
                         mapping = ggplot2::aes(fill = .data$pd_high)) +
        #scale_fill_viridis_c(option = "B") +
        ggplot2::geom_sf(data = pa, fill = NA, color = "lightblue",
                         linewidth = 0.03) +
        ggplot2::coord_sf(xlim = c(bbox_expanded["xmin"],
                                   bbox_expanded["xmax"]),
                 ylim = c(bbox_expanded["ymin"], bbox_expanded["ymax"]),
                 expand = FALSE) +
        ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude") +
        ggplot2::ggtitle(paste("Taxon:", taxon, "\n Phylogenetic Diversity")) +
        ggplot2::theme(
          panel.grid.major = ggplot2::element_line(color = gray(0.5),
                                                   linewidth = 0.5),
          panel.background = ggplot2::element_rect(fill = "aliceblue"))

      plots <- list(map_pd, map_hilo_pd)
    } else if (is.null(cutoff)) {
      plots <- map_pd
    }

    # Return the list of maps and indicators
    if (!is.null(cutoff)) {
      # Return the combined map and indicators for each period or overall
      return(list(plots, indicators))
    } else {
      return(plots)  # Return only the combined map if no cutoff
    }
  }
}
