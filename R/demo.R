
# Required packages: tidyverse, rotl, sf, gdalUtilities, ape, rnaturalearth
library(tidyverse)
library(rgbif)

# Load config file
source("config_demo.R")

# Load functions
source("./taxonmatch.R")
source("./pdmap.R")
source("./pdindicator.R")
source("./convert_multipolygons.R")
source("./get_pd_from_distance_matrix.R")

#-------------------------------------------------------
EXAMPLE 1: Mammals - Denmark
#-------------------------------------------------------

# Load datacube
cube <- read.csv(cube_path_Mammals_DE, stringsAsFactors = FALSE)
cube

# Aggregate over grid cell to create new dataframe with species list per grid cell id

cube
simpl_cube <- cube[,c("eea_cell_code","speciesKey")]
simpl_cube$eea_cell_code <- factor(simpl_cube$eea_cell_code)
str(cube)
aggr_cube <- simpl_cube %>% group_by(eea_cell_code) %>% summarize(speciesKeys = list(speciesKey))
aggr_cube[[1,2]]

# Extract speciesKeys, find corresponding name
speciesKeys <- unique(cube$speciesKey)


# Retrieve species information using the speciesKey
get_canonical_name <- function(speciesKey) {
  species_info <- name_usage(key = speciesKey)
  return(species_info$data$canonicalName)
}

canonicalnames <- sapply(speciesKeys, get_canonical_name)
canonicalnames

# Use TNRS to match names and get ott_ids
taxon_search_results <- rotl::tnrs_match_names(names = canonicalnames)

# Clean up search results (flagged obs)
taxon_search_cleaned <- taxon_search_results[!is.na(taxon_search_results$ott_id) &
                                               (taxon_search_results$is_synonym!=TRUE) &
                                               !grepl("(?i)incertae_sedis(?i)", taxon_search_results$flags) &
                                               !grepl("(?i)hidden(?i)", taxon_search_results$flags) &
                                               !grepl("(?i)unplaced(?i)", taxon_search_results$flags),]

# Get induced subtree
my_tree <- rotl::tol_induced_subtree(ott_ids = c(taxon_search_cleaned$ott_id), label_format = "name")
#plot(my_tree)

# Get patristic matrices
my_mammals_dr <- datelife::get_datelife_result(my_tree)

# We compute chronograms with branch lengths.

# my_mammals_dlr <- datelife::summarize_datelife_result(my_mammals_dr, summary_format = "phylo_all")

# We compute the median patristic matrix.
dist_matrix_mammals <- datelife::datelife_result_median_matrix(my_mammals_dr)

# And finally compute PD.

pd_mammals <- get_pd_from_distance_matrix(dist_matrix_mammals)

# PD HAS TO BE CALCULATED FOR EACH GRID CELL



# Append OTT id's to occurrence cube
speciesKeys <- cube["speciesKey"] %>% distinct()

mtable <- speciesKeys %>% left_join(matched[,c("ott_id","gbif_id")],
                                    by = join_by(speciesKey == gbif_id))

mcube <- cube %>% left_join(mtable[,c("speciesKey", "ott_id")],
                            by = join_by(speciesKey == speciesKey))


# or user-input for highest taxon and generation/lookup for tree and cube
# eg tree_induced <- rotl::tol_induced_subtree(ott_id(taxa), label="name")


## To do: generate datacube through GBIF  SQL API for species in user uploaded tree

#####################################################
## Run Biodiverse and feed tree and datacube to it.##
## Output metrics are stored as a cube ##############
#####################################################

# Output PD metrics are appended to the cube as new attribute 'PD'
PDcube <- mcube %>% select(year, eeaCellCode) %>% distinct(year, eeaCellCode,
                                                           .keep_all = TRUE) %>% mutate(PD = (rnorm(388, mean=5, sd=1.5)))

################################################
## Visualize PD on a map & calculate indicator##
################################################
# To do: build grid without loading external .shp file
# Merge grid cell geometry to cube

EEA_filepath <- "./shpfiles/EEA-reference-GRID-2013.gpkg"
sf::st_layers(EEA_filepath)
be_EEA <- sf::st_read(EEA_filepath, layer = "be_1km_polygon")
sf::st_crs(be_EEA)
PDcube_geo <- right_join(be_EEA, PDcube, by = join_by(CELLCODE == eeaCellCode ))

# Create map

## To do: get protected area polygons through google earth api

## Read in protected area polygons
pa_filepath <- "./shpfiles/20240528_protected_areas_BE.gpkg"
sf::st_layers(pa_filepath)
pa_natura <- sf::st_read(pa_filepath, layer = "NaturaSite_polygon")

# Plot PD map (use https://epsg.io/ for determining coordinates of desired
# geographic area)
map_PD <- pdmap(PDcube_geo, taxon, xmin, xmax, ymin, ymax)
print(map_PD)

# Calculate PD indicator
indicator <- pdindicator(PDcube_geo, cutoff)
print(paste(round(indicator), " % of high PD grid cells is currently located within a protected area."))
