
# Required packages: tidyverse, rotl, sf, gdalUtilities, ape, rnaturalearth
library(tidyverse)
library(purrr)

# Load config file
source("config.R")

# Load functions
source("./taxonmatch.R")
source("./append_ott_id.R")
source("./pdmap.R")
source("./pdindicator.R")
source("./convert_multipolygons.R")

# Load tree
tree <- ape::read.tree(tree_path)
plot(tree, cex=0.45)

# Load datacube
# To do: generate matching datacube for user-uploaded tree through GBIF SQL API
cube <- read.csv(cube_path, stringsAsFactors = FALSE, sep="\t")
head(cube)

cube_clean <- cube %>% filter(!is.na(specieskey))

# Match leaf labels of tree with GBIF id's and append OTT_id's to cube
mcube <- append_ott_id(tree, cube_clean)

# or user-input for highest taxon and generation/lookup for tree and cube
# eg tree_induced <- rotl::tol_induced_subtree(ott_id(taxa), label="name")


#####################################################
## Calculate PD per grid cell and append to cube ####
## Output metrics are stored as a cube ##############
#####################################################

# Aggregate over grid cell to create new dataframe with species list per grid
# cell id
head(mcube)
simpl_cube <- mcube[,c("eeaCellCode","speciesKey","ott_id", "unique_name")]
simpl_cube$eeaCellCode <- factor(simpl_cube$eeaCellCode)
aggr_cube <- simpl_cube %>% group_by(eeaCellCode) %>%
  summarize(speciesKeys = list(speciesKey), ott_ids = list(ott_id), names = list(unique_name)) %>%
  mutate(unique_spkeys = lapply(speciesKeys, unique)) %>%
  mutate(unique_ott_ids = lapply(ott_ids, unique)) %>%
  mutate(unique_names = lapply(names, unique))
head(aggr_cube)

PD_cube <- aggr_cube %>% mutate(PD = map(unique_names, calculate_pd, tree=tree))


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
