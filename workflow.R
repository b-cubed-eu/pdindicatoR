
# Required packages: tidyverse, rotl, sf, gdalUtilities, ape, rnaturalearth
library(tidyverse)

# Load config file
source("config.R")

# Load functions
source("./taxonmatch.R")
source("./pdmap.R")
source("./pdindicator.R")
source("./convert_multipolygons.R")

# Load tree
tree <- ape::read.tree(tree_path)
plot(tree, cex=0.45)

# Load datacube
# To do: find most recent common ancestor of tree, generate matching datacube
# through GBIF API
cube <- read.csv(cube_path, stringsAsFactors = FALSE)

# Match tree labels with gbif id's
matched <- taxonmatch(tree)

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
