
# Required packages: tidyverse, rotl, sf, gdalUtilities, ape, rnaturalearth, purrr

# Load config file
source("./config.R")

# Set working directory
setwd(wd_path)

# following lines can probably be removed once the package is fully documented
# because functions are exported

# Load functions
source("./R/taxonmatch.R")
source("./R/aggregate_cube.R")
source("./R/append_ott_id.R")
source("./R/pdmap.R")
source("./R/pdindicator.R")
source("./R/convert_multipolygons.R")
source("./R/calculate_PD.R")
source("./R/check_completeness.R")
source("./R/aggregate_cube.R")

# add dplyr functions to namespace
#' @import dplyr
library(dplyr)

# Load tree
tree <- ape::read.tree(tree_path)
plot(tree, cex=0.45)
ggtree(tree) + geom_tiplab(size = 2) + ylim(0, 150)

# Load datacube

cube <- read.csv(cube_path2, stringsAsFactors = FALSE, sep="\t") # GBIF SQL api currently returns tab-delimited files with lowercase field names
cube <- read.csv(cube_path, stringsAsFactors = FALSE) # Use in case cube has comma-seperated format
head(cube)

# Match leaf labels of tree with GBIF id's
matched <- taxonmatch(tree)

# Append OTT_id's to cube
mcube <- append_ott_id(tree, cube, matched)
# TO DO: generate info/warning message with how many specieskeys could not be match with a tree leaf label (count(ott_id is NA)
# Give option to continue and remove non-matched species from occurence cube OR upload a new tree

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
aggr_cube[1,2][1]

PD_cube <- aggr_cube %>% mutate(PD = purrr::map(unique_names, calculate_pd, tree=tree))


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

tree$tip.label
y <- c("Martes foina", "Mustela putorius")
x <- vector("list", length(y))
j = 1
for (i in y){
  x[[j]] <- which(tree$tip.label == i)
  j = j+1
}
x
