# Read in necessary source code, packages and data

files <- list.files(path = "R", pattern = "\\.R$", full.names = TRUE)
source("R/get_pd_cube.R")
source("R/aggregate_cube.R")
source("R/generate_map_and_indicator.R")
source("R/retrieve_example_data.R")
lapply(files, source)

# sapply(files, source)
library(ape)
library(dplyr)
library(httr)
library(jsonlite)
library(rgbif)
library(purrr)


# Register progress handlers
library(progressr)
library(furrr)
handlers(global = TRUE)
handlers("progress")

# Set parallelisation plan

workers <- parallel::detectCores() - 10
future::plan(multisession, workers = workers)

# Data
tree_path <- "data/oo_330891.tre"
cube_path <- "data/cube_flanders_Angiosperms_1km.csv"
grid_path <- "data/shpfiles/EEA_BE_1km/EEA_BE_1km.shp"

tree <- read.nexus(tree_path)
tree$tip.label <- lapply(tree$tip.label, function(label) {gsub("_", " ", label)})
tree$tip.label

options(width = 1000)
plot(tree, cex = 0.35, y.lim = 100)

cube <- read.csv(cube_path, stringsAsFactors = FALSE, sep = "\t")
grid <- sf::st_read(grid_path)


# Filter out invasives out of occurrence cube
## Get invasive gbif keys
invasives_belgium <- read.csv("data/dwca-alien-plants-belgium-v1.10/taxon.txt", sep="\t")
invasives_belgium_bb <- name_backbone_checklist(name = invasives_belgium$scientificName)
# write.csv(invasives_belgium_bb,"data/intermediate/invasives_Belgium_bb.csv")

## read in from file:
# invasives_belgium_bb <- read.csv('data/intermediate/invasives_Belgium_bb.csv', stringsAsFactors = FALSE, sep = "\t")


## Filter invasives out of cube

invasive_keys <- invasives_belgium_bb$usageKey

# filter occurrence cube to keep only non-invasive species
cube_native <- cube %>%
  filter(!(specieskey %in% invasive_keys))
cube_all <- cube
cube <- cube_native
# write.csv(cube_native,"data/intermediate/cube_native.csv")

# read in all observed species in Belgium (natuurpunt)
belgium_all_observed_taxon <- read.csv('data/dwca-natuurpunt-natagora-checklist-v1.3/taxon.txt', stringsAsFactors = FALSE, sep = "\t")
belgium_all_observed_distr <- read.csv('data/dwca-natuurpunt-natagora-checklist-v1.3/distribution.txt', stringsAsFactors = FALSE, sep = "\t")
belgium_all_observed_distr <- belgium_all_observed_distr[belgium_all_observed_distr$locationID=='ISO_3166-2:BE',]
belgium_all_observed <- left_join(belgium_all_observed_taxon, belgium_all_observed_distr, by = 'id')
belgium_plants_obs <- belgium_all_observed[belgium_all_observed$kingdom=="Plantae",]
belgium_plants_obs_native <- belgium_plants_obs[belgium_plants_obs$establishmentMeans=='native',]

# determine corresponding gbif specieskeys
species_list <- belgium_plants_obs_native$scientificName
# be_plants_native <- name_backbone_checklist(name = specieslist)
# rgbif::name_backbone_checklist() currently doesn't return acceptedUsageKey
# alternative:

match_species <- function(sp_name, dataset_key) {

  res <- tryCatch(
    GET(
      "https://api.gbif.org/v1/species/match",
      query = list(name = sp_name, datasetKey = dataset_key),
      timeout(10)
    ),
    error = function(e) NULL
  )

  if (is.null(res) || status_code(res) != 200) {
    return(tibble(
      verbatim_name = sp_name,
      usageKey = NA_integer_,
      scientificName = NA_character_,
      status = NA_character_,
      rank = NA_character_,
      matchType = NA_character_,
      confidence = NA_real_,
      acceptedUsageKey = NA_integer_,
      kingdomKey = NA_integer_,
      phylumKey = NA_integer_,
      classKey = NA_integer_,
      orderKey = NA_integer_,
      familyKey = NA_integer_
    ))
  }

  data <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)

  tibble(
    verbatim_name   = sp_name,
    usageKey          = data$usageKey %||% NA_integer_,
    scientificName    = data$scientificName %||% NA_character_,
    status            = data$status %||% NA_character_,
    rank              = data$rank %||% NA_character_,
    matchType         = data$matchType %||% NA_character_,
    confidence        = data$confidence %||% NA_real_,
    acceptedUsageKey  = data$acceptedUsageKey %||% NA_integer_,
    kingdomKey        = data$kingdomKey %||% NA_integer_,
    phylumKey         = data$phylumKey %||% NA_integer_,
    classKey          = data$classKey %||% NA_integer_,
    orderKey          = data$orderKey %||% NA_integer_,
    familyKey         = data$familyKey %||% NA_integer_
  )
}

plan(multisession, workers = 4)

dataset_key <- "d7dddbf4-2cf0-4f39-9b2a-bb099caae36c"

start_time <- Sys.time()
be_plants_native <- future_map_dfr(
  species_list,
  match_species,
  dataset_key = dataset_key,
  .options = furrr_options(seed = TRUE)
)

end_time <- Sys.time()

cat("Total runtime:", round(difftime(end_time, start_time, units = "mins"), 2), "minutes\n")

be_plants_native <- be_plants_native %>%
  mutate(
    acceptedUsageKey = if_else(
      status == "ACCEPTED",
      usageKey,
      acceptedUsageKey
    )
  )

be_plants_native <- be_plants_native[be_plants_native$matchType %in% c("EXACT", "FUZZY"), ]

# write.csv(cube_native,"data/intermediate/be_plants_native.csv")
# read in from file
# be_plants_native <- read.csv('data/intermediate/be_plants_native.csv', stringsAsFactors = FALSE, sep = "\t")

# alternative cube (only species on observed plant list Natuurpunt)
cube_native2 <- cube_native[cube_native$specieskey %in% be_plants_native$acceptedUsageKey,]
cube <- cube_native2
length(unique(cube_native$specieskey)) # 2606 angiosperm species in cube incl +- 1000 not in tree (most invasive but not on GRIIS)
length(unique(cube$specieskey)) # 1166 native angiosperm species
# write.csv(cube_native,"data/intermediate/cube_native_2.csv")

# Exploratory analysis
length(invasive_keys)
length(unique(invasive_keys)) # 2590 unique invasive species keys
length(unique(cube_all$specieskey)) # 4060 species in original cube
length(unique(cube_native$specieskey)) # 2606 species in cube with only natives
nrow(belgium_plants_obs)
length(unique(cube_native2$specieskey)) # 1166 native angiosperm species in final cube


# Find GBIF id's corresponding to tip labels

# taxonmatch(tree)
tree_labels <- tree$tip.label
tree_labels <- unlist(tree_labels)

plan(multisession, workers = 4)

dataset_key <- "d7dddbf4-2cf0-4f39-9b2a-bb099caae36c"

start_time <- Sys.time()

matched_all <- future_map_dfr(
  tree_labels,
  match_species,
  dataset_key = dataset_key,
  .options = furrr_options(seed = TRUE)
)

end_time <- Sys.time()

cat("Total runtime:", round(difftime(end_time, start_time, units = "mins"), 2), "minutes\n")

matched_all <- matched_all %>%
  mutate(
    acceptedUsageKey = if_else(
      status == "ACCEPTED",
      usageKey,
      acceptedUsageKey
    )
  )

matched <- matched_all[matched_all$matchType %in% c("EXACT", "FUZZY"), ]
# write.csv(matched,"data/intermediate/matched_exactfuzzy_Steventree.csv")
# read file without calculating again:
# matched <- read.csv('data/intermediate/matched_Steventree.csv', stringsAsFactors = FALSE, sep = ",")

# Append orig_tiplabel to cube
source("R/append_ott_id.R")
source("R/check_completeness.R")

mcube <- append_ott_id(tree, cube, matched)
head(mcube)

# not_in_tree <- mcube[is.na(mcube$verbatim_name), ]
not_in_tree <- mcube[is.na(mcube$verbatim_name), ]
head(not_in_tree)
unique<- unique(not_in_tree$species)
length(unique)
# Schrijf lijst van 222 soorten die wel in cube maar niet in tree voorkomen weg
# write.csv(unique,"data/intermediate/not_in_tree_update.csv")

# Remove occurrences that can't be matched to a tree species
nrow(mcube)
mcube <- mcube %>% dplyr::filter(!is.na(verbatim_name))
head(mcube)
# write.csv(mcube,"data/intermediate/mcube.csv")
# read file without calculating:
# mcube <- read.csv('data/intermediate/mcube.csv', stringsAsFactors = FALSE, sep = ",")
mcube_orig <- mcube

# Calculate PD
source("R/get_pd_cube.R")
source("R/aggregate_cube.R")
source("R/calculate_FaithPD.R")

# test with smaller cube
# group by year

cube_year <- mcube %>%
  mutate(
    year = as.integer(substr(yearmonthday, 1, 4))
  ) %>%
  group_by(
    kingdom,
    kingdomkey,
    phylum,
    phylumkey,
    class,
    classkey,
    order,
    orderkey,
    family,
    familykey,
    genus,
    genuskey,
    species,
    specieskey,
    eeacellcode,
    year,
    verbatim_name,
  ) %>%
  summarise(
    # summed count columns
    ordercount   = sum(ordercount,   na.rm = TRUE),
    familycount  = sum(familycount,  na.rm = TRUE),
    genuscount   = sum(genuscount,   na.rm = TRUE),
    occurrences  = sum(occurrences,  na.rm = TRUE),
  )

mcube <- cube_year
pdindicator <- generate_map_and_indicator(pd_cube, grid, "Angiosperms for Flanders")
pdindicator
