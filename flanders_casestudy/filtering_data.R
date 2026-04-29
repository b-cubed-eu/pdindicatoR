
#####################
### Read in data ####
#####################

## Read in necessary packages

library(readr)
library(dplyr)
library(rgbif)
library(tibble)
library(httr)
library(jsonlite)
library(purrr)
library(ape)

source("R/get_pd_cube.R")

# Register progress handlers
library(progressr)
library(furrr)
handlers(global = TRUE)
handlers("progress")

# Set parallelisation plan

source("params.R")
future::plan(multisession, workers = workers)

# Read in data

cube_path <- "data/Angiosperm_data_Flanders_1km.csv"
cube <- read.csv(cube_path, stringsAsFactors = FALSE, sep = ",")


#####################
##### Invasives #####
#####################

# Filter out invasives on Alien Plants list for Belgium'

# Get invasive gbif keys

 invasives_belgium <- read.csv("data/dwca-alien-plants-belgium-v1.10/taxon.txt", sep="\t")
 invasives_belgium_bb <- name_backbone_checklist(name = invasives_belgium$scientificName)
 table(invasives_belgium_bb$matchType)
 invasive_keys <- invasives_belgium_bb$usageKey
 length(unique(invasive_keys))

# Filter occurrence cube to keep only non-invasive species

 cube_all <- cube
 cube_native <- cube %>% filter(!(specieskey %in% invasive_keys))
 cube <- cube_native
 length(unique(cube_all$specieskey))
 length(unique(cube_native$specieskey))
 write.csv(cube_native,"data/intermediate/cube_native.csv")

 Filter out invasives based on Natuurpunt / Natagora observations checklist

# Read in all observed species on checklist

belgium_all_observed_taxon <- read.csv('data/dwca-natuurpunt-natagora-checklist-v1.3/taxon.txt', stringsAsFactors = FALSE, sep = "\t")
belgium_all_observed_distr <- read.csv('data/dwca-natuurpunt-natagora-checklist-v1.3/distribution.txt', stringsAsFactors = FALSE, sep = "\t")
belgium_all_observed <- left_join(belgium_all_observed_taxon, belgium_all_observed_distr, by = 'id')
be_plants_obs_native <- belgium_all_observed %>% filter(
  kingdom == "Plantae",
    establishmentMeans == "native"
  )

## determine corresponding gbif specieskeys

species_list <- be_plants_obs_native$scientificName
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

# alternative cube waarbij enkel native soorten op observed plantlijst Natuurpunt
# worden behouden
cube_native2 <- cube_native[cube_native$specieskey %in% be_plants_native$acceptedUsageKey,]
cube <- cube_native2
length(unique(cube_native$specieskey)) # 2606/2670 angiosperm species in cube incl +- 1000 not in tree (most invasive but not on GRIIS)
length(unique(cube$specieskey)) # 1166/1175 native angiosperm species
# write.csv(cube_native2,"data/intermediate/cube_native_2.csv")


#####################
##### Red list ######
#####################


## Read in Flanders redlist darwincore archive components

redlist_dist <- read_tsv("data/dwca-rl-flanders-validated-checklist-v1.7/distribution.txt")
redlist_desc <- read_tsv("data/dwca-rl-flanders-validated-checklist-v1.7/description.txt")
redlist_taxon <- read_tsv("data/dwca-rl-flanders-validated-checklist-v1.7/taxon.txt")
head(redlist_dist)
head(redlist_taxon)

## Select necessary columns and join dataframes in order to have scientific name
## and red list status in one dataframe

redlist_dist_sel <- redlist_dist %>% select(id, threatStatus)
nrow(redlist_dist_sel)
redlist_taxon_sel <- redlist_taxon %>% select(id, scientificName, kingdom, phylum)
nrow(redlist_taxon_sel)
redlist_join <- redlist_dist_sel %>% left_join(redlist_taxon_sel, by = "id")
redlist_plantae <- redlist_join %>%
  filter(kingdom == "Plantae")
head(redlist_plantae)
nrow(redlist_plantae)
redlist_tracheophyta <- redlist_plantae[redlist_plantae$phylum=="Tracheophyta",]
nrow(redlist_tracheophyta)

## Characterize  threatStatus categories

unique(redlist_tracheophyta$threatStatus)
rl_tracheophyta_bin <- redlist_tracheophyta %>%
  add_column(threatened = NA_integer_, .after = "threatStatus")
threatened_cat <- c("EN", "VU", "CR")
rl_tracheophyta_bin <- rl_tracheophyta_bin %>%
  mutate(threatened = if_else(threatStatus %in% threatened_cat, 1L, 0L))
tracheophyta_threatened <- rl_tracheophyta_bin[rl_tracheophyta_bin$threatened==1,]
length(unique(tracheophyta_threatened$scientificName))

## Find and add gbif id's to df by matching with GBIF taxonomic backbone

matched <- name_backbone_checklist(tracheophyta_threatened$scientificName)
table(matched$matchType)
results_strict <- matched[!matched$matchType %in% c("HIGHERRANK", "NONE", "VARIANT"),]
nrow(results_strict)

## Join the results back to  original dataframe

tracheophyta_threatened <- tracheophyta_threatened %>% tibble::add_column(matched %>%
  select(usageKey, matchType)) %>% rename(gbif_taxonID = usageKey) %>% filter(matchType == "EXACT")

## Load input occurrence cube

cube_flanders <- read.csv("data/Angiosperm_data_Flanders_5km.csv", sep=",")
head(cube_flanders)
nrow(cube_flanders)
length(unique(cube_flanders$specieskey))

## Filter cube to include only non-threatened species

cube_flanders_nonthreatened <- cube_flanders %>% filter(!(specieskey %in% tracheophyta_threatened$gbif_taxonID))
cube_flanders_threatened <- cube_flanders %>% filter((specieskey %in% tracheophyta_threatened$gbif_taxonID))
length(unique(cube_flanders_nonthreatened$specieskey))
length(unique(cube_flanders_threatened$specieskey))

## Write out filtered cubes

write.csv(cube_flanders_nonthreatened,"data/Angiosperm_Flanders_5km_nonthreatened.csv")
write.csv(cube_flanders_threatened,"data/Angiosperm_Flanders_5km_threatened.csv")




