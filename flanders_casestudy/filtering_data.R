#####################
##### Red list ######
#####################

## Read in necessary packages

library(readr)
library(dplyr)
library(rgbif)
library(tibble)

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
redlist_taxon_sel <- redlist_taxon %>% select(id, scientificName, kingdom)
nrow(redlist_taxon_sel)
redlist_join <- redlist_dist_sel %>% left_join(redlist_taxon_sel, by = "id")
redlist_plantae <- redlist_join %>%
  filter(kingdom == "Plantae")
head(redlist_plantae)

## Characterize  threatStatus categories

unique(redlist_plantae$threatStatus)
redlist_plantae_bin <- redlist_plantae %>%
  add_column(threatened = NA_integer_, .after = "threatStatus")
threatened_cat <- c("EN", "VU", "CR")
redlist_plantae_bin <- redlist_plantae_bin %>%
  mutate(threatened = if_else(threatStatus %in% threatened_cat, 1L, 0L))
redlist_plantae_bin

## Find and add gbif id's to df by matching with GBIF taxonomic backbone

results_strict <- name_backbone_checklist(redlist_plantae_bin$scientificName)
results_strict[results_strict$matchType %in% c("HIGHERRANK", "NONE", "VARIANT"),]
nrow(results_strict)
table(results_strict$matchType)

## Join the results back to  original dataframe

redlist <- redlist_plantae_bin %>% tibble::add_column(results_strict %>%
  select(usageKey)) %>% rename(gbif_taxonID = usageKey)

redlist

## Load input occurrence cube

cube_flanders <- read.csv("data/cube_flanders_Angiosperms_1km.csv", sep="\t")
head(cube_flanders)
nrow(cube_flanders)
length(unique(cube_flanders$specieskey))

## Filter cube to include only red list species

cube_flanders_threatened <- cube_flanders %>% filter(specieskey %in% redlist$gbif_taxonID)
length(unique(cube_flanders_threatened$specieskey))

## Generate once with and once without red listed species

## Make difference between the two so you have a map of 'likely PD loss'

###########################
##### Invasive species ####
###########################

invasives_belgium <- read.csv("data/dwca-alien-plants-belgium-v1.10/taxon.txt", sep="\t")
invasives_Belgium_bb <- name_backbone_checklist(invasives_belgium$scientificName)
invasives_taxonkeys <- invasives_Belgium_bb$usageKey
length(invasives_taxonkeys)


nativeSpecies <- speciesNames[!(invasiveNames %in% speciesNames)]

nativeTaxonBackbone <- lapply(nativeSpecies, name_backbone)
nativeTaxonKeys <- lapply(nativeTaxonBackbone, function(df){df$usageKey})


