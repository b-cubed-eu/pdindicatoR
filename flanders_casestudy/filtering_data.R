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

cube_flanders <- read.csv("data/Angiosperm_data_Flanders.csv", sep=",")
head(cube_flanders)
nrow(cube_flanders)
length(unique(cube_flanders$specieskey))

## Filter cube to include only non-threatened species
cube_flanders_nonthreatened <- cube_flanders %>% filter(!(specieskey %in% tracheophyta_threatened$gbif_taxonID))
cube_flanders_threatened <- cube_flanders %>% filter((specieskey %in% tracheophyta_threatened$gbif_taxonID))
length(unique(cube_flanders_nonthreatened$specieskey))
length(unique(cube_flanders_threatened$specieskey))

write.csv(cube_flanders_nonthreatened,"data/Angiosperm_Flanders_nonthreatened.csv")
write.csv(cube_flanders_threatened,"data/Angiosperm_Flanders_threatened.csv")




