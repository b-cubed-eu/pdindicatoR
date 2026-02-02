# Calculation of Faith's PD

This function calculates Faith's PD, based on a provided list of species
and a phylogenetic tree.

## Usage

``` r
calculate_faithpd(tree, species, mrca_node_id)
```

## Arguments

- tree:

  An object of class `'phylo'`, a phylogenetic tree in Newick format
  that was parsed by `ape::read_tree()`

- species:

  A character vector where each element is a species, and more
  specifically, matches a tip label of the phylogenetic tree exactly

- mrca_node_id:

  Node id of the taxon that represents the most recent common ancestor
  of the set of species under study (integer)

## Value

A string that combines "Phylogenetic diversity:" and the calculated
value

## Examples

``` r
ex_data <- retrieve_example_data()
#> Reading layer `EEA_1km_HK' from data source 
#>   `/home/runner/work/_temp/Library/pdindicatoR/extdata/EEA_1km_NPHogeKempen/EEA_1km_HK.shp' 
#>   using driver `ESRI Shapefile'
#> Simple feature collection with 4108 features and 7 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 3974000 ymin: 3068000 xmax: 4051000 ymax: 3125000
#> Projected CRS: ETRS89-extended / LAEA Europe
#> Reading layer `protected_areas_NPHogeKempen' from data source 
#>   `/home/runner/work/_temp/Library/pdindicatoR/extdata/PA_NPHogeKempen/protected_areas_NPHogeKempen.shp' 
#>   using driver `ESRI Shapefile'
#> Simple feature collection with 32 features and 6 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 3948585 ymin: 3065773 xmax: 4049889 ymax: 3141858
#> Projected CRS: ETRS89-extended / LAEA Europe
# determine the most recent common ancestor of all species under study
# (not necessarily all species in the tree!)
mrca_node_id <- ape::getMRCA(ex_data$tree, ex_data$tree$tip.label)
species <- c("Fagus lucida", "Castanopsis fabri", "Quercus_robur")
calculate_faithpd(ex_data$tree, species, mrca_node_id)
#> Warning: Species Quercus_robur not found in tree$tip.label
#> [1] 148.9729
```
