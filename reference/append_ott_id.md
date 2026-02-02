# Append ott id's to cube

This function uses the table produced by the
[`taxonmatch()`](https://b-cubed-eu.github.io/pdindicatoR/reference/taxonmatch.md)
function to create a linking table, and then append the `ott_id`'s as a
new field to the occurrence cube.

## Usage

``` r
append_ott_id(tree, cube, matched)
```

## Arguments

- tree:

  An object of class `'phylo'`, a phylogenetic tree in Newick or Nexus
  format that was parsed by `ape::read_tree()`

- cube:

  A dataframe with for selected taxa, the number of occurrences per taxa
  and per grid cell

- matched:

  A dataframe, returned by running the function
  [`taxonmatch()`](https://b-cubed-eu.github.io/pdindicatoR/reference/taxonmatch.md)
  on a phylogenetic tree, which contains the tip labels of the tree and
  their corresponding `gbif_id`'s

## Value

A dataframe which consist of all the data in the original datacube,
appended with columns `ott_id`, `unique_name` and `orig_tiplabel`

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
mcube <- append_ott_id(ex_data$tree, ex_data$cube, ex_data$matched_nona)
mcube <- dplyr::filter(mcube, !is.na(ott_id))
aggr_cube <- aggregate_cube(mcube)
```
