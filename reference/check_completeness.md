# Check if provided phylogenetic tree is complete and covers all species in occurrence cube

This function calculates which number of species in the provided
occurrence cube, is not a tip label of the provided phylogenetic tree.

## Usage

``` r
check_completeness(mcube)
```

## Arguments

- mcube:

  A dataframe which is returned by the function append_ott_id(), and
  contains the occurrence datacube with `ott_id` variable appended.
  format that was parsed by `ape::read_tree()`

## Value

a list - first element is the total number of species in the occurrence
cube, second element is the number of species lacking in the
phylogenetic tree.

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
mcube<- append_ott_id(ex_data$tree, ex_data$cube, ex_data$matched_nona)
check_completeness(mcube)
#> The following species are not part of the provided phylogenetic tree:
#>    specieskey                 species
#> 1     9148577            Alnus incana
#> 2          NA                        
#> 3     2880130         Quercus petraea
#> 4     2880580          Quercus cerris
#> 5     8288647 Pterocarya fraxinifolia
#> 6     2879292         Quercus rosacea
#> 7     2879520        Quercus conferta
#> 8     2876571         Alnus pubescens
#> 9     7797155           Alnus hirsuta
#> 10    2880652         Quercus phellos
```
