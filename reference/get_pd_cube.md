# Find MRCA for data cube and call function to calculate PD metrics

This function determines the MRCA of all species in the datacube and
calls the function(s) to calculate PD metrics

## Usage

``` r
get_pd_cube(mcube, tree, timegroup = NULL, metric = "faith")
```

## Arguments

- mcube:

  An occurrence data cube with matched names appended, product of
  function
  [`taxonmatch()`](https://b-cubed-eu.github.io/pdindicatoR/reference/taxonmatch.md)

- tree:

  A phylogenetic tree with branch lengths

- timegroup:

  Optional, an integer which represents the number of years over which
  occurrences need to be aggregated and the PD value calculated

- metric:

  Name of the PD metric to be calculated

## Value

Calculated PD value

## Examples

``` r
library(dplyr)
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
pd_cube <- get_pd_cube(mcube, ex_data$tree, metric="faith")
```
