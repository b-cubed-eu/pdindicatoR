# Visualizing PD maps for time periods in tabs

This function creates produces an r-shiny app that can showcase multiple
PD maps (for separate time periods) in tabs

## Usage

``` r
make_shiny_maps(pd_indicator, plots)
```

## Arguments

- pd_indicator:

  List containing PD plots and indicators, produced by function
  generate_map_and_indicator.R

- plots:

  A list of PD maps produced by the function
  [`generate_map_and_indicator()`](https://b-cubed-eu.github.io/pdindicatoR/reference/generate_map_and_indicator.md),
  named by their time-period.

## Value

An r-shiny app with PD maps in tabs

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
pd_cube <- get_pd_cube(mcube, ex_data$tree)
pd_indicator<- generate_map_and_indicator(
  pd_cube,
  ex_data$grid,
  taxon="Fagales")
#> Reading layer `protected_areas_NPHogeKempen' from data source 
#>   `/home/runner/work/_temp/Library/pdindicatoR/extdata/PA_NPHogeKempen/protected_areas_NPHogeKempen.shp' 
#>   using driver `ESRI Shapefile'
#> Simple feature collection with 32 features and 6 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 3948585 ymin: 3065773 xmax: 4049889 ymax: 3141858
#> Projected CRS: ETRS89-extended / LAEA Europe
plots <- pd_indicator[[1]]
#> Error in S7::prop(x, "meta")[[i]]: subscript out of bounds
indicators <- pd_indicator[[2]]
#> Error in S7::prop(x, "meta")[[i]]: subscript out of bounds
if (FALSE) make_shiny_maps(pd_indicator, plots) # \dontrun{}
```
