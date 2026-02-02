# Aggregate datacube over grid cell to create new dataframe with species list per grid

This function aggregates a provided datacube over grid cell id, so that
a new datacube is outputted with 3 variables that contain the lists of
species that are observed for each grid cell (as `speciesKeys`,
`ott_id`'s and names).

## Usage

``` r
aggregate_cube(mcube, timegroup = NULL)
```

## Arguments

- mcube:

  An occurrence datacube with appended `ott_id`'s, as produced by the
  [`append_ott_id()`](https://b-cubed-eu.github.io/pdindicatoR/reference/append_ott_id.md)
  function

- timegroup:

  An integer, representing the number of years by which you want to
  group your occurrence data

## Value

A dataframe with for each grid cell a list of observed species

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
