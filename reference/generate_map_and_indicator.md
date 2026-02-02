# Mapping PD and calculating indicator

This function creates, for a geographic area defined by the user, a map
with the calculated PD metric for each grid cell and the location of
protected nature areas.

## Usage

``` r
generate_map_and_indicator(
  pd_cube,
  grid,
  taxon = NULL,
  bbox_custom = NULL,
  cutoff = NULL
)
```

## Arguments

- pd_cube:

  A dataframe with a variable containing grid cell codes and a variable
  with calculated pd values (output of the function get_pd_cube())

- grid:

  An sf object with variable detailing grid cell codes and a geometry
  column

- taxon:

  A selected higher taxon, for which the occurrence cube was generated.
  Used to generate the map's title only.

- bbox_custom:

  Optional, numeric vector with custom bounding box coordinates as
  c(xmin, xmax, ymin, ymax)

- cutoff:

  A variable of type numeric which determines the cut-off point between
  low PD and high PD

## Value

a list `PDindicator`, which contains one or more maps in it's first
element, and possibly one or more indicator values in it's second
element

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
PDindicator <- generate_map_and_indicator(
  pd_cube,
  ex_data$grid,
  taxon="Fagales",
  cutoff=150)
#> Reading layer `protected_areas_NPHogeKempen' from data source 
#>   `/home/runner/work/_temp/Library/pdindicatoR/extdata/PA_NPHogeKempen/protected_areas_NPHogeKempen.shp' 
#>   using driver `ESRI Shapefile'
#> Simple feature collection with 32 features and 6 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 3948585 ymin: 3065773 xmax: 4049889 ymax: 3141858
#> Projected CRS: ETRS89-extended / LAEA Europe
#> Writing layer `file213f405d23bc' to data source 
#>   `/tmp/RtmptFpecn/file213f405d23bc.gpkg' using driver `GPKG'
#> Writing 1149 features with 4 fields and geometry type Polygon.
#> Reading layer `file213f405d23bc' from data source 
#>   `/tmp/RtmptFpecn/file213f66073fbc.gpkg' using driver `GPKG'
#> Simple feature collection with 1149 features and 4 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 3996000 ymin: 3080000 xmax: 4031000 ymax: 3119000
#> Projected CRS: ETRS89-extended / LAEA Europe
#> Warning: st_centroid assumes attributes are constant over geometries
#> [1] "The percentage of high PD grid cells that fall within protected areas is 20.97 %"
map <- PDindicator[[1]]
indicator <- PDindicator[[2]]
```
