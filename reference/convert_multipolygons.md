# Convert multisurface object to multipolygon object

Convert multisurface object to multipolygon object

## Usage

``` r
convert_multipolygons(object)
```

## Arguments

- object:

  An object of class multisurface

## Value

An object of class multipolygon

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
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
pd_cube_geo <- right_join(ex_data$grid, pd_cube,
                          by = join_by(CELLCODE == eeacellcode))
cutoff <- 150
pd_cube_geo$pd_high <- as.factor(ifelse((pd_cube_geo$pd > cutoff), 1, 0))
cube_highpd <- pd_cube_geo[pd_cube_geo$pd_high == 1,
    c("OBJECTID", "CELLCODE", "pd", "geometry", "pd_high")]
cube_mp <- convert_multipolygons(cube_highpd)
#> Writing layer `file213f51fd5997' to data source 
#>   `/tmp/RtmptFpecn/file213f51fd5997.gpkg' using driver `GPKG'
#> Writing 1149 features with 4 fields and geometry type Polygon.
#> Reading layer `file213f51fd5997' from data source 
#>   `/tmp/RtmptFpecn/file213f4c25e29c.gpkg' using driver `GPKG'
#> Simple feature collection with 1149 features and 4 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 3996000 ymin: 3080000 xmax: 4031000 ymax: 3119000
#> Projected CRS: ETRS89-extended / LAEA Europe
```
