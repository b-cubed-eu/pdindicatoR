% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_multipolygons.R
\name{convert_multipolygons}
\alias{convert_multipolygons}
\title{Convert multisurface object to multipolygon object}
\usage{
convert_multipolygons(object)
}
\arguments{
\item{object}{An object of class multisurface}
}
\value{
An object of class multipolygon
}
\description{
Convert multisurface object to multipolygon object
}
\examples{
library(dplyr)
ex_data <- retrieve_example_data()
mcube <- append_ott_id(ex_data$tree, ex_data$cube, ex_data$matched_nona)
mcube <- dplyr::filter(mcube, !is.na(ott_id))
PD_cube <- get_pd_cube(mcube, ex_data$tree)
PD_cube_geo <- right_join(ex_data$grid, PD_cube,
                          by = join_by(CELLCODE == eeacellcode))
cutoff <- 150
PD_cube_geo$PD_high <- as.factor(ifelse((PD_cube_geo$PD > cutoff), 1, 0))
cube_highPD <- PD_cube_geo[PD_cube_geo$PD_high == 1,
    c("OBJECTID", "CELLCODE", "PD", "geometry", "PD_high")]
cube_mp <- convert_multipolygons(cube_highPD)
}
