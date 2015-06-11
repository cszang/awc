##' Extract AWC for Point from Webb et al.'s data base
##'
##' This function extracts the AWC for a given point (list of
##'     coordinates as lon and lat). In case there no value at point,
##'     the functions looks around a bit to find the next gridpoint
##'     that is not NA.
##'
##' The data set used is the texture derived soil water holding
##'     capacity from Webb et al., that is freely available via
##'     http://daac.ornl.gov/SOILS/guides/Webb.html
##' @title Extract the AWP for a Coordinate Pair
##' @param coords a list with lon and lat giving the coordinates
##' @return a single value for AWC (in mm)
##' @examples get_awc(list(lon = 11.74, lat = 42.94))
##' @references Webb, R. W., C. E. Rosenzweig, and
##'     E. R. Levine. 2000. Global Soil Texture and Derived
##'     Water-Holding Capacities (Webb et al.). Data set. Available
##'     on-line [http://www.daac.ornl.gov] from Oak Ridge National
##'     Laboratory Distributed Active Archive Center, Oak Ridge,
##'     Tennessee, U.S.A. doi:10.3334/ORNLDAAC/548.
##' @import sp
##' @import raster
##' @export

get_awc <- function(coords) {
  spp <- SpatialPoints(data.frame(x = round(coords$lon), y =
                                  round(coords$lat)))
  ex <- extract(awc, spp)
  if (is.na(ex)) {
    ## we go further and use a largely expanded grid to look for some
    ## rough estimate of AWC as the nearest available data point
    xycords <- expand.grid(
      data.frame(x = seq(round(coords$lon) - 5,
                         round(coords$lon) + 5),
                 y = seq(round(coords$lat) - 5,
                         round(coords$lat) + 5)))
    spp <- SpatialPoints(xycords)
    ex <- extract(awc, spp)
    naex <- which(is.na(ex))
    ex <- ex[-naex]
    dists <- sqrt((coords$lon - xycords$x)^2 + (coords$lat -
                                                  xycords$y)^2)[-naex]
    mindist <- which.min(dists)
    ex[mindist]
  } else {
    ex
  }
}
