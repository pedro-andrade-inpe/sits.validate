

#' @title Converts a raster to a SpatialPolygons, keeping the same projection
#' @name rasterBoxToPolygon
#' @author Pedro R. Andrade, \email{pedro.andrade@@inpe.br}
#'
#' @description This function returns a SpatialPolygons with the
#' bounding box of a raster data, with the same projection of the
#' original data.
#' @export
rasterBoxToPolygon = function(raster){
  p <- as(extent(raster), "SpatialPolygons")
  projection(p) <- crs(raster)
  return(p)
}

pixelsWithinRaster <- function(biggerRaster, reference){
    p <- rasterToPolygon(reference)
    p <- spTransform(p, crs(biggerRaster))
    raster::extract(biggerRaster, p)
}

pixelsWithinPolygons <- function(polygons, raster){
    p <- as(extent(raster), "SpatialPolygons")
    projection(p) <- crs(raster)

    p <- spTransform(p, crs(biggerRaster))

    extract(biggerRaster, p)
}
