

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
