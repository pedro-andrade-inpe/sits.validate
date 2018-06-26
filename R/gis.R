
#' @title Converts a raster to a SpatialPolygons, keeping the same projection
#' @name rasterBoxToPolygon
#' @author Pedro R. Andrade, \email{pedro.andrade@@inpe.br}
#'
#' @description This function returns a SpatialPolygons with the
#' bounding box of a raster data, with the same projection of the
#' original data.
#' @param raster A raster::raster.
#' @export
rasterBoxToPolygon <- function(raster){
  p <- methods::as(raster::extent(raster), "SpatialPolygons")
  print(class(p))
  raster::projection(p) <- raster::crs(raster)
  return(p)
}

#' @title Empty raster from a given raster file
#' @description Return an empty raster with the same
#' pixels of a given raster stored into a file.
#' @param filename Name of a file that stores raster data.
#' @export
getEmptyRaster <- function(filename){
  raster::raster(filename) %>% raster::raster()
}

#' @title Get sits palette
#' @description Return the sits palette with the values, colors, and labels.
#' @param filename File name to be read (a qml file). As default, it uses the
#' sits palette, stored in style_cerrado_13_classes.qml.
#' @export
getPalette <- function(filename = baseDir("style_cerrado_13classes.qml")){
  xml <- XML::xmlParse(filename) %>% XML::xmlToList()

  palette <- t(as.data.frame(xml$pipe$rasterrenderer$colorPalette))

  rownames(palette) <- NULL

  return(palette)
}
