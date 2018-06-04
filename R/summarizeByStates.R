
# Convert a vector of pixels into a summary with the areas of each class
summarizePixels <- function(pixels, resolution){ # supposes that the resolution is in meters
  result <- table(pixels)
  t(t(round(result * resolution[1] * resolution[2] / 10000))) # to hectares
}

#' @title Summarize the classification areas into a set of polygons
#' @name summarizeOneByPolygons
#' @author Pedro R. Andrade, \email{pedro.andrade@@inpe.br}
#'
#' @description This function returns a table separating the
#' classifications according to the overlapping areas with a
#' set of polygons.
#' @export
summarizeOneByPolygons <- function(data, layer, attribute, log = TRUE){
  brazil <- sf::st_read(dsn <- mypath("Shapefiles/Brasil"), layer = layer, quiet=TRUE) %>%
    sf::st_transform("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")

  quantity <- dim(brazil)[1]

  mynames <- as.data.frame(brazil)[,attribute]

  output <- matrix(0, ncol = quantity, nrow = length(sits_validate.env$classes_sits))
  colnames(output) <- mynames
  rownames(output) <- sits_validate.env$classes_sits

  for(i in 1:quantity){
    cat(paste0("Processing ", i, "/", quantity, " object '", mynames[i], "'\n"))

    polygon <- brazil[i,]

    summ <- raster::extract(data, polygon) %>% summarizePixels(degreesToMeters(raster::res(data)))
    columns <- strtoi(rownames(t(t(summ))))
    output[columns, i] <- summ[,1]
  }

  output
}

#' @title Summarize the classification areas in each Brazilian municipality.
#' @name summarizeOneByMunicipalities
#' @author Pedro R. Andrade, \email{pedro.andrade@@inpe.br}
#'
#' @description This function returns the area of each class within
#' each Brazilian municipality. The output is a data.frame with the
#' same row names (classes) and municipalities as columns. It uses file
#' 55mu2500gsd_removednull.shp, with 5,564 municipalities.
#' @export
summarizeOneByMunicipalities <- function(data, log = TRUE){
  summarizeOneByPolygons(data, "55mu2500gsd_removednull", "Nome_Munic", log)
}

#' @title Summarize the classification areas in each Brazilian municipality.
#' @name summarizeOneByMunicipalities
#' @author Pedro R. Andrade, \email{pedro.andrade@@inpe.br}
#'
#' @description This function returns the area of each class within
#' each Brazilian municipality. The output is a data.frame with the
#' same row names (classes) and municipalities as columns. It uses file
#' UFEBRASIL.shp, with 27 objects.
#' @param data A raster::raster object.
#' @param log A boolean value indicating whether a log should be printed along the execution.
#' @export
summarizeOneByStates <- function(data, log = TRUE){
  summarizeOneByPolygons(data, "UFEBRASIL", "NM_ESTADO", log)
}

#' @title Summarize the classification areas in each Brazilian simulation units.
#' @name summarizeOneByMunicipalities
#' @author Pedro R. Andrade, \email{pedro.andrade@@inpe.br}
#'
#' @description This function returns the area of each class within
#' each Brazilian municipality. The output is a data.frame with the
#' same row names (classes) and municipalities as columns. It uses file
#' 55mu2500gsd_removednull.shp, with 18,194 simulation units.
#' @param data A raster::raster object.
#' @param log A boolean value indicating whether a log should be printed along the execution.
#' @export
summarizeOneBySimU <- function(data, log = TRUE){
  summarizeOneByPolygons(data, "simu_brazil_disjoint_units", "grd30", log)
}

