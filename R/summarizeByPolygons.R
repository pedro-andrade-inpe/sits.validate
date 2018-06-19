
# Convert a vector of pixels into a summary with the areas of each class
summarizePixels <- function(pixels, resolution){ # supposes that the resolution is in meters
  result <- table(pixels)
  round(result * resolution[1] * resolution[2] / 10000) # to hectares
}

#' @title Summarize the classification areas into a set of polygons
#' @author Pedro R. Andrade, \email{pedro.andrade@@inpe.br}
#' @description This function returns a table separating the
#' classifications according to the overlapping areas with a
#' set of polygons. It uses files within Dropbox/sits.validate/shapes.
#' @param data A raster::raster data.
#' @param layer Name of the layer with the polygons.
#' @param attribute Name of an attribute from the polygons data to be shown in the progress.
#' @param progress A boolean value indicating whether this function should print its progress. Default is true.
#' @seealso summarizeOneByMunicipalities summarizeOneByStates summarizeOneBySimU
#' @export
summarizeOneByPolygons <- function(data, layer, attribute, progress = TRUE){
  polygons <- sf::read_sf(dsn = baseDir("shapes"), layer = layer) %>%
    sf::st_transform(sits.validate.env$crs_sits)

  quantity <- dim(polygons)[1]
  mynames <- as.data.frame(polygons)[,attribute]
  output <- tibble::tibble(rowname = sits.validate.env$classes_sits)

  for(i in 1:quantity){
    myname <- mynames[i]

    polygon <- polygons[i,]

    inter <- raster::intersect(raster::extent(polygon), raster::extent(data))

    if(!is.null(inter)){
      printProgress(paste0("Processing ", i, "/", quantity, " object '", myname, "'"), progress)

      summ <- raster::extract(data, polygon, progress = "text") %>%
        summarizePixels(degreesToMeters(raster::res(data)))

      columns <- strtoi(rownames(summ))
      summ <- as.vector(summ)

      if(sum(summ > 0)){
          output[, myname] <- 0
          output[columns, myname] <- summ
      }
    }
  }

  # remove all lines (rows) that have only zeros
  output %>% dplyr::filter(rowSums(.[-1]) != 0)
}

#' @title Summarize the classification areas in each Brazilian municipality.
#' @author Pedro R. Andrade, \email{pedro.andrade@@inpe.br}
#' @description This function returns the area of each class within
#' each Brazilian municipality. The output is a data.frame with the
#' same row names (classes) and municipalities as columns. It uses file
#' Dropbox/sits.validate/shapes/55mu2500gsd_removednull.shp, with 5,564 municipalities.
#' @param data A raster::raster data.
#' @param progress A boolean value indicating whether this function should print its progress. Default is true.
#' @export
summarizeOneByMunicipalities <- function(data, progress = TRUE){
  summarizeOneByPolygons(data, "55mu2500gsd_removednull", "Nome_Munic", progress)
}

#' @title Summarize the classification areas in each Brazilian municipality.
#' @author Pedro R. Andrade, \email{pedro.andrade@@inpe.br}
#' @description This function returns the area of each class within
#' each Brazilian municipality. The output is a data.frame with the
#' same row names (classes) and municipalities as columns. It uses file
#' Dropbox/sits.validate/shapes/UFEBRASIL.shp, with 27 objects.
#' @param data A raster::raster data.
#' @param progress A boolean value indicating whether this function should print its progress. Default is true.
#' @export
summarizeOneByStates <- function(data, progress = TRUE){
  summarizeOneByPolygons(data, "UFEBRASIL", "NM_ESTADO", progress)
}

#' @title Summarize the classification areas in each Brazilian simulation units.
#' @author Pedro R. Andrade, \email{pedro.andrade@@inpe.br}
#' @description This function returns the area of each class within
#' each Brazilian municipality. The output is a data.frame with the
#' same row names (classes) and municipalities as columns. It uses file
#' Dropbox/sits.validate/shapes/55mu2500gsd_removednull.shp, with 18,194 simulation units.
#' @param data A raster::raster data.
#' @param progress A boolean value indicating whether this function should print its progress. Default is true.
#' @export
summarizeOneBySimU <- function(data, progress = TRUE){
  summarizeOneByPolygons(data, "simu_brazil_disjoint_units", "grd30", progress)
}

