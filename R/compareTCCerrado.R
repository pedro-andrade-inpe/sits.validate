
# Update rownames according to a vector of names that can be one of the above vectors
updateRownames <- function(data, names){
  rownames(data) <- names[strtoi(rownames(data))]
  return(data)
}

# Convert a vector of pixels into a summary with the areas of each class
summarizePixels <- function(pixels, resolution){ # supposes that the resolution is in meters
  result <- table(pixels)
  t(t(round(result * resolution[1] * resolution[2] / 10000))) # to hectares
}

# Compute total area of each class within a raster data
totalPixels <- function(raster){
  resolution <- raster::res(raster)
  values <- raster::getValues(raster)
  summarizePixels(values, resolution)
}

printTotal <- function(result){
  str <- capture.output(print(result))
  str <- str[-(1:2)]
  str[1] <- paste0(" ", str[1])
  cat(paste0(str, "ha\n"))
  cat(paste0("Total: ", sum(result), "ha\n"))
}

#' @title Return the total validation comparing with TerraClass Cerrado
#' @export
totalValidationTCCerrado <- function(result){
  total <- sum(result)
  (sum(result[10:14, 1]) + sum(result[1:5,5]) + result[9,11])/total
}

#' @title Compare with TerraClass Cerrado
#' @description Compares the data with TerraClass Cerrado data for 2013. It returns values in hectares.
#' @param data A raster data.
#' @param log A boolean value indicating whether a log with the processing steps should be printed in the screen.
#' Default is true.
#' @export
compareTCCerrado <- function(data, log = TRUE){
  tccerrado2013 <- raster(mypath("/MapasReferencia/TerraClassCerrado/TCCerrado_2013_GCS_30m/TCCerrado_2013_geo.tif"))

  polygons <- raster::rasterToPolygons(data, dissolve = TRUE) #%>%
  # st_as_sf() %>%
  #  st_transform("+proj=longlat +ellps=GRS80 +no_defs")

  output <- matrix(0, ncol = length(sits_validate.env$classificacao_tc), nrow = length(sits_validate.env$classes_sits))
  colnames(output) <- sits_validate.env$classificacao_tc_simplificada
  rownames(output) <- sits_validate.env$classes_sits

  # update to apply/mclapply
  quantity <- length(polygons)

  for (i in 1:quantity){
    p1 <- polygons[i,]

    line <- p1@data[1,1] #%>% st_set_geometry(NULL)
    #line = line[1,1]

    printLog(paste0("processing ", i, "/", quantity, " (class ", line, ")"), log)

    res1 <- raster::extract(tccerrado2013, p1) # do this for each polygon above
    summ <- summarizePixels(res1[[1]], degreesToMeters(raster::res(tccerrado2013)))

    columns <- strtoi(rownames(t(t(summ))))

    output[line, columns] <- summ[,1]
  }

  output
}
