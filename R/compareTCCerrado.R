
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

#' @title Return the total validation comparing with TerraClass Cerrado
#' @description The total validation is the sum of Soy_Corn, Soy_Cotton, Soy_Fallow, Soy_Millet, Sugarcane areas
#' classified as Agricultura anual, plus Araguaia, Campo_Cerrado, Cerradao, Cerrado, and Cerrado_Rupestre areas
#' classified as Natural, plus Pasture areas classified as Pastagem.
#' @param result The result of compareTCCerrado().
#' @seealso compareTCCerrado
#' @export
totalValidationTCCerrado <- function(result){
  total <- sum(result)
  return((sum(result[10:14, 1]) + sum(result[1:5, 5]) + result[9, 11]) / total)
}

#' @title Compare with TerraClass Cerrado
#' @description Compares the data with TerraClass Cerrado data for 2013. It returns values in hectares.
#' @param data A raster data.
#' @param progress A boolean value indicating whether this function should print its progress. Default is true.
#' @export
compareTCCerrado <- function(data, progress = TRUE){
  tccerrado2013 <- raster::raster(baseDir("cerrado/terraClass/TCCerrado_2013_geo.tif"))

  polygons <- raster::rasterToPolygons(data, dissolve = TRUE) %>%
    sf::st_as_sf() %>%
    sf::st_transform("+proj=longlat +ellps=GRS80 +no_defs")

  mynames <- sits.validate.env$classificacao_tc
  quantity <- dim(polygons)[1]
  output <- tibble::tibble(rowname = sits.validate.env$classes_sits)

  # update to apply/mclapply
  for (i in 1:quantity){
    p1 <- polygons[i,]

    line <- as.data.frame(p1)[,1]

    printProgress(paste0("processing ", i, "/", quantity, " (class ", line, ")"), progress)

    res1 <- raster::extract(tccerrado2013, p1, progress = "text") # do this for each polygon above
    summ <- summarizePixels(res1[[1]], degreesToMeters(raster::res(tccerrado2013)))

    columns <- strtoi(rownames(summ))
    summ <- as.vector(summ)

    myname <- mynames[line]

    output[, myname] <- 0
    output[columns, myname] <- summ
  }

  output %>% dplyr::filter(rowSums(.[-1]) != 0)
}
