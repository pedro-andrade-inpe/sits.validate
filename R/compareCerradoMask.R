
#' @title Summarize the results as percentages
#' @description Summarize the results for a given region using percentages
#' instead of the absolute values.
#' @param result A classification result.
#' @export
summarizeAsPercentage = function(result){
  sum_columns = apply(result, 2, sum)
  result/sum(sum_columns)*100
}

#' @title Computes the total agreement for cerrado mask
#' @description The total agreement for cerrado mask includes
#' @param result A classification result.
#' @export
totalValidationCerradoMask = function(result){
  percentages = summarizeAsPercentage(result)
  return(sum(percentages[1:5, 1]) + sum(percentages[9:13, 2]))
}

#' @title Compare with Cerrado Mask.
#' @description Compare the classification with Cerrado Mask, a data related to the years from 2000 to 2015,
#' indicating the areas that have antropic use within the Cerrado biome.
#' @param data A raster data.
#' @param progress A boolean value indicating whether this function should print its progress. Default is true.
#' @export
compareWithCerradoMask <- function(data, progress = TRUE){
  printProgress("1/7 - Computing box", progress)

  boxluc <- rasterBoxToPolygon(data) %>%
    sf::st_as_sf() %>%
    sf::st_transform("+proj=longlat +ellps=GRS80 +no_defs")

  printProgress("2/7 - Loading mask 1/3 (slow)", progress)

  mask1 <- sf::read_sf(dsn = basePath("cerrado/prodesMask"), layer = "MASC_CERR_2000_2015_lote_final_cor_pol")
  submask1a <- mask1[boxluc, op = sf::st_overlaps]
  submask1b <- mask1[boxluc, op = sf::st_within]
  rm(mask1)
  gc()

  printProgress("3/7 - Loading mask 2/3 (slow)", progress)

  mask2 <- sf::read_sf(dsn = basePath("cerrado/prodesMask"), layer = "MASC_CERR_2000_2015_lote_final_cor_pol_split1")
  submask2a <- mask2[boxluc, op = sf::st_overlaps]
  submask2b <- mask2[boxluc, op = sf::st_within]
  rm(mask2)
  gc()

  printProgress("4/7 - Loading mask 3/3", progress)

  mask3 <- sf::read_sf(dsn = basePath("cerrado/prodesMask"), layer = "MASC_CERR_2000_2015_lote_final_cor_pol_split2")
  submask3a <- mask3[boxluc, op = sf::st_overlaps]
  submask3b <- mask3[boxluc, op = sf::st_within]
  rm(mask3)
  gc()

  printProgress("5/7 - Joining the three masks", progress)

  submask <- rbind(submask1a, submask1b, submask2a, submask2b, submask3a, submask3b) %>%
    sf::st_transform(sits.validate.env$crs_sits) %>%
    sf::st_combine() %>%
    sf::st_sf()

  #submask = st_union(submask) # polygon has self-intersection? very strange...

  submask[,"abc"] <- "1" # need to do this because raster::extract requires that the data should have at least one attribute

  printProgress("6/7 - Extracting pixels within mask", progress)

  pixels_mask <- unlist(raster::extract(data, submask)) %>% table()

  printProgress("7/7 - Extracting pixels within all the data", progress)

  all_pixels <- raster::getValues(data) %>% table()

  output = matrix(0, ncol = length(sits.validate.env$classes_mask), nrow = length(sits.validate.env$classes_sits))

  output[as.numeric(names(all_pixels)), 1] <- as.vector(all_pixels)
  output[as.numeric(names(pixels_mask)), 2] <- as.vector(pixels_mask)
  output[,1] = output[,1] - output[,2] # removing the pixels within mask from the total

  colnames(output) <- sits.validate.env$classes_mask
  rownames(output) <- sits.validate.env$classes_sits

  output <- output * degreesToMeters(raster::res(data)[1]) / 10000000 # to khectars

  return(output)
}
