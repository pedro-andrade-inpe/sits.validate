
printTotal = function(result){
  str = capture.output(print(result))
  str = str[-(1:2)]
  str[1] = paste0(" ", str[1])
  cat(paste0(str, "ha\n"))
  cat(paste0("Total: ", sum(result), "ha\n"))
}

summarizeAsPercentage = function(result){
  sum_columns = apply(result, 2, sum)
  result/sum(sum_columns)*100
}

totalValidation = function(result){
  percentages = summarizeAsPercentage(result)
  return (sum(percentages[1:5,1]) + sum(percentages[9:13,2]))
}

#' @title Compare with Cerrado Mask.
#' @export
compareWithCerradoMask <- function(data, log = TRUE){
  printLog("1/7 - Computing box", log)

  boxluc <- rasterBoxToPolygon(data) %>%
    sf::st_as_sf() %>%
    sf::st_transform("+proj=longlat +ellps=GRS80 +no_defs")

  printLog("2/7 - Loading mask 1/3 (slow)", log)

  mask1 = sf::st_read(dsn = mypath("MapasReferencia/Cerrado"), layer = "MASC_CERR_2000_2015_lote_final_cor_pol", quiet=TRUE)
  submask1a = mask1[boxluc, op = st_overlaps]
  submask1b = mask1[boxluc, op = st_within]
  rm(mask1)
  gc()

  printLog("3/7 - Loading mask 2/3 (slow)", log)

  mask2 = sf::st_read(dsn = mypath("MapasReferencia/Cerrado"), layer = "MASC_CERR_2000_2015_lote_final_cor_pol_split1", quiet=TRUE)
  submask2a = mask2[boxluc, op = st_overlaps]
  submask2b = mask2[boxluc, op = st_within]
  rm(mask2)
  gc()

  printLog("4/7 - Loading mask 3/3", log)

  mask3 = sf::st_read(dsn = mypath("MapasReferencia/Cerrado"), layer = "MASC_CERR_2000_2015_lote_final_cor_pol_split2", quiet=TRUE)
  submask3a = mask3[boxluc, op = st_overlaps]
  submask3b = mask3[boxluc, op = st_within]
  rm(mask3)
  gc()

  printLog("5/7 - Joining the three masks", log)

  submask = rbind(submask1a, submask1b, submask2a, submask2b, submask3a, submask3b) %>%
    st_transform("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs") %>%
    st_combine() %>%
    st_sf()

  #submask = st_union(submask) # polygon has self-intersection? very strange...

  submask[,"abc"] = "1" # need to do this because raster::extract requires that the data should have at least one attribute

  printLog("6/7 - Extracting pixels within mask", log)

  pixels_mask = unlist(raster::extract(data, submask)) %>% table()

  printLog("7/7 - Extracting pixels within all the data", log)

  all_pixels = raster::getValues(data) %>% table()

  output = matrix(0, ncol = length(classes_mask), nrow = length(classes_sits))

  output[as.numeric(names(all_pixels)), 1] = as.vector(all_pixels)
  output[as.numeric(names(pixels_mask)), 2] = as.vector(pixels_mask)
  output[,1] = output[,1] - output[,2] # removing the pixels within mask from the total

  colnames(output) = classes_mask
  rownames(output) = classes_sits

  output = output * degreesToMeters(raster::res(data)[1]) / 10000000 # to khectars

  return(output)
}
