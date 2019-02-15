
require(sits.validate)
require(fasterize)

for(year in 2001:2015){
#  year = 2000
  cat(paste0("Processing year ", year, "\n"))

  sitstif <- baseDir("classificacoes-final/result-cerrado_2012_8_2013_8.tif")

  cat(paste0("Reading data\n"))

  # data was reprojected
  #mask1 <- sf::read_sf(dsn = baseDir("cerrado/prodesMask"), layer = "MASC_CERR_2000_2015_lote_final_cor_pol")
  #mask2 <- sf::read_sf(dsn = baseDir("cerrado/prodesMask"), layer = "MASC_CERR_2000_2015_lote_final_cor_pol_split1")
  #mask3 <- sf::read_sf(dsn = baseDir("cerrado/prodesMask"), layer = "MASC_CERR_2000_2015_lote_final_cor_pol_split2")

  mask1 <- sf::read_sf(dsn = baseDir("cerrado/prodesMask/reproj"), layer = "mask1")
  mask2 <- sf::read_sf(dsn = baseDir("cerrado/prodesMask/reproj"), layer = "mask2")
  mask3 <- sf::read_sf(dsn = baseDir("cerrado/prodesMask/reproj"), layer = "mask3")

  cat(paste0("Selecting year\n"))

  # removing water polygons
  mask1 <- mask1[mask1$class_name %in% paste0("D_", 2000:year),]
  mask2 <- mask2[mask2$class_name %in% paste0("D_", 2000:year),]
  mask3 <- mask3[mask3$class_name %in% paste0("D_", 2000:year),]

  mask <- rbind(mask1, mask2, mask3)

  cat("Number of polygons: ", dim(mask)[1], "\n")

  cat(paste0("Removing useless data\n"))

  rm(mask1, mask2, mask3)
  invisible(gc())

  sitsraster <- raster::raster(sitstif) %>% raster::raster()
  cat(paste0("Fasterize\n"))

  result <- fasterize::fasterize(mask, sitsraster)

  comparableDir <- normalizePath(baseDir("comparable"))

  cat(paste0("Saving data\n"))

  outputFile <- paste0(comparableDir, paste0("/prodesmask-", year, ".tif")) %>% normalizePath(mustWork = FALSE)

  rm(mask)
  rm(sitsraster)
  invisible(gc())
  s <- raster::raster(result)
  tr <- blockSize(result)
  s <- writeStart(s, filename=outputFile,  overwrite=TRUE)
  for (i in 1:tr$n) {
    cat(paste0("Writing block ", i, "/", tr$n, "\n"))

    v <- getValuesBlock(result, row=tr$row[i], nrows=tr$nrows[i])
    s <- writeValues(s, v, tr$row[i])
  }
  s <- writeStop(s)

  #raster::writeRaster(result, outputFile, overwrite = TRUE, dataType ="INT1U")
}
