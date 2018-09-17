
require(sits.validate)
require(fasterize)

sitstif <- baseDir("classificacoes-final/result-cerrado_2012_8_2013_8.tif")

#mask1 <- sf::read_sf(dsn = baseDir("cerrado/prodesMask"), layer = "MASC_CERR_2000_2015_lote_final_cor_pol")
#mask2 <- sf::read_sf(dsn = baseDir("cerrado/prodesMask"), layer = "MASC_CERR_2000_2015_lote_final_cor_pol_split1")
#mask3 <- sf::read_sf(dsn = baseDir("cerrado/prodesMask"), layer = "MASC_CERR_2000_2015_lote_final_cor_pol_split2")

# data was reprojected
mask1 <- sf::read_sf(dsn = baseDir("cerrado/prodesMask/reproj"), layer = "mask1")
mask2 <- sf::read_sf(dsn = baseDir("cerrado/prodesMask/reproj"), layer = "mask2")
mask3 <- sf::read_sf(dsn = baseDir("cerrado/prodesMask/reproj"), layer = "mask3")

# removing water polygons
mask1 <- mask1[mask1$class_name %in% paste0("D_", 2000:2015),]
mask2 <- mask2[mask2$class_name %in% paste0("D_", 2000:2015),]
mask3 <- mask3[mask3$class_name %in% paste0("D_", 2000:2015),]

mask <- rbind(mask1, mask2, mask3)

rm(mask1, mask2, mask3)
gc()

sitsraster <- raster::raster(sitstif) %>% raster::raster()
result <- fasterize::fasterize(mask, sitsraster)

comparableDir <- normalizePath(baseDir("comparable"))

outputFile <- paste0(comparableDir, "/prodesmask-2015.tif") %>% normalizePath(mustWork = FALSE)

raster::writeRaster(result, outputFile, overwrite = TRUE)
