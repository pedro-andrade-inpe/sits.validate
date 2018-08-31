
require(sits.validate)

sitstif <- baseDir("classificacoes-final/result-cerrado_2012_8_2013_8.tif")

# data must be reprojected prevously as it takes too much time in R
#mask1 <- sf::read_sf(dsn = baseDir("cerrado/prodesMask"), layer = "MASC_CERR_2000_2015_lote_final_cor_pol")
#mask2 <- sf::read_sf(dsn = baseDir("cerrado/prodesMask"), layer = "MASC_CERR_2000_2015_lote_final_cor_pol_split1")
#mask3 <- sf::read_sf(dsn = baseDir("cerrado/prodesMask"), layer = "MASC_CERR_2000_2015_lote_final_cor_pol_split2")

mask1 <- sf::read_sf(dsn = baseDir("cerrado/prodesMask/reproj"), layer = "mask1")
mask2 <- sf::read_sf(dsn = baseDir("cerrado/prodesMask/reproj"), layer = "mask2")
mask3 <- sf::read_sf(dsn = baseDir("cerrado/prodesMask/reproj"), layer = "mask3")

mask <- rbind(mask1, mask2, mask3)

rm(mask1)
rm(mask2)
rm(mask3)
gc()

## TODO: Remove WATER mask!!

sitsraster <- raster::raster(sitstif) %>% raster::raster()
result <- fasterize::fasterize(mask, sitsraster)

raster::writeRaster(result, "myresult.tif", overwrite = TRUE)
