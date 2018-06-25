
require(sits.validate)

classificationFile <- list.files(baseDir("classificacoes-agregado"), full.names = TRUE)[1]

result <- raster::raster(classificationFile) %>% raster::raster() # empty raster
newraster <- raster::raster(result)

raster::values(newraster) <- 1

crs_sits <- getSitsValidateEnv()$crs_sits

cerrado <- sf::read_sf(dsn = baseDir("shapes"), layer = "br_biomes") %>%
  dplyr::filter(name == "Cerrado") %>%
  sf::st_transform(crs_sits)

result <- raster::rasterize(cerrado, newraster, silent = FALSE)

outputname <- paste0(baseDir("cerrado"), "/cerradoMask.tif")

raster::writeRaster(result, outputname)
