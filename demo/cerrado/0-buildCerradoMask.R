
require(sits.validate)

classificationDir <- "cerrado-2019-08"

listBiomes <- function(){
  biomes <- sf::read_sf(dsn = baseDir("shapes"), layer = "br_biomes")
  return(biomes$name)
}

listBiomes()

newraster <- getTifFiles(classificationDir)[1] %>%
  getEmptyRaster()

raster::values(newraster) <- 1

crs_sits <- getSitsValidateEnv()$crs_sits

biomes <- sf::read_sf(dsn = baseDir("shapes"), layer = "br_biomes")

cerrado <- biomes %>%
  dplyr::filter(name == "Cerrado") %>%
  sf::st_transform(crs_sits)

outputname <- baseDir("masks/cerrado/cerradoMask.tif")

result <- raster::rasterize(cerrado, newraster, silent = FALSE, file = outputname, overwrite = TRUE)
