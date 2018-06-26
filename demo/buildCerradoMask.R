
require(sits.validate)

result <- getTifFiles("classificacoes-agregado")[1] %>%
  getEmptyRaster()

newraster <- raster::raster(result)

raster::values(newraster) <- 1

crs_sits <- getSitsValidateEnv()$crs_sits

cerrado <- sf::read_sf(dsn = baseDir("shapes"), layer = "br_biomes") %>%
  dplyr::filter(name == "Cerrado") %>%
  sf::st_transform(crs_sits)

outputname <- baseDir("cerrado/cerradoMask.tif")

result <- raster::rasterize(cerrado, newraster, silent = FALSE, file = outputname, overwrite = TRUE)
