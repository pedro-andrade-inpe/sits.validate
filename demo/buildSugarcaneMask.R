
require(sits.validate)
require(fasterize)

sitstif <- baseDir("classificacoes-final/result-cerrado_2012_8_2013_8.tif")

crs_sits <- getSitsValidateEnv()$crs_sits

files <- list.files(baseDir("sugarcane"), ".shp$") %>%
  tools::file_path_sans_ext()

for(file in files){
  cat(paste0("Processing '", file, "'\n"))

  mask <- sf::read_sf(dsn = baseDir("sugarcane"), layer = file) %>%
    sf::st_transform(crs_sits)

  sitsraster <- raster::raster(sitstif) %>% raster::raster()

  result <- fasterize::fasterize(mask, sitsraster)

  outputFile <- normalizePath(baseDir("masks")) %>%
    paste0("/", file, ".tif") %>%
    normalizePath(mustWork = FALSE)

  raster::writeRaster(result, outputFile, overwrite = TRUE)
}

masks <- baseDir("masks")

file.rename(paste0(masks, "/Canasat_03.tif"), paste0(masks, "/Canasat_2003.tif"))
file.rename(paste0(masks, "/Canasat_04.tif"), paste0(masks, "/Canasat_2004.tif"))

file.copy(paste0(masks, "/Canasat_2003.tif"), paste0(masks, "/Canasat_2000.tif"))
file.copy(paste0(masks, "/Canasat_2003.tif"), paste0(masks, "/Canasat_2001.tif"))
file.copy(paste0(masks, "/Canasat_2003.tif"), paste0(masks, "/Canasat_2002.tif"))
