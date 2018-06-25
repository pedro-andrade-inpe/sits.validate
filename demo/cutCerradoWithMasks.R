
require("sits.validate")
require(sf)

# This script creates cerrado data based on the SITS classifications,
# cutting the data with the cerrado polygon (from IBGE), hidrography
# polygons (from ...), and urban areas (from ...)

waterMask <- function(myraster, year){
  cat("Applying water mask\n")
  water <- raster::raster(baseDir("water/final/waterMask.tif"))

  raster::mask(myraster, water, maskvalue = 1, updatevalue = 16, progress = "text")
}

urbanMask <- function(myraster, year){
  cat("Applying urban mask\n")

  water <- paste0("urban/final/urban_areas_", year, ".tif") %>%
    baseDir %>%
    raster::raster()

  raster::mask(myraster, water, maskvalue = 1, updatevalue = 15, progress = "text")
}

cerradoMask <- function(myraster, year){
  cat("Applying cerrado mask\n")

  cerrado <- raster::raster(baseDir("cerrado/cerradoMask.tif"))

  raster::mask(myraster, cerrado, progress = "text")
}

classificationFiles <- list.files(baseDir("classificacoes-agregado"), full.names = TRUE)

for(file in classificationFiles){
  year <- file %>%
    basename %>%
    substr(16, 19) %>%
    as.numeric() %>%
    (function(.) . - 1)

  cat(paste0("Processing year ", year, "\n"))

  file %>%
    raster::raster() %>%
    waterMask(year) %>%
    cerradoMask(year) %>%
    urbanMask(year) %>%
    raster::writeRaster(paste0("result-", basename(file)), overwrite=TRUE)
}
