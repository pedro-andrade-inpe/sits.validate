
require("sits.validate")

# This script creates cerrado data based on the SITS classifications,
# cutting the data with the cerrado polygon (from IBGE), hidrography
# polygons (from ...), and urban areas (from ...)

waterMask <- function(myraster, year){
  cat("Applying water mask\n")
  water <- raster::raster(baseDir("masks/waterMask.tif"))

  raster::mask(myraster, water, maskvalue = 1, updatevalue = 16, progress = "text")
}

urbanMask <- function(myraster, year){
  cat("Applying urban mask\n")

  water <- paste0("masks/urban_areas_", year, ".tif") %>%
    baseDir %>%
    raster::raster()

  raster::mask(myraster, water, maskvalue = 1, updatevalue = 15, progress = "text")
}

sugarMask <- function(myraster, year){
  cat("Applying sugarcane mask\n")

  sugar <- paste0("masks/Canasat_", year, ".tif") %>%
    baseDir %>%
    raster::raster()

  raster::mask(myraster, sugar, maskvalue = 1, updatevalue = 14, progress = "text")
}

cerradoMask <- function(myraster, year){
  cat("Applying cerrado mask\n")

  cerrado <- raster::raster(baseDir("masks/cerradoMask.tif"))

  raster::mask(myraster, cerrado, progress = "text")
}

classificationFiles <- getTifFiles("classificacoes-agregado")

for(file in classificationFiles){
  year <- file %>%
    basename %>%
    substr(16, 19) %>%
    as.numeric() %>%
    (function(.) . - 1)

  cat(paste0("Processing year ", year, "\n"))

  outputfile <- baseDir(paste0("classificacoes-final/result-", basename(file)))

  file %>%
    raster::raster() %>%
    cerradoMask(year) %>%
    urbanMask(year) %>%
    sugarMask(year) %>%
    waterMask(year) %>%
    raster::writeRaster(outputfile, overwrite = TRUE)
}
