
require("sits.validate")
require(dplyr)

# This script creates cerrado data based on the SITS classifications,
# cutting the data with the cerrado polygon (from IBGE), hidrography
# polygons (from ...), and urban areas (from ...)

mydirectory <- "/Users/pedro/cerrado"
crs_sits <- getSitsValidateEnv()$crs_sits

#################################################################################
# See that the current data is only for mato grosso state
biomes <- sf::read_sf(dsn = baseDir("Shapefiles/Brasil"), layer = "br_biomes") %>%
  st_transform(crs_sits)

plot(st_geometry(biomes))
plot(st_geometry(urban), add=T)
plot(st_geometry(water), add=T, col="red")
##################################################################################


cerrado <- sf::read_sf(dsn = baseDir("shapes"), layer = "br_biomes") %>%
  filter(name == "Cerrado") %>%
  sf::st_transform(crs_sits)

classificationFiles <- list.files(baseDir("classificacoes"), full.names = TRUE)

classificationFiles

detectCores()

process_file <- function(filename)
  raster::raster(filename) %>%
  raster::mask(cerrado) %>%
  raster::waterMask() %>%
#  raster::mask(urban) %>%
  raster::writeRaster(paste0(mydirectory, basename(filename)), overwrite=TRUE)

mclapply(classificationFiles, process_file, mc.cores = 2)

myraster<-raster::raster(classificationFiles[1])

waterMask <- function(raster){
  waterFiles <- list.files(baseDir("water"), "*.tif", full.names = TRUE)

  for(file in waterFiles){
    print(file)
    water <- raster::raster(file)
    if(raster::intersect(raster::extent(raster), raster::extent(water)))
      raster <- raster::mask(raster, water, maskvalue = 1, updatevalue = 16)
  }

  return(raster)
}

result <- waterMask(myraster)

water <- raster::raster(waterFiles[1])

water <- raster::raster(water)

raster::intersect(raster::extent(water), raster::extent(myraster))

water
myraster

class(result)

