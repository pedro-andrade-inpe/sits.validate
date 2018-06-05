
require("sits.validate")

# This script creates cerrado data based on the SITS classifications,
# cutting the data with the cerrado polygon (from IBGE), hidrography
# polygons (from ...), and urban areas (from ...)

mydirectory <- "/Users/pedro/cerrado"

water <- sf::read_sf(dsn = mydirectory, layer = "corpo_agua_2012") %>%
  st_transform(crs_sits)

urban <- sf::read_sf(dsn = mydirectory, layer = "area_urb_2012") %>%
  st_transform(crs_sits)

#################################################################################
# See that the current data is only for mato grosso state
biomes <- sf::read_sf(dsn = basePath("Shapefiles/Brasil"), layer = "br_biomes") %>%
  st_transform(crs_sits)

plot(st_geometry(biomes))
plot(st_geometry(urban), add=T)
plot(st_geometry(water), add=T, col="red")
##################################################################################

cerrado <- sf::read_sf(dsn = basePath("Shapefiles/Brasil"), layer = "br_biomes") %>%
  filter(name == "Cerrado") %>%
  st_transform(crs_sits)

classificationFiles <- Sys.glob(basePath("Classificacoes/ROLF/Cerrado_20180514/*"))

detectCores()

process_file <- function(filename)
  raster::raster(filename) %>%
  raster::mask(cerrado) %>%
#  raster::mask(water) %>%
#  raster::mask(urban) %>%
  raster::writeRaster(paste0(mydirectory, basename(filename)), overwrite=TRUE)

mclapply(classificationFiles, process_file, mc.cores = 2)
