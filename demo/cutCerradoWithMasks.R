
require("sits-validate")

# This script creates cerrado data based on the SITS classifications,
# cutting the data with the cerrado polygon (from IBGE), hidrography
# polygons (from ...), and urban areas (from ...)

biomes = sf::st_read(dsn = mypath("Shapefiles/Brasil"), layer = "br_biomes", quiet=TRUE) %>%
  st_transform(crs_sits)

plot(st_geometry(biomes))
plot(st_geometry(urban), add=T)
plot(st_geometry(water), add=T, col="red")

cerrado <- sf::st_read(dsn = mypath("Shapefiles/Brasil"), layer = "br_biomes", quiet=TRUE) %>%
  filter(name == "Cerrado") %>%
  st_transform(crs_sits)


mydirectory = "/Users/pedro/cerrado"

water = sf::st_read(dsn = mydirectory, layer = "corpo_agua_2012", quiet = TRUE) %>%
  st_transform(crs_sits)


urban = sf::st_read(dsn = mydirectory, layer = "area_urb_2012", quiet = TRUE) %>%
  st_transform(crs_sits)



classificationFiles <- Sys.glob(mypath("Classificacoes/ROLF/Cerrado_20180514/*"))

detectCores()

classificationFiles

process_file <- function(filename)
  raster::raster(filename) %>%
  raster::mask(cerrado) %>%
  raster::mask(water) %>%
  raster::mask(urban) %>%
  raster::writeRaster(paste0(mydirectory, basename(filename)), overwrite=TRUE)

mclapply(classificationFiles, process_file, mc.cores = 3)
