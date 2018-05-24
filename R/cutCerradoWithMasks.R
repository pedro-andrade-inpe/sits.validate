
# This script creates cerrado data based on the SITS classifications,
# cutting the data with the cerrado polygon (from IBGE), hidrography
# polygons (from INPE), and urban areas (from ...)

require(sf)
require(raster)
require(rgeos)
require(dplyr)

###### DEFINE WHERE THE DATA IS ######

base_dir = "/Users/pedro/TWDTWAmazoniaCerrado/"

mypath = function(path) paste0(base_dir, path)

cerrado = sf::st_read(dsn = mypath("Shapefiles/Brasil"), layer = "br_biomes", quiet=TRUE) %>%
  filter(name == "Cerrado") %>%
  st_transform("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")
  

classificationFiles = Sys.glob(mypath("Classificacoes/ROLF/Cerrado_20180514/*"))


filename = classificationFiles[1]


hidro = raster::raster(mypath("/MapasReferencia/Hidrografia-TerraHidro/tiles_mosaic_resampled.tif")) %>% 
  raster::projectRaster(crs = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")


?raster::projectRaster()

# values:
# 1 = hidrography
# 6 = land



hidro


plot(hidro)                    


data = raster::raster(filename) %>% 
  raster::mask(hidro, maskvalue = 1)
  
  
  
  raster::mask(cerrado) %>% 
  
  writeRaster(paste0("/Users/pedro/cerrado/", basename(filename)))



plot(file, add=T)
plot(cerrado, add=T)



