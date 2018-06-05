require(sits.validate)
require(dplyr)

classificationFiles <- Sys.glob(basePath("Classificacoes/ROLF/Cerrado_20180514/*"))

crs_sits <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"

cerrado <- sf::read_sf(dsn = basePath("Shapefiles/Brasil"), layer = "br_biomes") %>%
    dplyr::filter(name == "Cerrado") %>%
    sf::st_sf() %>%
    sf::st_transform(crs_sits)

process_file <- function(filename){
    summarizeOneBySimU(raster::raster(filename) %>% raster::mask(cerrado))
}

## use mclapply instead
result <- lapply(classificationFiles[1:2], process_file)

## compute the sum of all results
mysum <- Reduce("+", result)

simple <- simplifyOutput(mysum)
simple
dim(simple)

## TODO: save to csv file



