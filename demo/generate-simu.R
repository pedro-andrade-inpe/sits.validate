require(sits.validate)

classificationFiles <- Sys.glob(mypath("Classificacoes/ROLF/Cerrado_20180514/*"))

crs_sits <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"

cerrado <- sf::st_read(dsn = mypath("Shapefiles/Brasil"), layer = "br_biomes", quiet=TRUE) %>%
    filter(name == "Cerrado") %>%
    sf::st_sf() %>%
    sf::st_transform(crs_sits)

process_file <- function(filename){
    summarizeOneBySimU(raster::raster(filename) %>% raster::mask(cerrado))
}

## use mclapply instead
result = lapply(classificationFiles[1:3], process_file)

## compute the sum of all results
mysum = Reduce("+", result)

simple = simplifyOutput(mysum)
dim(simple)
