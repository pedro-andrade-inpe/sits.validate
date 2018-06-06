require(sits.validate)

classificationFiles <- Sys.glob("~/TWDTWAmazoniaCerrado/Classificacoes/ROLF/Cerrado_20180514/*")

crs_sits <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"

cerrado <- sf::read_sf(dsn = basePath("shapes"), layer = "br_biomes") %>%
  dplyr::filter(name == "Cerrado") %>%
  sf::st_sf() %>%
  sf::st_transform(crs_sits)

process_file <- function(filename){
  summarizeOneBySimU(raster::raster(filename) %>% raster::mask(cerrado))
}

## use mclapply instead
result <- lapply(classificationFiles[2:3], process_file)

result[[1]]
result[[2]]

output <- joinClassifications(result)
output

## TODO: join the clases according to needed in globiom

## TODO: save to csv file
