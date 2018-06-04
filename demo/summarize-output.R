

LUC2015 = raster::raster(mypath("RasterData/Luciara_MT/classificacoes/LUC-class-Cerrado_28022018_2014_8_2015_8.tif"))
ADN2015 = raster::raster(mypath("RasterData/AlvoradaDoNorte_GO/classificacoes/ADN-class-Cerrado_28022018_2014_8_2015_8.tif"))

result1 = summarizeOneByStates(LUC2015)
result2 = summarizeOneByStates(ADN2015)

simplifyOutput(result1 + result2)


simplifyOutput(result1)


result = summarizeOneByStates(LUC2015)
result = summarizeOneByMunicipalities(LUC2015)
result = summarizeOnrBySimU(LUC2015)

classificationFiles <- Sys.glob(mypath("Classificacoes/ROLF/Cerrado_20180514/*"))

crs_sits <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"

cerrado <- sf::st_read(dsn = mypath("Shapefiles/Brasil"), layer = "br_biomes", quiet=TRUE) %>%
    filter(name == "Cerrado") %>%
    st_transform(crs_sits)

process_file <- function(filename){
    summarizeOneBySimU(raster::raster(filename) %>% raster::mask(cerrado))
}

## use mclapply instead
result = lapply(classificationFiles[1:3], process_file)

## compute the sum of all results
mysum = Reduce("+", result)

simple = simplifyOutput(mysum)
dim(simple)
