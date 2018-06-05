
require(sits.validate)

LUC2015 <- raster::raster(basePath("RasterData/Luciara_MT/classificacoes/LUC-class-Cerrado_28022018_2014_8_2015_8.tif"))
ADN2015 <- raster::raster(basePath("RasterData/AlvoradaDoNorte_GO/classificacoes/ADN-class-Cerrado_28022018_2014_8_2015_8.tif"))

result1 <- summarizeOneByMunicipalities(LUC2015)
result2 <- summarizeOneByMunicipalities(ADN2015)

simplifyOutput(result1)
simplifyOutput(result1 + result2)
