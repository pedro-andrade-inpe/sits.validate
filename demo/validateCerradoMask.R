
require(sits.validate)

LUC2015 <- raster::raster(baseDir("RasterData/Luciara_MT/classificacoes/LUC-class-Cerrado_28022018_2014_8_2015_8.tif"))

result <- compareWithCerradoMask(LUC2015) # result in khectars
summarizeAsPercentage(result)
totalValidationCerradoMask(result)

# Try with other data

ADN2015 <- raster::raster(baseDir("RasterData/AlvoradaDoNorte_GO/classificacoes/ADN-class-Cerrado_28022018_2014_8_2015_8.tif"))
WAN2015 <- raster::raster(baseDir("RasterData/Wanderley_BA/classificacoes/WAN-class-filtered-svm_2014_8_2015_8.tif"))

##############################################################################


classifications = getTifFiles("classificacoes-final")


classification <- raster::raster(classifications[1])
result <- compareWithCerradoMask(classification) # result in khectars

