
require("sits-validate")

LUC2015 = raster::raster(mypath("RasterData/Luciara_MT/classificacoes/LUC-class-Cerrado_28022018_2014_8_2015_8.tif"))

result = compareWithCerradoMask(LUC2015) # result in khectars
summarizeAsPercentage(result)
totalValidation(result)

ADN2015 = raster::raster(mypath("RasterData/AlvoradaDoNorte_GO/classificacoes/ADN-class-Cerrado_28022018_2014_8_2015_8.tif"))

result = compareWithCerradoMask(ADN2015) # result in khectars

WAN2015 = raster::raster(mypath("RasterData/Wanderley_BA/classificacoes/WAN-class-filtered-svm_2014_8_2015_8.tif"))
