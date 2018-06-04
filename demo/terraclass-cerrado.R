
require("sits-validate")

ADN2013 = raster::raster(mypath("RasterData/AlvoradaDoNorte_GO/classificacoes/ADN-class-Cerrado_28022018_2012_8_2013_8.tif"))
result = compareTCCerrado(ADN2013)

simple = simplifyOutput(result)

simple/total*100

totalValidation(result)

LUC2013 = raster::raster(mypath("RasterData/Luciara_MT/classificacoes/LUC-class-Cerrado_28022018_2012_8_2013_8.tif"))
WAN2013 = raster::raster(mypath("RasterData/Wanderley_BA/classificacoes/WAN-class-filtered-svm_2012_8_2013_8.tif"))
