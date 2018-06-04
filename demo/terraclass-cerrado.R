
require("sits.validate")

ADN2013 = raster::raster(mypath("RasterData/AlvoradaDoNorte_GO/classificacoes/ADN-class-Cerrado_28022018_2012_8_2013_8.tif"))
result = compareTCCerrado(ADN2013)

total = sum(result)
simple = simplifyOutput(result)

simple/total*100

totalValidationTCCerrado(result)

# You can try with other classifications

LUC2013 = raster::raster(mypath("RasterData/Luciara_MT/classificacoes/LUC-class-Cerrado_28022018_2012_8_2013_8.tif"))
WAN2013 = raster::raster(mypath("RasterData/Wanderley_BA/classificacoes/WAN-class-filtered-svm_2012_8_2013_8.tif"))
