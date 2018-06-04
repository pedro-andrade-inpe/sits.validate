
require(sits.validate)

LUC2015 = raster::raster(mypath("RasterData/Luciara_MT/classificacoes/LUC-class-Cerrado_28022018_2014_8_2015_8.tif"))
ADN2015 = raster::raster(mypath("RasterData/AlvoradaDoNorte_GO/classificacoes/ADN-class-Cerrado_28022018_2014_8_2015_8.tif"))

result1 = summarizeOneByStates(LUC2015)
result2 = summarizeOneByStates(ADN2015)

simplifyOutput(result1 + result2)

simplifyOutput(result1)

result = summarizeOneByMunicipalities(LUC2015)

simplifyOutput(result)
