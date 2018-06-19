
require("sits.validate")

classificationsDir = baseDir("")

ADN2013 <- raster::raster(paste0(classificationsDir, "AlvoradaDoNorte_GO/classificacoes/ADN-class-Cerrado_28022018_2012_8_2013_8.tif"))

result <- compareTCCerrado(ADN2013)
result

# You can try with other classifications

LUC2013 <- raster::raster(paste0(classificationsDir, "Luciara_MT/classificacoes/LUC-class-Cerrado_28022018_2012_8_2013_8.tif"))

result <- compareTCCerrado(LUC2013)
result

WAN2013 <- raster::raster(paste0(classificationsDir, "Wanderley_BA/classificacoes/WAN-class-filtered-svm_2012_8_2013_8.tif"))
