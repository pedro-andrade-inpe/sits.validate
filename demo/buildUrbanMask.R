
require(sits.validate)

classificationFile <- list.files(baseDir("classificacoes-agregado"), full.names = TRUE)[1]

result <- raster::raster(classificationFile) %>% raster::raster() # empty raster
newraster <- raster::raster(result)
urbanFiles <- list.files(baseDir("urban/rasters"), "*.tif$", full.names = TRUE)

urbanFiles

for(urban in urbanFiles){
  cat(paste0("Processing '", basename(urban), "'\n"))
  myurban <- raster::raster(urban)

  result <- raster::resample(myurban, newraster, method = "ngb")
  outputname <- paste0(baseDir("urban/final"), "/", basename(urban))
  raster::writeRaster(result, outputname)
}
