
require(sits.validate)

classificationFile <- list.files(baseDir("classificacoes-agregado"), full.names = TRUE)[1]

result <- raster::raster(classificationFile) %>% raster::raster() # empty raster
newraster <- raster::raster(result)
waterFiles <- list.files(baseDir("water/reproject"), "*.tif", full.names = TRUE)


for(water in waterFiles){
  cat(paste0("Processing '", basename(water), "'\n"))
  mywater <- raster::raster(water)
  inter <- raster::intersect(raster::extent(newraster), raster::extent(mywater))

  if(!is.null(inter)){
    cat("Resampling as it has intersection\n")
    result <- raster::resample(mywater, newraster, method = "ngb") %>%
      raster::merge(result, progress = "text")
#    outputname <- paste0(baseDir("water/agregado"), "/", basename(water))
#    raster::writeRaster(result, outputname)
  }

  rm(inter)
  rm(mywater)
  gc()
}

outputname <- baseDir("water/agregado/waterMask.tif")
raster::writeRaster(result, outputname)
