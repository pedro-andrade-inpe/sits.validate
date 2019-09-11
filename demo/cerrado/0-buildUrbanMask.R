require(sits.validate)

classificationDir <- "cerrado-2019-08"

newraster <- getTifFiles(classificationDir)[1] %>%
  getEmptyRaster

urbanFiles <- getTifFiles("urban/rasters")

for(urban in urbanFiles){
  cat(paste0("Processing '", basename(urban), "'\n"))
  myurban <- raster::raster(urban)

  outputname <- baseDir(paste0("masks/cerrado/", basename(urban)))

  raster::resample(myurban, newraster, method = "ngb", filename = outputname, overwrite = TRUE)
}

# copy 2013 to 2014 and 2015 as there is no available data for such years
inputname <- baseDir("masks/urban_areas_2013.tif")

for(year in c(2014, 2015)){
  outputname <- baseDir(paste0("masks/cerrado/urban_areas_", year, ".tif"))

  cat(paste0("Copying '", basename(outputname), "'\n"))

  file.copy(inputname, outputname, overwrite = TRUE)
}
