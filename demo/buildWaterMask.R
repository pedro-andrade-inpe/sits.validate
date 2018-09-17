
require(sits.validate)
require(dplyr)
require(gdalUtils)

# To reproduce everything from scratch, first download water data using
# baseDir("water/download-water.sh").

# 1st: split water files into smaller ones

outputDir <- normalizePath(baseDir("water/split"))
inputDir <- baseDir("water")
splitRasters(inputDir, outputDir, 4)

# 2nd: seproject the data to the same projection used by sits,

waterFiles <- getTifFiles("/water/split")
crs_sits <- getSitsValidateEnv()$crs_sits
outputDir <- baseDir("/water/reproject")

# Takes too long
# raster::projectRaster(myraster, filename = outputFile, crs = crs_sits, method = "ngb", progress = "text")

for(file in waterFiles){
  name <- basename(file)
  outputFile <- paste0(outputDir, "/", name)
  cat(paste0("Processing '", name, "'\n"))
  gdalwarp(file, outputFile, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", crs_sits, r = "near")
}

# 3rd: Merge water

classificationFile <- getTifFiles("classificacoes-agregado")[1]

result <- raster::raster(classificationFile) %>% raster::raster() # empty raster
newraster <- raster::raster(result)
waterFiles <- getTifFiles("water/reproject")

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

outputname <- baseDir("masks/waterMask.tif")
raster::writeRaster(result, outputname)
