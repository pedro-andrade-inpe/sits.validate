
require(sits.validate)
require(dplyr)
require(raster)
require(gdalUtils)

# To reproduce everything from scratch, first download water data using
# baseDir("hansen/download-hansen.sh").

# this script supposes only 2000 data was downloaded. The script above downloads ALL
# data (2000, deforestation until 2014 and gain until 2012)

# split water files into smaller ones and then run
splitRasters("hansen", "hansen/split", 4)

# reproject the data to the same projection used by sits, and finally
# run this script.

hansenFiles <- getTifFiles("hansen/split")
crs_sits <- getSitsValidateEnv()$crs_sits
outputDir <- baseDir("hansen/reproject")

# Takes too long
# raster::projectRaster(myraster, filename = outputFile, crs = crs_sits, method = "ngb", progress = "text")

for(file in hansenFiles){
  name <- basename(file)
  outputFile <- paste0(outputDir, "/", name)
  cat(paste0("Processing '", name, "'\n"))
  gdalwarp(file, outputFile, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84", crs_sits, r = "near")
}

hansenFiles <- getTifFiles("hansen/reproject")
filename <- getTifFiles("classificacoes-agregado")[1]

myraster <- raster::raster(filename)
newraster <- raster::raster(myraster)
result <- raster::raster(myraster)

for(hansen in hansenFiles){
  myhansen <- raster::raster(hansen)
  inter <- raster::intersect(raster::extent(newraster), raster::extent(myhansen))

  if(is.null(inter))
    cat(paste0("Skipping '", basename(hansen), "'\n"))
  else{
    cat(paste0("Resampling '", basename(hansen), "'\n"))
    result <- raster::resample(myhansen, newraster, method = "ngb") %>%
      raster::merge(result, progress = "text")
  }
}

comparableDir <- normalizePath(baseDir("comparable"))

outputFile <- paste0(comparableDir, "/hansen-2000.tif") %>% normalizePath(mustWork = FALSE)

raster::writeRaster(result, outputFile, overwrite = TRUE)
