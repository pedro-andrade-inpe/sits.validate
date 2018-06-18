
require(sits.validate)
require(raster)

# To reproduce everything from scratch, first download water data using
# baseDir("water/download-water.sh") then run 'splitWater.R' to split
# water files into smaller ones and then run 'reprojectWater.R' to
# reproject the data to the same projection used by sits, and finally
# run this script.

# get files with unique six first characters in their names
filesWithUniqueBox <- function(directory){
  classificationFiles <- getTifFiles(directory)

  scenes <- classificationFiles %>%
    basename %>%
    substr(1, 6) %>%
    duplicated

  classificationFiles[!scenes]
}

generateWaterMask <- function(filename){
  waterFiles <- getTifFiles("water/reproject")

  myraster <- raster::raster(filename)
  newraster <- raster::raster(myraster)
  result <- raster::raster(myraster)

  for (water in waterFiles){
    mywater <- raster::raster(water)
    inter <- raster::intersect(raster::extent(newraster), raster::extent(mywater))

    if(!is.null(inter)){
      cat(paste0("Resampling '", basename(water), "'\n"))
      result <- raster::resample(mywater, newraster, method = "ngb") %>%
        raster::merge(result, progress = "text")
    }
  }

  outputname <- baseDir(paste0("water/final/", basename(filename))) %>% substr(1, 6), ".tif")
  raster::writeRaster(result, outputname)
  return(invisible())
}

files <- filesWithUniqueBox("classificacoes")


files <- files[-1]


for(file in files){
  cat(paste0("Processing '", basename(file), "'\n"))

  generateWaterMask(file)
}
