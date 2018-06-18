
require(sits.validate)
require(dplyr)
require(raster)
require(gdalUtils)

waterFiles <- list.files(baseDir("/water/split"), "*.tif", full.names = TRUE)
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

######################################


cells <- mydim[1]

# Remove all values but 1 (which will be used as mask)
cleanValues <- function(x, filename) {
  out <- x
  bs <- raster::blockSize(out)
  out <- raster::writeStart(out, filename, overwrite=TRUE)
  for (i in 1:bs$n) {
    v <- raster::getValues(x, row=bs$row[i], nrows=bs$nrows[i] )
    cat(paste0("Line ", i, "/", bs$n, "\n"))
    v[v != 1] <- NA
    out <- raster::writeValues(out, v, bs$row[i])
  }
  out <- raster::writeStop(out)
  return(out)
}

outputFile <- paste0(outputDir, "/", basename(waterFiles[1]))
outputFile
cleanValues(myraster, outputFile)


###############################################################################


result <- raster::calc(myraster, filename = "result-one-class.tif", fun = function(x){
  cat(paste0(count * 100 / cells, "%\n"))
  count <<- count + length(x)
  x[x != 1] <- NA;

  return(x)
})




outputFile <- paste0(outputDir, "/", basename(file))


s <- f3(r, 5, filename='test')



dim(result)

raster::unique(result)

raster::writeRaster(result, "result-one-class.tif")

system.time(polygons <- raster::rasterToPolygons(result))

