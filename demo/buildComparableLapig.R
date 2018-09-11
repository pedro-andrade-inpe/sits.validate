
require(sits.validate)

sitstif <- baseDir("classificacoes-final/result-cerrado_2012_8_2013_8.tif")

splitRaster(sitstif, "lapig/split", 8) # split into 64 tif files

files <- list.files(splitDir)

lapigtif <- baseDir("lapig/pa_br_rf_pastagens_30_2015_lapig.tif")
lapig <- raster::raster(lapigtif)

resultDir <- normalizePath(baseDir("lapig/result"))

for(file in files){
  cat(paste0("Processing ", file, "\n"))
  myraster <- raster::raster(paste0(splitDir, "/", file)) %>% raster::raster()

  result <- raster::projectRaster(lapig, myraster, method = "ngb",
                                  file = paste0(resultDir, "/", file), overwrite = TRUE)
}


# join the results
rasters <- lapply(files, function(file){
  cat(paste0("Reading file '", file, "'\n"))
  paste0(resultDir, "/", file) %>% raster::raster()
})

comparableDir <- normalizePath(baseDir("lapig/comparable"))

outputFile <- paste0(comparableDir, "/lapig_2015.tif") %>% normalizePath(mustWork = FALSE)

cat(paste0("Creating '", outputFile, "'\n"))
rasters$filename <- outputFile

do.call(raster::merge, rasters)

cerrado <- raster::raster(baseDir("cerrado/cerradoMask.tif"))
myraster <- raster::raster(outputFile)

result <- raster::mask(myraster, cerrado, progress = "text")
raster::writeRaster(result, outputFile, overwrite = TRUE)

