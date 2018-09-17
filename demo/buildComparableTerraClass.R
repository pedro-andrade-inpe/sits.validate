
require(sits.validate)

sitstif <- baseDir("classificacoes-final/result-cerrado_2012_8_2013_8.tif")
splitDir <- normalizePath(baseDir("cerrado/terraClass/split"))

splitRaster(sitstif, splitDir, 8) # split into 64 tif files

files <- list.files(splitDir)

tctif <- baseDir("cerrado/terraClass/TCCerrado_2013_geo.tif")
tccerrado <- raster::raster(tctif)

resultDir <- normalizePath(baseDir("cerrado/terraClass/result"))

for(file in files){
  cat(paste0("Processing ", file, "\n"))
  myraster <- raster::raster(paste0(splitDir, "/", file)) %>% raster::raster()

  result <- raster::projectRaster(tccerrado, myraster, method = "ngb",
    file = paste0(resultDir, "/", file), overwrite = TRUE)
}

# join the results
rasters <- lapply(files, function(file){
  cat(paste0("Reading file '", file, "'\n"))
  paste0(resultDir, "/", file) %>% raster::raster()
})

comparableDir <- normalizePath(baseDir("comparable"))

outputFile <- paste0(comparableDir, "/terraclass-2013.tif") %>% normalizePath(mustWork = FALSE)

cat(paste0("Creating '", outputFile, "'\n"))
rasters$filename <- outputFile

do.call(raster::merge, rasters)
