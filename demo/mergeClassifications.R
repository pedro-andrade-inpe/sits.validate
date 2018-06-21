
require(sits.validate)

outputDir <- baseDir("classificacoes-agregado")
directory <- "classificacoes"
classificationFiles <- getTifFiles(directory)

scenes <- classificationFiles %>%
  basename %>%
  substr(1, 6) %>%
  unique

middle <- classificationFiles %>%
  basename %>%
  substr(7, 23) %>%
  unique

years <- classificationFiles %>%
  basename %>%
  substr(24, 36) %>%
  unique

for(year in years){
  cat(paste0("Processing year ", year, "\n"))

  rasters <- lapply(scenes, function(scene){
    cat(paste0("Reading scene '", scene, "'\n"))
    paste0(directory, "/", scene, middle, year, ".tif") %>%
      baseDir %>%
      raster::raster()
  })

  outputFile <- paste0(outputDir, "/cerrado_", year, ".tif") %>%
    normalizePath(mustWork = FALSE)

  cat(paste0("Creating '", outputFile, "'\n"))
  rasters$filename <- outputFile

  do.call(raster::merge, rasters)
}
