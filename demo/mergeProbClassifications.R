
# merge all probabilities tif files for a given year into a single tif file


require(sits.validate)

outputDir <- baseDir("probs-agregado")
directory <- "C:/Users/pedro/Dropbox/pesquisa/2015/TWDTWAmazoniaCerrado/Classificacoes/Cerrado-13classes-DL"

classificationFiles <- list.files(directory)
classificationFiles <- grep("probs", classificationFiles, value = TRUE)
classificationFiles <- classificationFiles[endsWith(classificationFiles, "tif")]

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
      raster::stack()
  })

  outputFile <- paste0(outputDir, "/cerrado_", year, ".tif") %>%
    normalizePath(mustWork = FALSE)

  cat(paste0("Creating '", outputFile, "'\n"))
  rasters$filename <- outputFile


  do.call(raster::merge, rasters)
}

