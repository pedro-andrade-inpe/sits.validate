
#' @title Reclassify pixels using the first level of the legend
#' @description Reclassify each pixel of a image using the first
#' level of the legend, which are the values less than ten.
#' @param file A tif file.
#' @param legend A tibble representing a legend.
#' @export
groupRasterToLevel1 <- function(file, legend){
  values <- legend$Value
  highestLevel <- values %>%
    paste() %>%
    substr(1, 1) %>%
    as.numeric()

  replacements <- rep(0, max(values))
  replacements[values] <- highestLevel

  result <- sits.validate:::applyFunctionByBlocks(file, function(block) replacements[block])
}

#' @title Reclassify pixels using the first level of the legend
#' @description Reclassify each pixel of each image of a directory using the first
#' level of the legend, which are the values less than ten. The
#' output images are saved in the output directory.
#' @param inputDir Input directory. It must be a relative path within baseDir().
#' @param outputDir Output directory. It will be cleaned before processing
#' the files. It must also be a relative path within baseDir().
#' @param legend A legend, that can be created with readLegend().
#' @export
groupDirectoryToLevel1 <- function(inputDir, outputDir, legend){
  files <- getTifFiles(inputDir)
  createEmptyDir(outputDir)

  currentDirectoy <- getwd()
  setwd(baseDir(outputDir))

  for(file in files){
    cat(paste0("Processing ", file, "\n"))
    groupRasterToLevel1(file, legend) %>%
      raster::writeRaster(basename(file))
  }

  subLegend(legend, basename(files[1])) %>%
    buildStyle("style.qml")

  setwd(currentDirectoy)
}
