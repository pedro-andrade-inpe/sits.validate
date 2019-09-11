
#' @title Reclassify pixels using the first level of the legend
#' @description Reclassify each pixel of a image using the first
#' level of the legend, which are the values less than ten.
#' @param file A tif file.
#' @param legend A tibble representing a legend.
#' @export
groupRasterToLevel1 <- function(file, legend){
  original_values <- legend$Value

  highest_levels <- legend %>%
    dplyr::filter(Parent == Value) %>%
    dplyr::pull(Value) %>%
    length()

  while(highest_levels != length(unique(legend$Parent))){
    values <- legend$Value
    parents <- legend$Parent

    new_parents <- parents[match(parents, values)]

    legend$Value <- legend$Parent
    legend$Parent <- new_parents
  }

  #legend$Value <- original_values

  result <- sits.validate:::applyFunctionByBlocks(file, function(block) legend$Parent[block])
}

groupRasterToClasses <- function(file, leged, classes){

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
  processDirectory(inputDir, outputDir, function(file)
    groupRasterToLevel1(file, legend)
  )
}

