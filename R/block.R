#' @title Creates a vector of strings from variable names to represent land use and cover classes
#' @description Convert a set of class names in a vector with its code.
#' @param legend A tibble legend.
#' @param ... Names of the variables.
#' @export
landClasses <- function(legend, ...) {

  replacements <- paste(substitute(list(...)))[-1]

  ok = replacements %in% legend$Short

  if (!all(ok)) {
    missing = replacements[!ok]

    stop(paste0("The following classes are missing in the legend: ",
                paste(missing, collapse = ", "), "."))
  }

  positions <- match(replacements, legend$Short)

  return(legend$Value[positions])
}

#' @title Apply a function over all pixels of a image by block
#' @description Generate a raster from another raster an a function that
#' takes block (a vector of pixels) as argument and returns a new raster
#' in memory.
#' @param raster A raster object.
#' @param block_func A function that takes a vector of pixels as argument
#' and returns another vector of pixels with the same size of the input.
#' All but the last call use vectors with the same size. The last one
#' uses the remaining pixels of the raster.
#' @param ... Any other parameters to pass to block_func.
#' @export
applyFunctionByBlocks <- function(raster, block_func, ...){

  result <- raster::raster(raster)

  bs <- raster::blockSize(raster)

  for (i in 1:bs$n) {
    cat(paste0("Processing block ", i, "/", bs$n, "\n"))

    row = bs$row[i]
    nrows = bs$nrows[i]

    result[row:(row + nrows - 1),] <-
      raster::getValues(raster, row = row, nrows = nrows) %>%
      block_func(...)
  }

  return(result)
}

#' @title Remap pixel values of an image
#' @description Replace
#' each pixel of an image file by a given set of values.
#' Every value in the original image must be mapped, even
#' if the updated value is the same. It returns a raster in memory
#' with the updated values.
#' @param file A tiff file.
#' @param replacements A vector with the code of classes to be remapped according
#' to its position.
#' @export
remapRaster <- function(file, replacements) {

  replacements <- c(0, replacements)
  names(replacements) <- as.character(0:(length(replacements) - 1))

  in_raster <- raster::raster(file)
  res <- raster::raster(in_raster)
  res[] <- replacements[as.character(in_raster[])]
  return(res)
}

#' @title Remap pixel values of all images within a directory
#' @description create a directory with the classifications, replacing
#' each pixel of each image by a given set of values.
#' Every value in the original image must be mapped, even
#' if the updated value is the same.
#' @param inputDir Input directory. It must be a relative path within baseDir().
#' @param outputDir Output directory. It will be cleaned before processing
#' the files. It must also be a relative path within baseDir().
#' @param legend A legend, that can be created with readLegend().
#' @param replacements A vector with the code of classes to be remapped according
#' to its position.
#' @param overwrite Should output files be overwritten?
#' @details This function also creates a qml file with the colors
#' of the pixels.
#' @seealso remapRaster()
#' @export
remapDirectory <- function(inputDir, outputDir, replacements, overwrite = FALSE) {

  processDirectory(inputDir, outputDir,
                   function(file) remapRaster(file = file, replacements = replacements),
                   overwrite = overwrite)

  cat("Writing style file\n")
  subLegend(legend, replacements) %>%
    buildStyle("style.qml")

  invisible(NULL)
}
