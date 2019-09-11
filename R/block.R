#' @title Creates a vector of strings from variable names to represent land use and cover classes
#' @description Convert a set of variable names in a vector of strings with their names.
#' This function is useful to name the attributes to be read from CSV files.
#' @param ... Names of the variables.
#' @export
landClasses <-function(...) {
  paste(substitute(list(...)))[-1]
}

#' @title Apply a function over all pixels of a image by block
#' @description Generate a raster from another raster an a function that
#' takes block (a vector of pixels) as argument and returns a new raster
#' in memory.
#' @param file A tiff file.
#' @param block_func A function that takes a vector of pixels as argument
#' and returns another vector of pixels with the same size of the input.
#' All but the last call use vectors with the same size. The last one
#' uses the remaining pixels of the raster.
#' @export
applyFunctionByBlocks <- function(file, block_func){
  myraster <- raster::raster(file)
  result <- raster::raster(myraster)

  bs <- raster::blockSize(myraster)

  for(i in 1:bs$n){
    cat(paste0("Processing block ", i, "/", bs$n, "\n"))

    row = bs$row[i]
    nrows = bs$nrows[i]

    result[row:(row + nrows - 1),] <-
      raster::getValues(myraster, row = row, nrows = nrows) %>%
      block_func()
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
#' @param replacements A list whose indexes belong to Short values of the legend,
#' and whose values are the pixels of the images.
#' @export
remapRaster <- function(file, replacements){
  applyFunctionByBlocks(file, function(block) replacements[block])
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
#' @param replacements A list whose indexes belong to Short values of the legend,
#' and whose values are the pixels of the images.
#' @details This function also creates a qml file with the colors
#' of the pixels.
#' @seealso remapRaster()
#' @export
remapDirectory <- function(inputDir, outputDir, legend, replacements){
  ok = replacements %in% legend$Short

  if(!all(ok)){
    missing = replacements[!ok]

    stop(paste0("The following classes are missing in the legend: ",
                paste(missing, collapse = ", "), "."))
  }

  positions <- match(replacements, legend$Short)
  values <- legend$Value[positions]

  processDirectory(inputDir, outputDir, function(file)
    remapRaster(file, values)
  )
}
