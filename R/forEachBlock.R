
#' @title Iterates over a raster by block
#' @description Iterates over a raster using a second order function.
#' @param raster A raster::raster.
#' @param func A function that takes three arguments: (1) A vector of pixels
#' representing a block, (2) The row of the raster where the block starts, and
#' (3) The number of rows within the block.
#' @export
forEachBlock <- function(raster, func){
  bs <- raster::blockSize(raster)

  total <- bs$n
  pb <- utils::txtProgressBar(min = 0, max = total, style = 3)
  for(i in 1:total){
    utils::setTxtProgressBar(pb, i)
    row <- bs$row[i]
    nrows <- bs$nrows[i]

    block <- raster::getValues(raster, row = row, nrows = nrows)

    func(block, row, nrows)
  }

  close(pb)
}

#' @title Iterates over two rasters by blocks
#' @description Iterates over two rasters using a second order function. It
#' supposes both rasters have the same extent and resolution.
#' @param raster1 A raster::raster.
#' @param raster2 Another raster::raster.
#' @param func A function that takes two arguments: (1) A vector of pixels
#' representing a block from raster1 and (2) A vector of pixels
#' representing a block from raster2. Both vectors have the same size.
#' @export
forEachBlockPair <- function(raster1, raster2, func){
  bs <- raster::blockSize(raster1)

  for(i in 1:bs$n){
    cat(paste0("Processing block ", i, "/", bs$n, "\n"))
    row <- bs$row[i]
    nrows <- bs$nrows[i]

    block1 <- raster::getValues(raster1, row = row, nrows = nrows)
    block2 <- raster::getValues(raster2, row = row, nrows = nrows)

    func(block1, block2)
  }
}
