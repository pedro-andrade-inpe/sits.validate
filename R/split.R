
#' @title Split a raster file
#' @description Split a raster file into n.side ^ 2 files. Each output file
#' will be have the same name of the input file plut -tile-<number>.
#' @param inputFile Name of the input file.
#' @param outputDir Output directory where the files will be saved.
#' @param n.side Number of splits in each side (x and y).
#' @seealso splitRasters
#' @export
splitRaster <- function(inputFile, outputDir, n.side){
  outputDir <- baseDir(outputDir)
  r  <- raster::raster(inputFile)
  er <- raster::extent(r)

  dx <- (er[2] - er[1]) / n.side  # extent of one tile in x direction
  dy <- (er[4] - er[3]) / n.side  # extent of one tile in y direction
  xs <- seq(er[1], by = dx, length = n.side) #lower left x-coordinates
  ys <- seq(er[3], by = dy, length = n.side) #lower left y-coordinates
  cS <- expand.grid(x = xs, y = ys)

  ## loop over extents and crop
  for(i in 1:nrow(cS)) {
    cat(paste0("tile ", i, "/", nrow(cS), "\n"))
    ex1 <- c(cS[i, 1], cS[i, 1] + dx, cS[i, 2], cS[i, 2] + dy)  # create extents for cropping raster
    cl1 <- raster::crop(r, ex1, progress = "text")
    outputFile <- paste0(tools::file_path_sans_ext(basename(inputFile)), "-tile-", i, ".tif")
    raster::writeRaster(cl1, paste0(outputDir, "/", outputFile), progress = "text", overwrite = TRUE)
  }
}

#' @title Split all rasters within a given directory
#' @description Split all tif files within a directory, each one into n.side ^ 2 files.
#' @param inputDir Name of the input directory.
#' @param outputDir Output directory where the files will be saved.
#' @param n.side Number of splits in each side (x and y).
#' @seealso splitRaster
#' @export
splitRasters <- function(inputDir, outputDir, n.side){
  files <- getTifFiles(inputDir)

  for(file in files){
    cat(paste0("processing file ", basename(file), "\n"))
    splitRaster(file, outputDir, n.side)
  }
}
