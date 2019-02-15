
#' @title Converts a raster to a SpatialPolygons, keeping the same projection
#' @name rasterBoxToPolygon
#' @author Pedro R. Andrade, \email{pedro.andrade@@inpe.br}
#'
#' @description This function returns a SpatialPolygons with the
#' bounding box of a raster data, with the same projection of the
#' original data.
#' @param raster A raster::raster.
#' @export
rasterBoxToPolygon <- function(raster){
  p <- methods::as(raster::extent(raster), "SpatialPolygons")
  print(class(p))
  raster::projection(p) <- raster::crs(raster)
  return(p)
}

#' @title Empty raster from a given raster file
#' @description Return an empty raster with the same
#' pixels of a given raster stored into a file.
#' @param filename Name of a file that stores raster data.
#' @export
getEmptyRaster <- function(filename){
  raster::raster(filename) %>% raster::raster()
}

#' @title Get sits palette
#' @description Return the sits palette with the values, colors, and labels.
#' @param filename File name to be read (a qml file). As default, it uses the
#' sits palette, stored in style_cerrado_13_classes.qml.
#' @export
getPalette <- function(filename = baseDir("style_cerrado_13classes.qml")){
  xml <- XML::xmlParse(filename) %>% XML::xmlToList()

  palette <- t(as.data.frame(xml$pipe$rasterrenderer$colorPalette))

  rownames(palette) <- NULL

  return(palette)
}

#' @title Resample raster data
#' @description Resample a raster file using the resolution and extent of a
#' reference data.
#' @param inputFile File name to be resampled.
#' @param outputFile File name to be created.
#' @param reference The reference data whose resolution and extent will be
#' used for the otput file.
#' @export
reSample <- function(inputFile, outputFile, reference){
  referenceData <- baseDir(reference)
  path <- dirname((baseDir(inputFile)))

  splitDir <- paste0(path, "/split")
  dir.create(splitDir)

  resultDir <- baseDir(paste0(dirname(outputFile), "/result"))
  dir.create(resultDir)

  splitRaster(referenceData, paste0(dirname(inputFile), "/split"), 8) # split into 64 tif files

  files <- list.files(splitDir)

  lapigtif <- baseDir(inputFile)
  lapig <- raster::raster(lapigtif)

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

  outputFile <- baseDir(outputFile) %>% normalizePath(mustWork = FALSE)

  if(file.exists(outputFile)) file.remove(outputFile)

  cat(paste0("Creating '", outputFile, "'\n"))
  rasters$filename <- outputFile

  do.call(raster::merge, rasters)

  unlink(splitDir, recursive = TRUE)
  unlink(resultDir, recursive = TRUE)
}

#' @title Compare two rasters
#' @description Generate a confusion matrix to compare two rasters.
#' @param data String with the input data file.
#' @param reference String with the reference data file.
#' @export
compareRasters <- function(data, reference){
  referenceRaster <- raster::raster(reference)
  dataRaster <- raster::raster(data)

  total <- data.frame()

  forEachBlockPair(referenceRaster, dataRaster, function(blockpr, blocksi){
    blockpr[is.na(blockpr)] <- 0

    for(prvalue in unique(blockpr)){
      sivalues = blocksi[which(blockpr == prvalue)] %>% table() %>% as.data.frame()
      if(dim(sivalues)[1] > 0){
        sivalues[, "Var2"] = prvalue
        total <<- rbind(total, sivalues)
      }
    }
  })

  names(total) <- c("data", "count", "reference")
  tib <- tibble::as_tibble(total) %>% dplyr::group_by(data, reference) %>% dplyr::summarize_all(sum)

  xtabs(count ~ data + reference, tib) %>%
    as.matrix() %>%
    summarizeAsPercentage() %>%
    round(2)
}

#' @title Count pixels of each class
#' @description Count the number of pixels of each class in a raster file.
#' @param filename String with the file name.
#' @export
countPixels <- function(filename){
  filename %>%
    raster::raster() %>%
    getValues %>%
    table
}
