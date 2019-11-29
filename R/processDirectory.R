
getYear <- function(file){
  nc <- nchar(file)
  year <- file %>%
    substr(nc - 9, nc - 6) %>%
    as.numeric()

  cat(paste0("Processing year ", year, "\n"))
  return(year)
}

#' @title Process a classification directory
#' @description Process a classification directory applying a function to
#' every tif file it belongs. For each file, a new file in the output directory
#' will be created with the result of a user-defined processing function.
#' @param inputDir A string with the input directory path.
#' @param outputDir A string with the output directory path.
#' @param whatToDo A function that takes one or two arguments. The first argument
#' is the file name. The second one (optional) is a number with the year related
#' to the file name. The year is extracted from the file name. It is represented
#' as the last four characters before the file extension.
#' @param overwrite Should the output be overwritten?
#' @export
processDirectory <- function(inputDir, outputDir, whatToDo, overwrite = FALSE) {

  files <- getTifFiles(inputDir)

  if (length(files) == 0)
    stop(paste0("Could not find any tif files in directory '", inputDir, "'"))

  createEmptyDir(outputDir, overwrite = overwrite)

  args <- formals(whatToDo) %>% length()

  for (file in files) {

    cat(paste0("Processing ", file, "\n"))

    out_file <- baseDir(paste(outputDir, basename(file), sep = "/"))

    if (args == 1) {

      if (!overwrite && file.exists(out_file))
        next()

      result <- whatToDo(file)

      cat(paste0("Writing ", out_file, "\n"))

      result <- raster::writeRaster(result, filename = out_file, overwrite = overwrite,
                                    datatype = raster::dataType(raster::raster(file)))

    } else if (args == 2) {

      if (!overwrite && file.exists(file))
        next()

      year <- getYear(file)
      result <- whatToDo(file, year)

      cat(paste0("Writing ", out_file, "\n"))

      result <- raster::writeRaster(result, filename = out_file, overwrite = overwrite,
                                    datatype = raster::dataType(raster::raster(file)))
    } else {

      stop("Function should have at most two arguments")
    }

    raster::removeTmpFiles(h = 0)
  }

  invisible(NULL)
}

