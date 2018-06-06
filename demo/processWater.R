
require(sits.validate)
require(raster)

process <- function(filename, progress = TRUE){
    file <- raster::raster(basePath(paste0("water/", filename)))

    printProgress(paste0("Processing file '", filename, "'"), progress)

    pixels <- dim(file)[1] * dim(file)[2]

    mymap <- function(values){
        printProgress(pixels, progress)
        pixels <<- pixels - length(values)
        result <- rep(0, length(values))
        result[values > 50 & values < 100] <- 1
        return(result)
    }

    result <- raster::calc(file, mymap)

    outFile <- basePath(paste0("water/50p/", tools::file_path_sans_ext(filename), "-50p.tif"))

    printProgress(paste0("Writing file '", outFile, "'"), progress)

    writeRaster(result, outFile, format="GTiff", overwrite=TRUE)
}

classificationFiles <- Sys.glob(basePath("water/*.tif"))

classificationFiles

lapply(classificationFiles, process)
