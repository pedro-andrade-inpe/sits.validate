
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

#' @title Compare two categorical rasters
#' @description Compares two categorical rasters with the same projection, extent, and resolution.
#' It returns a matrix with the percentage of the area covered by each pair of values from the
#' two rasters.
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

  result <- xtabs(count ~ data + reference, tib) %>%
    as.matrix()

  result <- fillMissingColRows(result)
  result <- result[order(as.numeric(rownames(result))),]
  result <- result[,order(as.numeric(colnames(result)))]
  return(result)
}

#' @title Compare two categorical rasters weitghing each class by its area.
#' @description Compares two categorical rasters with the same projection, 
#' extent, and resolution. It returns a list made of a confusion matrix, a named 
#' vector of areas (in the units of the reference raster), a sublist of vectors 
#' of the superior and inferior confidence intervals (95%), and a sublist of 
#' vectors of the user and producer accuracies.
#' @param data String with the input data file.
#' @param reference String with the reference data file.
#' @export
#' @examples
#' \dontrun{
#' library(raster)
#' dat <- ref <- raster::raster(ncol = 10, nrow = 10)
#' dat[] <- sample(1:5, ncell(dat), replace = TRUE)
#' ref[] <- sample(1:5, ncell(ref), replace = TRUE)
#' dat_file <- tempfile(fileext = ".tif")
#' ref_file <- tempfile(fileext = ".tif")
#' writeRaster(dat, dat_file)
#' writeRaster(ref, ref_file)
#' compareRasters_area(dat_file, ref_file) 
#' }
compareRasters_area <- function(data, reference){

    # @title Asses accuracy and estimate area according to Olofsson
    # @author Alber Sanchez, \email{alber.ipia@@inpe.br}
    # @description Compute the accuracy normalized by the area. Note that, these computations don't work on clustered sampling because the equations are different.
    #
    # @param error_matrix A matrix given in sample counts. Columns represent the reference data and rows the results of the classification
    # @param area         A vector of the total area of each class on the map
    # @return             A list of lists: The error_matrix, the class_areas, confidence interval (confint95, a list of two numerics) and the accuracy (accuracy, a list of three numerics: overall, user, and producer)
    .asses_accuracy_area <- function(error_matrix, area){

        if (any(dim(error_matrix) == 0))
            stop("Invalid dimensions in error matrix.", call. = FALSE)
        if (length(unique(dim(error_matrix))) != 1)
            stop("The error matrix is not square.", call. = FALSE)
        if (!all(colnames(error_matrix) == rownames(error_matrix)))
            stop("Labels mismatch in error matrix.", call. = FALSE)
        if (unique(dim(error_matrix)) != length(area))
            stop("Mismatch between error matrix and area vector.", 
                 call. = FALSE)
        if (!all(names(area) %in% colnames(error_matrix)))
            stop("Label mismatch between error matrix and area vector.", 
                 call. = FALSE)

        # Re-order vector elements.
        area <- area[colnames(error_matrix)]

        W <- area/sum(area)
        n <- rowSums(error_matrix)
        if (any(n < 2))
            stop("Undefined accuracy when there is one or fewer pixels in any predicted class (division by zero).", 
                 call. = FALSE)
        n.mat <- matrix(rep(n, times = ncol(error_matrix)), 
                        ncol = ncol(error_matrix))
        p <- W * error_matrix / n.mat
        error_adjusted_area_estimate <- colSums(p) * sum(area)
        Sphat_1 <- vapply(1:ncol(error_matrix), function(i){
            sqrt(sum(W^2 * error_matrix[, i]/n * (1 - error_matrix[, i]/n)/(n - 1)))
        }, numeric(1))
        
        SAhat <- sum(area) * Sphat_1
        Ahat_sup <- error_adjusted_area_estimate + 2 * SAhat
        Ahat_inf <- error_adjusted_area_estimate - 2 * SAhat
        Ohat <- sum(diag(p))
        Uhat <- diag(p) / rowSums(p)
        Phat <- diag(p) / colSums(p)
        
        return(
            list(error_matrix = error_matrix, area = area,
                 confint95 = list(superior = Ahat_sup, inferior = Ahat_inf),
                 accuracy = list(overall = Ohat, user = Uhat, producer = Phat))
        )
    }

    # Do the math.
    error_matrix <- compareRasters(data = data, reference = reference)

    # Get labels' area in the reference map.
    freq_tab <- reference %>% 
        raster::raster() %>% 
        raster::freq(useNA = "no") %>%
        as.data.frame(stringsAsFactors = TRUE)
    # Number of pixels times the spatial resolution.
    freq_tab["count"] <- freq_tab["count"] * prod(raster::res(raster::raster(reference)))

    # build a vector of areas
    area <- freq_tab[["count"]]
    names(area) <- as.character(freq_tab[["value"]])

    return(.asses_accuracy_area(error_matrix, area))
}

#' @title Count pixels of each class
#' @description Count the number of pixels of each class in a raster file.
#' @param filename String with the file name.
#' @export
countPixels <- function(filename){
  filename %>%
    raster::raster() %>%
    raster::getValues() %>%
    table()
}

