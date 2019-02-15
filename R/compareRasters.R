#' @title Compare categorical rasters
#' @description Compares two categorical rasters with the same projection, extent, and resolution.
#' It returns a matrix with the percentage of the area covered by each pair of values from the
#' two rasters.
#' @param file1 A file name.
#' @param file2 Another file name.
#' @export
compareRasters <- function(file1, file2){
  raster1 <- raster::raster(file1)
  raster2 <- raster::raster(file2)
  bs <- raster::blockSize(raster1)
  total <- data.frame()

  for(i in 1:bs$n){
    cat(paste0("Processing block ", i, "/", bs$n, "\n"))
    row <- bs$row[i]
    nrows <- bs$nrows[i]

    blockpr <- raster::getValues(raster2, row = row, nrows = nrows)
    blockpr[is.na(blockpr)] <- 0

    blocksi <- raster::getValues(raster1, row = row, nrows = nrows)

    for(prvalue in unique(blockpr)){
      sivalues = blocksi[which(blockpr == prvalue)] %>% table() %>% as.data.frame()
      if(dim(sivalues)[1] > 0){
        sivalues[, "Var2"] = prvalue
        total <- rbind(total, sivalues)
      }
    }
  }

  names(total) <- c("file1", "count", "file2")
  tib <- tibble::as_tibble(total) %>% dplyr::group_by(file1, file2) %>% dplyr::summarize_all(sum)
  result <- stats::xtabs(count ~ file1 + file2, tib) %>% as.matrix()

  res <- summarizeAsPercentage(result) %>% round(2)
  res <- res[order(as.numeric(rownames(res))),]
  res[,order(as.numeric(colnames(res)))]
}
