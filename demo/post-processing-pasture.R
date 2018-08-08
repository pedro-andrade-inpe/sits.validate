
pasture = 9

require(sits.validate)

inputDir = "classificacoes-final"
inputFiles <-getTifFiles(inputDir)
inputFiles

outputDir <- "classificacoes-pos-processamento"
file.copy(inputFiles, baseDir(outputDir), overwrite = TRUE)
outputFiles <- getTifFiles(outputDir)
outputFiles

for(pos in 2:16){
  cat(paste0("Processing year ", pos, "/", 16, "\n"))

  raster0 = raster::raster(inputFiles[pos - 1])
  raster1 = raster::raster(outputFiles[pos]) # <- OUTPUT
  raster2 = raster::raster(inputFiles[pos + 1])

  bs <- raster::blockSize(raster0)

  for(i in 1:bs$n){
    cat(paste0("Processing block ", i, "/", bs$n, "\n"))
    row = bs$row[i]
    nrows = bs$nrows[i]

    block0 = raster::getValues(raster0, row = row, nrows = nrows)
    block1 = raster::getValues(raster1, row = row, nrows = nrows)
    block2 = raster::getValues(raster2, row = row, nrows = nrows)

    block1[which(block0 == pasture & block2 == pasture)] <- pasture

    raster1[row:(row + nrows - 1),] <- block1
  }

  cat(paste0("Writing raster\n"))

  raster::writeRaster(raster1, outputFiles[pos], overwrite = TRUE)
}
