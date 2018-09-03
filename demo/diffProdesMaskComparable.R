
require(sits.validate)

prodesmask2013 <- raster::raster(baseDir("cerrado/prodesMask/comparable/prodesmask.tif"))
sits2013 <- raster::raster(baseDir("classificacoes-final/result-cerrado_2014_8_2015_8.tif"))

result <- raster::raster(sits2013) %>%
  raster::writeStart("diff-prodes-mask-comparable.tif", overwrite = TRUE)

bs <- raster::blockSize(sits2013)
total <- data.frame()

for(i in 1:bs$n){
  cat(paste0("Processing block ", i, "/", bs$n, "\n"))
  row <- bs$row[i]
  nrows <- bs$nrows[i]

  blockpr <- raster::getValues(prodesmask2013, row = row, nrows = nrows)
  blockpr[is.na(blockpr)] <- 0

  blocksi <- raster::getValues(sits2013, row = row, nrows = nrows)

  blockresult <- rep(0, length(blocksi))

  zerospr <- blockpr == 0
  onespr <- blockpr == 1

  zerostc <- blocksi >= 1 & blocksi <= 5
  onestc <- blocksi >= 9 & blocksi <= 13

  blockresult[which(zerospr & zerostc)] <- 1
  blockresult[which(onespr & onestc)] <- 1

  blockresult[which(blocksi %in% c(15, 16))] <- 1

  result <- raster::writeValues(result, blockresult, row)
}

result <- raster::writeStop(result)
