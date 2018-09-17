
require(sits.validate)

tc2013 <- raster::raster(baseDir("comparable/terraclass-2013.tif"))
sits2013 <- raster::raster(baseDir("classificacoes-final/result-cerrado_2012_8_2013_8.tif"))

result <- raster::raster(sits2013) %>%
  raster::writeStart(baseDir("results/map-tc-vs-sits.tif"), overwrite = TRUE)

bs <- raster::blockSize(sits2013)
total <- data.frame()

for(i in 1:bs$n){
  cat(paste0("Processing block ", i, "/", bs$n, "\n"))
  row <- bs$row[i]
  nrows <- bs$nrows[i]

  blocktc <- raster::getValues(tc2013, row = row, nrows = nrows)
  blocktc[is.na(blockpr)] <- 0

  blocksi <- raster::getValues(sits2013, row = row, nrows = nrows)

  blockresult <- rep(0, length(blocksi))

  tcpasto <- blocktc == 11
  sitspasto <- blocksi == 9

  blockresult[which(sitspasto & tcpasto)]  <- 1 # pasto no sits e no tc
  blockresult[which(sitspasto & !tcpasto)] <- 2 # pasto no sits mas nao no tc
  blockresult[which(!sitspasto & tcpasto)] <- 3 # pasto no tc mas nao no sits

  result <- raster::writeValues(result, blockresult, row)
}

result <- raster::writeStop(result)
