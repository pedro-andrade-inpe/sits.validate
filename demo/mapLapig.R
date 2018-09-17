
require(sits.validate)

lapig2015 <- raster::raster(baseDir("comparable/lapig-2015.tif"))
sits2015 <- raster::raster(baseDir("classificacoes-final/result-cerrado_2014_8_2015_8.tif"))

result <- raster::raster(sits2015) %>%
  raster::writeStart(baseDir("results/map-lapig-vs-sits.tif"), overwrite = TRUE)

bs <- raster::blockSize(sits2015)
total <- data.frame()

for(i in 1:bs$n){
  cat(paste0("Processing block ", i, "/", bs$n, "\n"))
  row <- bs$row[i]
  nrows <- bs$nrows[i]

  blocklapig <- raster::getValues(lapig2015, row = row, nrows = nrows)
  blocklapig[is.na(blocklapig)] <- 0

  blocksi <- raster::getValues(sits2015, row = row, nrows = nrows)

  blockresult <- rep(0, length(blocksi))

  zeroslapig <- blocklapig == 0
  oneslapig <- blocklapig == 1

  pasturesits <- blocksi == 9

  blockresult[which(pasturesits & oneslapig)]  <- 1 # pasto no sits e no lapig
  blockresult[which(pasturesits & zeroslapig)] <- 2 # pasto no sits mas nao no lapig
  blockresult[which(!pasturesits & oneslapig)] <- 3 # pasto no lapig mas nao no sits

  result <- raster::writeValues(result, blockresult, row)
}

result <- raster::writeStop(result)

