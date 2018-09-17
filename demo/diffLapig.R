
require(sits.validate)

lapig2015 <- raster::raster(baseDir("comparable/lapig-2015.tif"))
sits2015 <- raster::raster(baseDir("classificacoes-final/result-cerrado_2014_8_2015_8.tif"))

bs <- raster::blockSize(sits2015)
total <- data.frame()

for(i in 1:bs$n){
  cat(paste0("Processing block ", i, "/", bs$n, "\n"))
  row <- bs$row[i]
  nrows <- bs$nrows[i]

  blockpr <- raster::getValues(lapig2015, row = row, nrows = nrows)
  blockpr[is.na(blockpr)] <- 0

  blocksi <- raster::getValues(sits2015, row = row, nrows = nrows)

  for(prvalue in unique(blockpr)){
    sivalues = blocksi[which(blockpr == prvalue)] %>% table() %>% as.data.frame()
    if(dim(sivalues)[1] > 0){
      sivalues[, "Var2"] = prvalue
      total <- rbind(total, sivalues)
    }
  }
}

names(total) <- c("sits", "count", "lapig")
tib <- tibble::as_tibble(total) %>% dplyr::group_by(sits, lapig) %>% dplyr::summarize_all(sum)
result <- xtabs(count ~ sits + lapig, tib) %>% as.matrix()

result
colnames(result) <- getSitsValidateEnv()$classes_mask
rownames(result) <- getSitsValidateEnv()$classes_sits[-14] # removing sugarcane class

totalValidationCerradoMask(result) # 0.703

res <- summarizeAsPercentage(result)
res <- round(res, 2)

write.table(res, baseDir("results/diff-lapig-vs-sits.csv"), sep=",")
