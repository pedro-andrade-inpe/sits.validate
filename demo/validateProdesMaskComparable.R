
require(sits.validate)

prodesmask2013 <- raster::raster(baseDir("cerrado/prodesMask/comparable/prodesmask.tif"))
sits2013 <- raster::raster(baseDir("classificacoes-final/result-cerrado_2014_8_2015_8.tif"))

bs <- raster::blockSize(sits2013)
total <- data.frame()

for(i in 1:bs$n){
  cat(paste0("Processing block ", i, "/", bs$n, "\n"))
  row <- bs$row[i]
  nrows <- bs$nrows[i]

  blockpr <- raster::getValues(prodesmask2013, row = row, nrows = nrows)
  blockpr[is.na(blockpr)] <- 0

  blocksi <- raster::getValues(sits2013, row = row, nrows = nrows)

  for(prvalue in unique(blockpr)){
    sivalues = blocksi[which(blockpr == prvalue)] %>% table() %>% as.data.frame()
    if(dim(sivalues)[1] > 0){
      sivalues[, "Var2"] = prvalue
      total <- rbind(total, sivalues)
    }
  }
}

names(total) <- c("sits", "count", "prodes")
tib <- tibble::as_tibble(total) %>% dplyr::group_by(sits, prodes) %>% dplyr::summarize_all(sum)
result <- xtabs(count ~ sits +prodes, tib) %>% as.matrix()

result
colnames(result) <- getSitsValidateEnv()$classes_mask
rownames(result) <- getSitsValidateEnv()$classes_sits[-14] # removing sugarcane class

totalValidationCerradoMask(result) # 0.729

res <- summarizeAsPercentage(result)
res <- round(res, 2)
write.table(res, "result-pm.csv", sep=",")
