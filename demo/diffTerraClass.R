
require(sits.validate)

tccerrado2013 <- raster::raster(baseDir("comparable/terraclass-2013.tif"))
sits2013 <- raster::raster(baseDir("classificacoes-final/result-cerrado_2012_8_2013_8.tif"))

bs <- raster::blockSize(sits2013)
total <- data.frame()

for(i in 1:bs$n){
  cat(paste0("Processing block ", i, "/", bs$n, "\n"))
  row = bs$row[i]
  nrows = bs$nrows[i]

  blocktc = raster::getValues(tccerrado2013, row = row, nrows = nrows)
  blocksi = raster::getValues(sits2013, row = row, nrows = nrows)

  for(tcvalue in unique(blocktc)){
    sivalues = blocksi[which(blocktc == tcvalue)] %>% table() %>% as.data.frame()
    if(dim(sivalues)[1] > 0){
      sivalues[,"Var2"] = tcvalue
      total <- rbind(total, sivalues)
    }
  }
}

names(total) <- c("sits", "count", "tc")
tib <- tibble::as_tibble(total) %>% dplyr::group_by(sits, tc) %>% dplyr::summarize_all(sum)
result <- xtabs(count~sits+tc,tib) %>% as.matrix()

colnames(result) <- getSitsValidateEnv()$classes_tc
rownames(result) <- getSitsValidateEnv()$classes_sits[-14] # removing sugarcane class

totalValidationTCCerrado(result) # 0.713

res <- summarizeAsPercentage(result)
res <- round(res, 2)
write.table(res, baseDir("results/diff-tc-vs-sits.csv"), sep=",")
