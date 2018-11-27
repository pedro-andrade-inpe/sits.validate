
require(sits.validate)

prodesmask2015 <- raster::raster(baseDir("comparable/prodesmask-2015.tif"))
sits2015 <- raster::raster(baseDir("classificacoes-final/result-cerrado_2014_8_2015_8.tif"))

bs <- raster::blockSize(sits2015)
total <- data.frame()

forEachBlockPair(prodesmask2015, sits2015, function(blockpr, blocksi){
  blockpr[is.na(blockpr)] <- 0

  for(prvalue in unique(blockpr)){
    sivalues <- blocksi[which(blockpr == prvalue)] %>% table() %>% as.data.frame()
    if(dim(sivalues)[1] > 0){
      sivalues[, "Var2"] = prvalue
      total <<- rbind(total, sivalues)
    }
  }
})

names(total) <- c("sits", "count", "prodes")
tib <- tibble::as_tibble(total) %>% dplyr::group_by(sits, prodes) %>% dplyr::summarize_all(sum)
result <- xtabs(count ~ sits +prodes, tib) %>% as.matrix()

result
colnames(result) <- getSitsValidateEnv()$classes_mask
rownames(result) <- getSitsValidateEnv()$classes_sits

totalValidationCerradoMask(result) # 0.719

res <- summarizeAsPercentage(result)
res <- round(res, 2)
write.table(res, baseDir("results/diff-prodesmask-vs-sits.csv"), sep=",")
