
require(sits.validate)

tccerrado2013 <- raster::raster(baseDir("comparable/terraclass-2013.tif"))
sits2013 <- raster::raster(baseDir("classificacoes-final/result-cerrado_2012_8_2013_8.tif"))

bs <- raster::blockSize(sits2013)
total <- data.frame()

forEachBlockPair(tccerrado2013, sits2013, function(blocktc, blocksi){
  for(tcvalue in unique(blocktc)){
    sivalues = blocksi[which(blocktc == tcvalue)] %>% table() %>% as.data.frame()
    if(dim(sivalues)[1] > 0){
      sivalues[,"Var2"] = tcvalue
      total <<- rbind(total, sivalues)
    }
  }
})

names(total) <- c("sits", "count", "tc")
tib <- tibble::as_tibble(total) %>% dplyr::group_by(sits, tc) %>% dplyr::summarize_all(sum)
result <- xtabs(count~sits+tc,tib) %>% as.matrix()

colnames(result) <- getSitsValidateEnv()$classes_tc
rownames(result) <- getSitsValidateEnv()$classes_sits#[-14] # removing sugarcane class

totalValidationTCCerrado(result) # 0.712

res <- summarizeAsPercentage(result)
res <- round(res, 2)
write.table(res, baseDir("results/diff-tc-vs-sits.csv"), sep=",")
