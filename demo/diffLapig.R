
require(sits.validate)

lapig2015 <- raster::raster(baseDir("comparable/lapig-2015.tif"))
sits2015 <- raster::raster(baseDir("classificacoes-final/result-cerrado_2014_8_2015_8.tif"))

total <- data.frame()

forEachBlockPair(lapig2015, sits2015, function(blockpr, blocksi){
  blockpr[is.na(blockpr)] <- 0

  for(prvalue in unique(blockpr)){
    sivalues = blocksi[which(blockpr == prvalue)] %>% table() %>% as.data.frame()
    if(dim(sivalues)[1] > 0){
      sivalues[, "Var2"] = prvalue
      total <<- rbind(total, sivalues)
    }
  }
})

names(total) <- c("sits", "count", "lapig")
tib <- tibble::as_tibble(total) %>% dplyr::group_by(sits, lapig) %>% dplyr::summarize_all(sum)
result <- xtabs(count ~ sits + lapig, tib) %>% as.matrix()

result
colnames(result) <- getSitsValidateEnv()$classes_mask
rownames(result) <- getSitsValidateEnv()$classes_sits

totalValidationCerradoMask(result) # TODO: fazer um para o Lapig
# provavelmente fazer uma funcao apenas para mapa e diff para executar mais rapido

res <- summarizeAsPercentage(result)
res <- round(res, 2)

write.table(res, baseDir("results/diff-lapig-vs-sits.csv"), sep=",")
