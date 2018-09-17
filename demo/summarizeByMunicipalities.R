

require(sits.validate)

classificationsDir = baseDir("")

LUC2015 <- raster::raster(paste0(classificationsDir, "Luciara_MT/classificacoes/LUC-class-Cerrado_28022018_2014_8_2015_8.tif"))
ADN2015 <- raster::raster(paste0(classificationsDir, "AlvoradaDoNorte_GO/classificacoes/ADN-class-Cerrado_28022018_2014_8_2015_8.tif"))

result1 <- summarizeOneByMunicipalities(LUC2015)
result2 <- summarizeOneByMunicipalities(ADN2015)

result1
result2

output <- joinClassifications(list(result1, result2))

output


###############################################
# transpose the csv

read.csv("result2001.csv") %>% tibble::as.tibble() -> data

data %>%
  select(-X) %>%
  tidyr::gather(var, value, -rowname) %>%
  tidyr::spread(rowname, value) %>%
  write.csv(file = paste0("result-test.csv"))

###############################################


classifications = getTifFiles("classificacoes-final")

for(file in classifications[5:17]){
  myraster = raster::raster(file)

  result <- summarizeOneByMunicipalities(myraster)
  year <- file %>%
    basename %>%
    substr(23, 26) %>%
    as.numeric()

  var <- paste0("result", year)
  assign(var, result)

  write.csv(result, file = paste0("result", year, ".csv"))
}



