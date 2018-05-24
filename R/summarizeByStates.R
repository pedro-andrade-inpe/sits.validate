
require(sf)
require(raster)
require(rgeos)
require(dplyr)


###### DEFINE WHERE THE DATA IS ######

base_dir = "/Users/pedro/TWDTWAmazoniaCerrado/"

mypath = function(path) paste0(base_dir, path)


classes_sits = c(
  "1.  Araguaia",
  "2.  Campo_Cerrado",
  "3.  Cerradao",
  "4.  Cerrado",
  "5.  Cerrado_Rupestre",
  "6.  Dunas",
  "7.  Fallow_Cotton",
  "8.  Millet_Cotton",
  "9.  Pasture",
  "10. Soy_Corn",
  "11. Soy_Cotton",
  "12. Soy_Fallow",
  "13. Soy_Millet",
  "14. Sugarcane",
  "15. Urban Area",
  "16. Water"
)


simplifyOutput = function(output){
  # adding the totals as a new line and a new column
  sum_lines = apply(output, 1, sum)
  sum_columns = apply(output, 2, sum)
  
  total = sum(output)
  sum_columns = c(sum_columns, total)
  output = cbind(output, Total = sum_lines)
  output = rbind(output, Total = sum_columns)
  
  # removing the lines whose values are all equal to zero
  output<-output[-which(apply(output, 1, function(x) all(x == 0))),]
  
  # removing the columns whose values are all equal to zero
  output<-output[,-which(apply(output, 2, function(x) all(x == 0)))]
  
  # final output comparing the two data
  output
}

printLog = function(value, log)
  if(log) cat(paste0(value, "\n"))


# Convert a vector of pixels into a summary with the areas of each class
summarizePixels = function(pixels, resolution){ # supposes that the resolution is in meters
  result = table(pixels)
  t(t(round(result * resolution[1] * resolution[2] / 10000))) # to hectares
}

summarizeByStates = function(data, log = TRUE){
  brazil = sf::st_read(dsn = mypath("Shapefiles/Brasil"), layer = "UFEBRASIL", quiet=TRUE) %>%
    st_transform("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")

  nstates = dim(brazil)[1]
  statesnames = as.data.frame(brazil)[,"NM_ESTADO"]
  
  output = matrix(0, ncol = nstates, nrow = length(classes_sits))
  colnames(output) = statesnames
  rownames(output) = classes_sits

  for(i in 1:nstates){
    print(paste0("Processing ", statesnames[i]))
    
    polygon = brazil[i,]
    
    summ = raster::extract(data, polygon) %>% summarizePixels(degreesToMeters(raster::res(data)))
    columns = strtoi(rownames(t(t(summ))))
    output[columns, i] <- summ[,1]
  }

  output  
}

LUC2015 = raster::raster(mypath("RasterData/Luciara_MT/classificacoes/LUC-class-Cerrado_28022018_2014_8_2015_8.tif"))

result = summarizeByStates(LUC2015)

simplifyOutput(result)
