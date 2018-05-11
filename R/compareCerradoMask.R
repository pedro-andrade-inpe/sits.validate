
# explanations about raster::extract()
# https://gis.stackexchange.com/questions/187096/extract-function-from-r-raster-package-returns-different-cells-if-weights-pa
# logical. If TRUE and normalizeWeights=FALSE, the function returns, for each polygon, a matrix with the cell values and the approximate fraction of each cell that is covered by the polygon(rounded to 1/100). If TRUE and normalizeWeights=TRUE the weights are normalized such that they add up to one. The weights can be used for averaging; see examples. This option can be useful (but slow) if the polygons are small relative to the cells size of the Raster* object

require(sf)
require(raster)
require(rgeos)
require(parallel)

###### DEFINE WHERE THE DATA IS ######

base_dir = "/Users/pedro/TWDTWAmazoniaCerrado/"

mypath = function(path) paste0(base_dir, path)

###### NAMES FOR THE VALUES ######

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

classes_mask = c(
  "0. Fora",
  "1. Dentro"
)

degreesToMeters = function(degrees) degrees * 111000

printTotal = function(result){
  str = capture.output(print(result))
  str = str[-(1:2)]
  str[1] = paste0(" ", str[1])
  cat(paste0(str, "ha\n"))
  cat(paste0("Total: ", sum(result), "ha\n"))
}

rasterToPolygon = function(raster){
  p <- as(extent(raster), "SpatialPolygons")
  projection(p) = crs(raster)
  return(p)
}

summarizeAsPercentage = function(result){
  sum_columns = apply(result, 2, sum)
  result/sum(sum_columns)*100
}

totalValidation = function(result){
  percentages = summarizeAsPercentage(result)
  return (sum(percentages[1:5,1]) + sum(percentages[9:13,2]))
}

compareWithCerradoMask = function(data, log = TRUE){
  if(log) print("1/7 - Computing box")
  
  boxluc = rasterToPolygon(data) %>%
    st_as_sf %>%
    st_transform("+proj=longlat +ellps=GRS80 +no_defs")

  if(log) print("2/7 - Loading mask 1/3 (slow)")

  mask1 = sf::st_read(dsn = mypath("MapasReferencia/Cerrado"), layer = "MASC_CERR_2000_2015_lote_final_cor_pol")
  submask1a = mask1[boxluc, op = st_overlaps]
  submask1b = mask1[boxluc, op = st_within]
  rm(mask1)
  gc()
  
  if(log) print("3/7 - Loading mask 2/3 (slow)")
  
  mask2 = sf::st_read(dsn = mypath("MapasReferencia/Cerrado"), layer = "MASC_CERR_2000_2015_lote_final_cor_pol_split1")
  submask2a = mask2[boxluc, op = st_overlaps]
  submask2b = mask2[boxluc, op = st_within]
  rm(mask2)
  gc()
  
  if(log) print("4/7 - Loading mask 3/3")
  
  mask3 = sf::st_read(dsn = mypath("MapasReferencia/Cerrado"), layer = "MASC_CERR_2000_2015_lote_final_cor_pol_split2")
  submask3a = mask3[boxluc, op = st_overlaps]
  submask3b = mask3[boxluc, op = st_within]
  rm(mask3)
  gc()

  if(log) print("5/7 - Joining the three masks")
  
  submask = rbind(submask1a, submask1b, submask2a, submask2b, submask3a, submask3b) %>%
    st_transform("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs") %>%
    st_combine() %>%
    st_sf()

  #submask = st_union(submask) # polygon has self-intersection? very strange...

  submask[,"abc"] = "1" # need to do this because raster::extract requires that the data should have at least one attribute

  if(log) print("6/7 - Extracting pixels within mask")
  
  pixels_mask = unlist(raster::extract(data, submask)) %>% table()

  if(log) print("7/7 - Extracting pixels within all the data")
  
  all_pixels = raster::getValues(data) %>% table()

  output = matrix(0, ncol = length(classes_mask), nrow = length(classes_sits))

  output[as.numeric(names(all_pixels)), 1] = as.vector(all_pixels)
  output[as.numeric(names(pixels_mask)), 2] = as.vector(pixels_mask)
  output[,1] = output[,1] - output[,2] # removing the pixels within mask from the total

  colnames(output) = classes_mask
  rownames(output) = classes_sits

  output = output * degreesToMeters(raster::res(data)[1]) / 10000000 # to khectars

  return(output)
}

LUC2015 = raster::raster(mypath("RasterData/Luciara_MT/classificacoes/LUC-class-Cerrado_28022018_2014_8_2015_8.tif"))

result = compareWithCerradoMask(LUC2015) # result in khectars
summarizeAsPercentage(result)
totalValidation(result)


ADN2015 = raster::raster(mypath("RasterData/AlvoradaDoNorte_GO/classificacoes/ADN-class-Cerrado_28022018_2014_8_2015_8.tif"))

result = compareWithCerradoMask(ADN2015) # result in khectars



WAN2015 = raster::raster(mypath("RasterData/Wanderley_BA/classificacoes/WAN-class-filtered-svm_2014_8_2015_8.tif"))
