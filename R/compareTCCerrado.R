
# explanations about raster::extract()
# https://gis.stackexchange.com/questions/187096/extract-function-from-r-raster-package-returns-different-cells-if-weights-pa
# weights
# logical. If TRUE and normalizeWeights=FALSE, the function returns, for each polygon, a matrix with the cell values and the approximate fraction of each cell that is covered by the polygon(rounded to 1/100). If TRUE and normalizeWeights=TRUE the weights are normalized such that they add up to one. The weights can be used for averaging; see examples. This option can be useful (but slow) if the polygons are small relative to the cells size of the Raster* object

###### NAMES FOR THE VALUES ######

# Update rownames according to a vector of names that can be one of the above vectors
updateRownames = function(data, names){
  rownames(data) = names[strtoi(rownames(data))]
  return(data)
}

# Convert a vector of pixels into a summary with the areas of each class
summarizePixels = function(pixels, resolution){ # supposes that the resolution is in meters
  result = table(pixels)
  t(t(round(result * resolution[1] * resolution[2] / 10000))) # to hectares
}

# Compute total area of each class within a raster data
totalPixels = function(raster){
  resolution = raster::res(raster)
  values = raster::getValues(raster)
  summarizePixels(values, resolution)
}

printTotal = function(result){
  str = capture.output(print(result))
  str = str[-(1:2)]
  str[1] = paste0(" ", str[1])
  cat(paste0(str, "ha\n"))
  cat(paste0("Total: ", sum(result), "ha\n"))
}

degreesToMeters = function(degrees) degrees * 111000

rasterToPolygon = function(raster){
  p <- as(extent(raster), "SpatialPolygons")
  projection(p) = crs(raster)
  return(p)
}

pixelsWithinRaster = function(biggerRaster, reference){
  p <- rasterToPolygon(reference)
  p = spTransform(p, crs(biggerRaster))
  raster::extract(biggerRaster, p)
}

pixelsWithinPolygons = function(polygons, raster){
  p <- as(extent(raster), "SpatialPolygons")
  projection(p) = crs(raster)

  p = spTransform(p, crs(biggerRaster))

  extract(biggerRaster, p)
}

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

totalValidation = function(result){
  total = sum(result)
  (sum(result[10:14, 1]) + sum(result[1:5,5]) + result[9,11])/total
}

printLog = function(value, log)
  if(log) cat(paste0(value, "\n"))

compareTCCerrado = function(data, log = TRUE){
  tccerrado2013 = raster(mypath("/MapasReferencia/TerraClassCerrado/TCCerrado_2013_GCS_30m/TCCerrado_2013_geo.tif"))

  polygons = raster::rasterToPolygons(data, dissolve = TRUE) #%>%
   # st_as_sf() %>%
  #  st_transform("+proj=longlat +ellps=GRS80 +no_defs")

  output = matrix(0, ncol = length(classificacao_tc), nrow = length(classificacao_sits))
  colnames(output) = classificacao_tc_simplificada
  rownames(output) = classificacao_sits

  # update to apply/mclapply
  quantity = length(polygons)
  print(quantity)
  for (i in 1:quantity){
    p1 = polygons[i,]

    line = p1@data[1,1] #%>% st_set_geometry(NULL)
    #line = line[1,1]

    printLog(paste0("processing ", i, "/", quantity, " (class ", line, ")"), log)

    res1 = raster::extract(tccerrado2013, p1) # do this for each polygon above
    summ = summarizePixels(res1[[1]], degreesToMeters(raster::res(tccerrado2013)))

    columns = strtoi(rownames(t(t(summ))))

    output[line, columns] <- summ[,1]
  }

  output

}

