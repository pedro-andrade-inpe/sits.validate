
#' @title Apply cerrado mask
#' @description Apply cerrado biome mask in order to remove all pixels that do not
#' belong to the biome
#' @param myraster A raster data.
#' @export
cerradoMask <- function(myraster){
  cat("Applying cerrado mask\n")

  cerrado <- raster::raster(baseDir("masks/cerrado/cerradoMask.tif"))
  cerrado.sub <- raster::crop(cerrado, raster::extent(myraster))

  raster::mask(myraster, cerrado.sub, progress = "text")
}

#' @title Apply cerrado water mask
#' @description Apply cerrado water mask from Perkel et al. (doi:10.1038/nature20584) over a raster, given a legend and an year.
#' @param myraster A raster.
#' @param legend A tibble legend.
#' @export
cerradoWaterMask <- function(myraster, legend){
  cat("Applying water mask\n")
  water.sub <- baseDir("masks/cerrado/cerradoWaterMask.tif") %>%
    raster::raster() %>%
    raster::crop(raster::extent(myraster))

  waterClass <- legend$Label %>% stringr::str_detect("Water") %>% which()

  if(length(waterClass) > 1){
    stop("waterMask() cannot be used as there are more than one 'Water' classes in the legend")
  }
  else if(length(waterClass) == 0){
    stop("Could not find any 'Water' class in the legend")
  }

  raster::mask(myraster, water.sub, maskvalue = 1, updatevalue = waterClass, progress = "text")
}

#' @title Apply cerrado urban mask
#' @description Apply cerrado urban mask from ?? over a raster, given a legend and an year.
#' @param myraster A raster.
#' @param legend A tibble legend.
#' @param year An year. Urban area data are available for 2000 to 2016.
#' @export
cerradoUrbanMask <- function(myraster, legend, year){
  cat("Applying urban mask\n")

  urban.sub <- paste0("masks/cerrado/urban_areas_", year, ".tif") %>%
    baseDir %>%
    raster::raster() %>%
    raster::crop(raster::extent(myraster))

  urbanClass <- legend$Label %>% stringr::str_detect("Urban") %>% which()

  if(length(urbanClass) > 1){
    stop("urbanMask() cannot be used as there are more than one 'Urban' classes in the legend")
  }
  else if(length(urbanClass) == 0){
    stop("Could not find any 'Urban' class in the legend")
  }

  raster::mask(myraster, urban.sub, maskvalue = 1, updatevalue = urbanClass, progress = "text")
}

#' @title Apply cerrado sugarcane mask
#' @description Apply cerrado sugarcane mask from Canasat over a raster, given a legend and an year.
#' @param myraster A raster.
#' @param legend A tibble legend.
#' @param year An year. Sugarcane data are available for 2000 to 2016.
#' @export
cerradoSugarcaneMask <- function(myraster, legend, year){
  cat("Applying sugarcane mask\n")

  sugar <- paste0("masks/cerrado/Canasat_", year, ".tif") %>%
    baseDir %>%
    raster::raster() %>%
    raster::crop(raster::extent(myraster))

  sugarcaneClass <- legend$Label %>% stringr::str_detect("Sugarcane") %>% which()

  if(length(sugarcaneClass) > 1){
    stop("sugarcaneMask() cannot be used as there are more than one 'Sugarcane' classes in the legend")
  }
  else if(length(sugarcaneClass) == 0){
    stop("Could not find any 'Sugarcane' class in the legend")
  }

  raster::mask(myraster, sugar, maskvalue = 1, updatevalue = sugarcaneClass, progress = "text")
}


#' @title Apply amazon mask
#' @description Apply amazon biome mask in order to remove all pixels that do not
#' belong to the biome
#' @param myraster A raster data.
#' @export
amazonMask <- function(myraster){
  cat("Applying amazon mask\n")

  amazon <- raster::raster(baseDir("masks/amazonMask.tif"))
  amazon.sub <- raster::crop(amazon, raster::extent(myraster))

  raster::mask(myraster, amazon.sub, progress = "text")
}

#' @title Apply amazon water mask
#' @description Apply amazon water mask from Perkel et al. (doi:10.1038/nature20584) over a raster, given a legend and an year.
#' @param myraster A raster.
#' @param legend A tibble legend.
#' @export
amazonWaterMask <- function(myraster, legend) {

  cat("Applying water mask\n")
  water.sub <- baseDir("masks/amazonWaterMask.tif") %>%
    raster::raster() %>%
    raster::crop(raster::extent(myraster))

  waterClass <- legend$Label %>% stringr::str_detect("Water") %>% which()

  if (length(waterClass) > 1) {
    stop("waterMask() cannot be used as there are more than one 'Water' classes in the legend")
  }
  else if (length(waterClass) == 0) {
    stop("Could not find any 'Water' class in the legend")
  }

  waterClass <- legend$Value[waterClass]
  raster::mask(myraster, water.sub, maskvalue = 1, updatevalue = waterClass, progress = "text")
}

#' @title Apply amazon urban mask
#' @description Apply amazon urban mask from ?? over a raster, given a legend and an year.
#' @param myraster A raster.
#' @param legend A tibble legend.
#' @param year An year. Urban area data are available for 2000 to 2017.
#' @export
amazonUrbanMask <- function(myraster, legend, year) {
  cat("Applying urban mask\n")

  urban.sub <- paste0("masks/urban_areas_", year, ".tif") %>%
    baseDir %>%
    raster::raster() %>%
    raster::crop(raster::extent(myraster))

  urbanClass <- legend$Label %>% stringr::str_detect("Urban") %>% which()

  if (length(urbanClass) > 1) {
    stop("urbanMask() cannot be used as there are more than one 'Urban' classes in the legend")
  }
  else if (length(urbanClass) == 0) {
    stop("Could not find any 'Urban' class in the legend")
  }

  urbanClass <- legend$Value[urbanClass]
  raster::mask(myraster, urban.sub, maskvalue = 1, updatevalue = urbanClass, progress = "text")
}
