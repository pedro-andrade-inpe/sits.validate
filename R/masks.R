
#' @title Applies water mask
#' @description Updates the pixels that overlap the water pixels
#' into water. This function only works for Brazil.
#' @param myraster A raster.
#' @param legend A tibble with the legend, as described by readLegend().
#' @export
waterMask <- function(myraster, legend){
  cat("Applying water mask\n")
  water <- raster::raster(baseDir("masks/waterMask.tif"))

  water.sub <- raster::crop(water, raster::extent(myraster))
  waterClass <- csv$Label %>% str_detect("Water") %>% which()

  if(length(waterClass) > 1){
    stop("waterMask() cannot be used as there are more than one 'Water' classes in the legend")
  }
  else if(length(waterClass) == 0){
    stop("Could not find any 'Water' class in the legend")
  }

  raster::mask(myraster, water.sub, maskvalue = 1, updatevalue = waterClass, progress = "text")
}

urbanMask <- function(myraster, year, legend){
  cat("Applying urban mask\n")

  urban <- paste0("masks/urban_areas_", year, ".tif") %>%
    baseDir %>%
    raster::raster()

  urban.sub <- raster::crop(urban, raster::extent(myraster))

  urbanClass <- legend$Label %>% str_detect("Urban") %>% which()

  if(length(urbanClass) > 1){
    stop("urbanMask() cannot be used as there are more than one 'Urban' classes in the legend")
  }
  else if(length(urbanClass) == 0){
    stop("Could not find any 'Urban' class in the legend")
  }

  raster::mask(myraster, urban.sub, maskvalue = 1, updatevalue = urbanClass, progress = "text")
}

sugarcaneMask <- function(myraster, legend, year){
  cat("Applying sugarcane mask\n")

  sugar <- paste0("masks/Canasat_", year, ".tif") %>%
    baseDir %>%
    raster::raster()

  if(raster::extent(sugar) != raster::extent(myraster)){
    sugar <- raster::crop(sugar, raster::extent(myraster))
  }

  sugarcaneClass <- csv$Label %>% str_detect("Sugarcane") %>% which()

  if(length(waterClass) > 1){
    stop("sugarcaneMask() cannot be used as there are more than one 'Sugarcane' classes in the legend")
  }
  else if(length(waterClass) == 0){
    stop("Could not find any 'Sugarcane' class in the legend")
  }

  raster::mask(myraster, sugar, maskvalue = 1, updatevalue = sugarcaneClass, progress = "text")
}
