# This script creates cerrado data based on the SITS classifications,
# cutting the data with the cerrado polygon (from IBGE), hidrography
# and urban areas

require("sits.validate")

applyMask <- function(file){
  years <- c(16, 19)
  year <- file %>%
    basename %>%
    substr(years[1], years[2])

  cat(paste0("Processing year ", year, "\n"))

  year <- year %>% as.numeric()

  file %>%
    raster::raster() %>%
    cerradoMask() %>%
    cerradoUrbanMask(legend, year) %>%
    cerradoSugarcaneMask(legend, year) %>%
    cerradoWaterMask(legend)
}

processDirectory("cerrado-2019-08", "cerrado-2019-08-pos-mascaras", applyMask)
