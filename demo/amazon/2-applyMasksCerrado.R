# This script creates amazon data based on the SITS classifications,
# cutting the data with the amazon polygon (from IBGE), hidrography
# and urban areas

require("sits.validate")
setBaseDir("/home/rolf/inpe/Dropbox/Rolf/Geodata/sits/Amazon/")

applyMask <- function(file) {
  years <- c(14, 17)
  year <- file %>%
    basename %>%
    substr(years[1], years[2])

  cat(paste0("Processing year ", year, "\n"))

  year <- year %>% as.numeric()

  file %>%
    raster::raster() %>%
    amazonMask() %>%
    amazonUrbanMask(legend, year) %>%
    amazonWaterMask(legend)
}

processDirectory("post_process_remap", "post_process_masks", applyMask)
