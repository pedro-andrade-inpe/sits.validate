# remap amazon biome classification classes

require(sits.validate)

setBaseDir("/home/rolf/inpe/Dropbox/Rolf/Geodata/sits/Amazon/")
classificationDir <- "DL_4Bands_3rdAMSamples"

legend <- readLegend()

replaceAmazon <- legend %>% landClasses(
  COTF, FAMZ, MILC, PAST, SAVN, SAVN,
  SOYC, SOYT, SOYF, SOYM, SOYS, SWET,
  URBA, WATR, SECV)

remapDirectory(classificationDir, "post_process_remap", replaceAmazon)
