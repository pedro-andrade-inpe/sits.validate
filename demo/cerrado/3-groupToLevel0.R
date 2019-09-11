
require(sits.validate)

classificationDir <- "cerrado-2019-08-pos-mascaras"

legend <- readLegend()

groupDirectoryToLevel1(classificationDir, "cerrado-2019-08-grouped", legend)
