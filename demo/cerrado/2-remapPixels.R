
require(sits.validate)

classificationDir <- "cerrado-2019-08-pos-mascaras"

legend <- readLegend()

replacementsCerrado <- landClasses(
  CERR, CERR, CERD, SAVN, CERR,
  SAND, COTF, MILC, PAST, SOYC,
  SOYT, SOYF, SOYM, URBA, NOBS,
  WATR
)

remapDirectory(classificationDir, "cerrado-2019-08-replacements", legend, replacementsCerrado)
