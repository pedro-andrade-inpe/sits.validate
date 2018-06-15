
require(sits.validate)

outputDir <- normalizePath(baseDir("water/split"))
inputDir <- baseDir("water")
splitRasters(inputDir, outputDir, 4)
