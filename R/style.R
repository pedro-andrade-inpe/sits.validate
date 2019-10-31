
#' @title Read a legend from a file
#' @description Read a legend from a csv file, with four columns:
#' Value (with pixel values), Label (with pixel labels), Short
#' (with short names to be used when one wants to refer to the pixel values),
#' and Color (strings with hexadecimal values, such as #1e33f4).
#' This function returns a tibble with these four columns.
#' @param legend_file A csv file with the legend. As default, it uses
#' the sits brazil legend.
#' @export
readLegend <- function(legend_file = NULL) {

  if (is.null(legend_file))
    legend_file <- system.file("extdata", "sits-brazil-legend.csv", package = "sits.validate")

  csv <- suppressMessages(
    readr::read_csv2(legend_file, col_types = readr::cols(
      Value = readr::col_double(),
      Parent = readr::col_double(),
      Label = readr::col_character(),
      Short = readr::col_character(),
      Color = readr::col_character()
    ))
  )

  dup <- which(duplicated(csv$Short))
  if (length(dup) > 0) {
    stop(paste0("There are duplicated short names: ", paste0(csv$Short[dup], collapse = ", ")))
  }

  dup <- which(duplicated(csv$Value))
  if (length(dup) > 0) {
    warning(paste0("There are duplicated values: ", paste0(csv$Value[dup], collapse = ", ")))
  }

  return(csv)
}

#' @title Compute a sublegend
#' @description Compute a sublegend based on a legend
#' the values available in a raster.
#' @param legend A Legend, which might be read from readLegend(), or also a sublegend
#' created from this function.
#' @param classes A vector of classes codes to filter the legend.
#' @export
subLegend <- function(legend, classes){

  missing <- which(!(classes %in% legend$Value))

  if (length(missing) > 0) {
    missing <- missing %>% paste(collapse = ", ")
    warning(paste0("The following values are missing in the legend: ", missing))
  }

  return(legend[legend$Value %in% classes,])
}

#' @title Create a style file
#' @description Create a QGIS style file from a legend.
#' @param legend A tibble with four columns as described in readLegend().
#' @param outputFile A file name to be created. It tipically has extension .qml.
#' @export
buildStyle <- function(legend, outputFile){
  header <- "<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>
  <qgis minScale=\"1e+8\" version=\"3.0.1-Girona\" hasScaleBasedVisibilityFlag=\"0\" maxScale=\"0\">
  <pipe>
  <rasterrenderer type=\"paletted\" band=\"1\" opacity=\"1\" alphaBand=\"-1\">
  <rasterTransparency/>
  <minMaxOrigin>
  <limits>None</limits>
  <extent>WholeRaster</extent>
  <statAccuracy>Estimated</statAccuracy>
  <cumulativeCutLower>0.02</cumulativeCutLower>
  <cumulativeCutUpper>0.98</cumulativeCutUpper>
  <stdDevFactor>2</stdDevFactor>
  </minMaxOrigin>
  <colorPalette>"

  lines <- legend %>% dplyr::transmute(line = paste0(
    "    <paletteEntry value=\"",
    Value,
    "\" color=\"",
    Color,
    "\" alpha=\"255\" label=\"",
    Value,
    ". ",
    Label,
    "\"/>\n"))

  lines <- paste0(lines$line, collapse="")

  footer<-  "</colorPalette>
  <colorramp type=\"randomcolors\" name=\"[source]\"/>
  </rasterrenderer>
  <brightnesscontrast contrast=\"0\" brightness=\"0\"/>
  <huesaturation grayscaleMode=\"0\" colorizeOn=\"0\" colorizeRed=\"255\" colorizeStrength=\"100\" colorizeGreen=\"128\" saturation=\"0\" colorizeBlue=\"128\"/>
  <rasterresampler maxOversampling=\"2\"/>
  </pipe>
  <blendMode>0</blendMode>
  </qgis>"

  write(paste0(header, lines, footer), outputFile)
  return(invisible())
}
