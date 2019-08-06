
buildStyle <- function(csv, outputFile){
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

  lines <- csv %>% transmute(line = paste0(
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