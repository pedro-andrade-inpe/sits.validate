require(sits.validate)

plantado <- read.csv(baseDir("IBGE/area-plantada.csv"), sep = ";", skip = 4, as.is = TRUE)

years <- 2000:2015

cols <- c("Nivel", "GEOCODIG_M", "Nome",
  paste0(rep(c("Total", "Algodao", "Milho", "Soja"), length = 16 * 4), rep(years, each = 4)))

colnames(plantado) <- cols

for(pos in 4:length(cols)){
  plantado[,pos] <- as.numeric(plantado[,pos])
}

plantado[is.na(plantado)] <- 0

munic <- sf::read_sf(baseDir("municipios/cerradoMunic.shp"))

munic <- munic %>%
  dplyr::left_join(plantado, by = "GEOCODIG_M")

require(tmap)

tm_shape(munic) +
  tm_polygons("Algodao2010")

getCommodity <- function(data, commodity) sapply(years, function(year)
  sum(as.data.frame(data)[,paste0(commodity, year)]) / 1e6
)

mdata <- munic # plantado #munic

algodao <- getCommodity(mdata, "Algodao")
milho <- getCommodity(mdata, "Milho")
soja <- getCommodity(mdata, "Soja")

df <- data.frame(algodao, milho, soja)
rownames(df) <- years

ibge <- tibble::tibble(
  Class = rep(c("Algodao IBGE", "Milho IBGE", "Soja IBGE"), each = length(years)),
  Year = rep(years, 3),
  Total = c(algodao, milho, soja)
)
