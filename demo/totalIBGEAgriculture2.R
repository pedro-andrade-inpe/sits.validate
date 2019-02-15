require(sits.validate)

plantado <- read.csv(baseDir("IBGE/area-plantada2.csv"), sep = ";", skip = 4, as.is = TRUE)

years <- 1974:2017
classes <- c("Algodao", "Arroz", "Centeio", "Cevada", "Feijao", "Milho", "Soja", "Sorgo", "Trigo")

cols <- c("GEOCODIG_M", "Nome",
  paste0(rep(classes, length = length(years) * 9), rep(years, each = 9)))

colnames(plantado) <- cols

for(pos in 3:length(cols)){
  plantado[,pos] <- as.numeric(plantado[,pos])
}

plantado[is.na(plantado)] <- 0
plantado$GEOCODIG_M <- as.numeric(plantado$GEOCODIG_M)

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

df <- getCommodity(mdata, "Algodao")

for(use in c("Arroz", "Centeio", "Cevada", "Feijao", "Milho", "Soja", "Sorgo", "Trigo")){
  df <- c(df, getCommodity(mdata, use))
}

ibge <- tibble::tibble(
  Class = rep(classes, each = length(years)),
  Year = rep(years, 9),
  Total = df
)

ibge <- ibge %>% dplyr::filter(Year >= 2000)

ggplot(ibge) +
  aes(x = Year, y = Total, group = Class, colour = Class) +
  geom_line(lwd = 1.5)

sojaplys <- ibge %>%
  dplyr::filter(Class %in% c("Soja", "Sorgo", "Feijao", "Arroz")) %>%
  dplyr::filter(Year <= 2015) %>%
  dplyr::group_by(Year) %>%
  dplyr::summarise(Total = sum(Total))

sojaplys$Class <- "SojaSorFeiArr"

# run after totalIBGEAgrig

ibge <- ibge %>%
  dplyr::filter(Class %in% c("Soja", "Milho", "Algodao")) %>%
  dplyr::filter(Year <= 2015)

ibge <- rbind(ibge, sojaplys)

