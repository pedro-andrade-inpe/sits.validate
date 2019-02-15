require(sits.validate)

plantado <- read.csv(baseDir("IBGE/milho-2-safras.csv"), sep = ";", skip = 4, as.is = TRUE)

years <- 2003:2017
classes <- c("Milho1", "Milho2")
qtde <- length(classes)

cols <- c("GEOCODIG_M", "Nome",
  paste0(rep(classes, length = length(years) * qtde), rep(years, each = qtde)))

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
  tm_polygons("Milho12010")

getCommodity <- function(data, commodity) sapply(years, function(year)
  sum(as.data.frame(data)[,paste0(commodity, year)], na.rm =TRUE) / 1e6
)

mdata <- munic # plantado #munic

df <- getCommodity(mdata, classes[1])

for(use in classes[-1]){
  df <- c(df, getCommodity(mdata, use))
}

ibge <- tibble::tibble(
  Class = rep(classes, each = length(years)),
  Year = rep(years, qtde),
  Total = df
)

ibgeTotal <- ibge %>% dplyr::filter(Year >= 2000) %>%
  dplyr::select(Class, Year, Total) %>%
  dplyr::group_by(Year) %>%
  dplyr::summarise(Total = sum(Total))

ibgeTotal$Class <- "TotalMilho"

ibgeTotal <- rbind(ibgeTotal, ibge)

ibgeCorn <- result %>%
  dplyr::filter(Class == "10. Soy_Corn") %>%
  dplyr::select(Class, Year, Total) %>%
  rbind(ibgeTotal)

ggplot(ibgeCorn) +
  aes(x = Year, y = Total, group = Class, colour = Class) +
  geom_line(lwd = 1.5)


