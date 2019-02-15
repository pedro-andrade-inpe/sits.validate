
csv <- read.csv(baseDir("gado-brasil.csv"), sep = ";", stringsAsFactors = FALSE)
munic <- sf::read_sf(baseDir("municipios/cerradoMunic.shp"))

colnames(csv)[1] <- "GEOCODIG_M"

result <- dplyr::full_join(munic, csv, by = "GEOCODIG_M") %>%
  dplyr::select(GEOCODIG_M, name_plain, X2000, X2015) %>%
  dplyr::mutate(X2000 = as.numeric(X2000), X2015 = as.numeric(X2015))

result

sf::write_sf(result, "cerrado-gado.shp")

sum(result$X2000, na.rm =TRUE) / 10e6 # 16.98
sum(result$X2015, na.rm =TRUE) / 10e6 # 21.52
