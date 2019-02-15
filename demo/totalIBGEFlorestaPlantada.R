require(sits.validate)
require(sp)
require(sf)
require(tmap)

# Produção da Extração Vegetal e da Silvicultura - PEVS
plantado <- read.csv(baseDir("IBGE/florestas_plantadas_ibge_2014_2016.csv"), sep = ";", as.is = TRUE)

colnames(plantado) <- c("Ano", "Pais", "Regiao", "Estado", "Sigla", "Municipio",
                        "MunicEstado", "Latitude", "Longitude", "Especie", "Area")

# plantado$Latitude[640:660] # something like "1,234,567,890"

plantado$Latitude <- plantado$Latitude %>%
  stringr::str_replace(",", ".") %>%
  substr(1, 5) %>%
  as.numeric()

plantado$Longitude <- plantado$Longitude %>%
  stringr::str_replace(",", ".") %>%
  substr(1, 5) %>%
  as.numeric()

plantado$Ano <- plantado$Ano %>%
  substr(7, 10) %>%
  as.numeric()

plantado$Longitude[which(plantado$Longitude == -3.57)] <- -35.7
plantado$Latitude[which(plantado$Latitude > 200)] <- 2.272

plantado <- plantado %>%
  dplyr::filter(Area > 0)

#plantado$Area <- plantado$Area %>%
#  haToMha()

# todo o Brasil
plantado %>%
  dplyr::group_by(Ano) %>%
  dplyr::summarise(Area = sum(Area))

munic <- sf::read_sf(baseDir("municipios/cerradoMunic.shp"))

coordinates(plantado) = ~Longitude+Latitude
plantado.sf = st_as_sf(plantado) %>%
  st_set_crs(4326) %>%
  st_transform(st_crs(munic))

st_write(plantado.sf, "floresta-plantada-ibge.shp")

for(year in plantado.sf$Ano %>% unique()){
  intersects <- st_intersects(munic, plantado.sf %>% dplyr::filter(Ano == year))

  areas <- sapply(1:length(intersects),
    function(i) plantado.sf[intersects[[i]], ]$Area %>% sum()
  )

  munic[, paste0("For_", year)] = areas
}


plotAll <- function(data, prefix, title = prefix, slices = 5, palette = "RdPu", additional = NULL, extension = "pdf", ...){
  attributes <- colnames(data) %>%
    stringr::str_sub(1, nchar(prefix)) %>%
    (function(.) colnames(data)[which(. == prefix)])

  palette <- RColorBrewer::brewer.pal(slices, palette)
  values <- as.data.frame(data)[,attributes]

  vmax <- max(values)
  vmin <- min(values)

  cuts <- seq(vmin, vmax, length.out = slices)

  for(attribute in attributes){
    cat(paste0("Processing attribute '", attribute, "'\n"))

    suffix <- stringr::str_sub(attribute, nchar(prefix) + 1, nchar(attribute))
    mtitle <- paste(title, suffix)

    m <- tm_shape(data) +
      tm_fill(col = attribute, palette = palette, breaks = cuts, title = mtitle) +

      additional

    tmap_save(m, paste0(attribute, ".", extension))
  }
}

plotAll(munic, "For_",
        slices = 5,
        palette = "RdPu",
        title = "Forest",
        additional = tm_borders(lwd = 1, col = "black")
)
