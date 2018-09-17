
require(sits.validate)
require(ggplot2)

classificationFiles <- getTifFiles("classificacoes-pos-processamento")

getTotal <- function(file){
  year <- file %>%
    basename %>%
    substr(16, 19)

  myraster <- raster::raster(file)
  bs <- raster::blockSize(myraster)
  total <- data.frame()

  for(i in 1:bs$n){
    cat(paste0("Processing block ", i, "/", bs$n, "\n"))

    v <- raster::getValues(myraster, row = bs$row[i], nrows = bs$nrows[i]) %>%
      table %>%
      as.data.frame

    total <- rbind(total, v)
  }

  total <- tibble::as.tibble(total)
  total$Year <- year

  total
}

tt <- tibble::tibble()

for(file in classificationFiles){
  cat(paste0("Processing file '", basename(file), "'\n"))
  tt <- rbind(tt, getTotal(file))
}

names(tt)[1] <- "Class"

result <- tt %>%
  dplyr::group_by(Class, Year) %>%
  dplyr::summarize(Total = sum(Freq))

result %>% write.csv(., file = "cerrado-total.csv")

result <- read.csv("cerrado-total.csv") %>% tibble::as.tibble()

result$Class = paste(result$Class)

pal <- getPalette()

labels <- sapply(result$Class, function(value)
  pal[which(value == pal[,"value"]), "label"])

names(labels) <- NULL
result$Class <- labels

values <- pal[,"color"]
names(values) <- as.numeric(pal[,"value"])

values <- values[-c(1, 17)]

#################################################################
# Compute the total area of cerrado based on the pixels size
# and quantity
data = raster::raster(classificationFiles[1])
res = raster::res(data)
resol <- res[1] * res[2]

result$Total <- (result$Total * resol) / 1e6 / 1e6
###############  m2                      Km2   MKm2

names(values) = unique(result$Class)

ggplot(result) +
  aes(x = Year, y = Total, group = Class, colour = Class) +
  geom_line(lwd = 1.5) +
  scale_colour_manual(name = "Class", values = values) +
  theme_bw() +
  ylab("Total (Mkm^2)")

total_cerrado <- dplyr::filter(result, Year == 2016) %>%
  dplyr::filter(Class %in% c("1. Araguaia", "2. Campo_Cerrado", "3. Cerradao", "4. Cerrado", "5. Cerrado_Rupestre")) %>%
  dplyr::select(Total) %>%
  sum()
total_cerrado

urban <- dplyr::filter(result, Class == "15. Urban Area")
urban

water <- dplyr::filter(result, Class == "16. Water")
water


total <- result %>% dplyr::filter(Year == 2016) %>% dplyr::select(Total) %>% sum()
total

total_pasto <- dplyr::filter(result, Year == 2016 & Class == "9. Pasture") %>%
    dplyr::select(Total)
total_pasto

  dplyr::filter(Class %in% c("1. Araguaia", "2. Campo_Cerrado", "3. Cerradao", "4. Cerrado", "5. Cerrado_Rupestre")) %>%
  dplyr::select(Total) %>%
  sum()

