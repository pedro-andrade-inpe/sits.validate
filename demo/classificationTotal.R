
require(sits.validate)
require(ggplot2)

classificationFiles <- getTifFiles("classificacoes-final")

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

ggplot(result) +
  aes(x =Year, y = Total, group = Class, colour = Class) +
  geom_line(lwd = 1.5) +
  scale_fill_manual(values = values)
