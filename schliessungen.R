library(XML)
library(ggplot2)
library(maptools)
gpclibPermit()
library(raster)

# load data
data <- readLines("schliessungen-schlecker_neu.txt")

# extract cities and streets
city <- data[seq(1,6028,3)]
street <- substr(data[seq(2,6029,3)], 2, 100)

# Yahoo geocoding - 468 NAs
for (i in 1:length(city)) {
  url <- paste("http://where.yahooapis.com/geocode?q=", street[i], ", ", city[i], "&appid=ClTAQy7a", sep = "")
  #  url <- URLencode(url)
  doc <- xmlTreeParse(url, useInternal=TRUE)
  top <- xmlRoot(doc)
  found <- as.numeric(xmlValue(top[[5]]))
  
  # check if a loacation can be found
  if (found == 1) {
    lat <- as.numeric(xmlValue(top[[6]] [[ "latitude"]]))
    long <- as.numeric(xmlValue(top[[6]] [[ "longitude"]]))
    print(i)
  } else {
    lat <- NA
    long <- NA
  }
  
  if (i == 1) {
    df <- data.frame(city[i], street[i], lat, long)
  } else {
    df <- rbind(df, data.frame(city[i], street[i], lat, long))
  }
  Sys.sleep(0.5)
}

yahoo_df <- df


# Google geocoding - 77 NAs
for (i in 1:length(city)) {
  url <- paste("http://maps.googleapis.com/maps/api/geocode/xml?address=", street[i], ", ", city[i], "&sensor=false", sep = "")
  #  url <- URLencode(url)
  doc <- xmlTreeParse(url, useInternal=TRUE)
  top <- xmlRoot(doc)
  found <- xmlValue(top[[1]])
  
  # check if a location could be found
  if (found == "OK") {
    lat <- as.numeric(xmlValue(top[[2]] [[ "geometry"]] [[ "location" ]] [[ "lat" ]]))
    long <- as.numeric(xmlValue(top[[2]] [[ "geometry"]] [[ "location" ]] [[ "lng" ]]))
    print(i)
  } else {
    lat <- NA
    long <- NA
  }
  
  if (i == 1) {
    df <- data.frame(city[i], street[i], lat, long)
  } else {
    df <- rbind(df, data.frame(city[i], street[i], lat, long))
  }
  Sys.sleep(0.5)
}

google_df <- df

# 1 Outsider
google_df$long[google_df$long < 0] <- NA

# save data
save(yahoo_df, google_df, file = "data.RData")


# load German map
gadm <- getData('GADM', country="DE", level=1)
gadm_2 <- fortify(gadm, "NAME_1")

# plot
plot <- ggplot(gadm_2, aes(long, lat)) + geom_path(aes(group = group), colour = "grey30", size = 0.5)
plot <- plot + geom_point(data = google_df, aes(x = long, y = lat), colour = "#0c367b", alpha = 0.75, size = 2)
plot <- plot + coord_map()
plot <- plot + theme_bw() + scale_x_continuous(breaks = NA) + scale_y_continuous(breaks = NA) + labs(x = NULL, y = NULL)
plot <- plot + opts(title = "Closing Schlecker stores", plot.title = theme_text(size = 14, face = "bold"))
plot

pdf("schlecker_closing.pdf", width = 6, height = 8)
print(plot)
dev.off()

png("schlecker_closing.png", width = 500, height = 700)
print(plot)
dev.off()

png("schlecker_closing_big.png", width = 1500, height = 2100)
print(plot)
dev.off()


