library(ggmap) ## for plotting the map. Cite!
#library(scales)
library(openxlsx)
library(dplyr)
library(ggtree)
library(scatterpie) ## for pies on the map. Cite!
library(tidyr)
#library(rgdal) ## for satellite images
#library(OpenStreetMap)

## read the data
coord_data <- read.xlsx("Haplogroups Eve.xlsx")

## get latitude and longitude from coordinate string
gsub(" E", "", coord_data$coordinate) %>% strsplit(" N, ") -> latlon
sapply(latlon, "[", 1) %>% as.numeric() -> lat
sapply(latlon, "[", 2) %>% as.numeric() -> lon
## and put them back to the main dataframe
coord_data$lat <- lat; coord_data$lon <- lon

## 1. Baikal map
## get the borders
bbox <- c(left=floor(min(lon)), right=ceiling(max(lon)+1), bottom=floor(min(lat)), top=ceiling(max(lat)))

## download map tiles
BaikalMap <- get_stamenmap(bbox, zoom=10, maptype = "terrain")
BaikalMapBW <- get_stamenmap(bbox, zoom=10, maptype = "toner")

## some unused code; trying openmap
#BaikalMapO <- openmap(c(lat=ceiling(max(lat)), lon=floor(min(lon))), 
#        c(lat=floor(min(lat)), lon=ceiling(max(lon)+1)),
#        minNumTiles=9,type="osm")


ggmap(BaikalMapBW) + xlab("Longitude") + ylab("Latitude") +
  geom_point(data = coord_data, aes(x = lon, y = lat), alpha = 1, size = 6, col = "red", shape = '\U2691') +
  theme_classic(base_size = 22) + theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
ggsave("BaikalMapBW.png", height = 10, width = 6)

ggmap(BaikalMap) + xlab("Longitude") + ylab("Latitude") +
  geom_point(data = coord_data, aes(x = lon, y = lat), alpha = 1, size = 6, col = "red", shape = '\U2691') +
  theme_classic(base_size = 22) + theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
ggsave("BaikalMap.png", height = 10, width = 6)


## 2. Maps of particular locations with pies

irkbox <- c(left=104.25, right=104.37, bottom=52.2, top=52.3)
IrkMap <- get_stamenmap(irkbox, zoom=11, maptype = "terrain") ##maybe also toner ## and terrain-bcgnd with less zoom?
#IrkMap <- get_googlemap(center=c(lat=104.3,lon=52.25), zoom=12, maptype = "satellite")
#IrkMap <- get_stamenmap(irkbox, zoom=12, maptype = "watercolor")
pIrkMap <- ggmap(IrkMap) + xlab("Longitude") + ylab("Latitude")
pIrkMap
  
## COI
gather(coord_data[,c("coordinate", "spot", "lat", "lon", "COI")], key, value, -c(lat, lon, coordinate, spot)) %>% 
  count(coordinate, spot, lat, lon, key, value) %>% 
  spread(value, n, fill = 0) -> pie_data_COI
pie_data_COI

#ggmap(IrkMap) + ##, extent = "device") + 
pIrkMap + 
  geom_scatterpie(data = pie_data_COI, cols=c("A", "S", "W"), color = "white",
                  aes(x=lon, y=lat, group=coordinate, r =.005), pie_scale = .2) + 
      scale_fill_manual(values = c("forestgreen", "#4477AA"), name = "Haplogroup") + 
      theme_bw(base_size = 14) + 
      theme(title = element_text(hjust = 1), aspect.ratio = 1) + #coord_quickmap(expand = FALSE)
      coord_fixed() + ggtitle("COI")
ggsave("COI_Irk.png", width = 4, height = 4)
write.csv(pie_data_COI, "pie_data_COI.csv")


### and 18S
gather(coord_data[,c("coordinate", "spot", "lat", "lon", "18S")], key, value, -c(lat, lon, coordinate, spot)) %>% 
  count(coordinate, spot, lat, lon, key, value) %>% 
  spread(value, n, fill = 0) -> pie_data_18S

pie_data_18S
ggmap(IrkMap) + 
  geom_scatterpie(data = pie_data_18S, cols=c("A", "S", "W"),
                  aes(x=lon, y=lat, group=coordinate, r =.003)) + 
  coord_fixed() + ggtitle("18S") + 
  scale_fill_manual(values = c("green", "blue"))
ggsave("18S_Irk.png", width = 8, height = 4)
write.csv(pie_data_18S, "pie_data_18S.csv")

## we can try with long_format ## nope, doesn't work
#ggmap(IrkMap) + 
#  geom_scatterpie(data = coord_data, cols="COI", long_format = TRUE,
#                  aes(x=lon, y=lat, r =.003)) + 
#  coord_fixed() + ggtitle("COI")


## Angara map (upstream of Irkutsk)
upperbox <- c(left=104, right=105, bottom=51.5, top=52.2)
UpperMap <- get_stamenmap(upperbox, zoom=12, maptype = "terrain")
ggmap(UpperMap) + 
  geom_scatterpie(data = pie_data_18S, cols=c("A", "S", "W"),
                  aes(x=lon, y=lat, group=coordinate, r =.003)) + 
  coord_fixed() + ggtitle("18S") + 
  scale_fill_manual(values = c("green", "blue"))

ggmap(UpperMap) + 
  geom_scatterpie(data = coord_data, cols="COI", long_format = TRUE,
                  aes(x=lon, y=lat, r =.003)) + 
  coord_fixed() + ggtitle("COI")

ggmap(UpperMap) + 
  geom_scatterpie(data = coord_data, cols="18S", long_format = TRUE,
                  aes(x=lon, y=lat, r =.003)) + 
  coord_fixed() + ggtitle("18S")

