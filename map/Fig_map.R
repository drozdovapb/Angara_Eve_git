library("ggmap")
library(scales)
library(openxlsx)
library(dplyr)
library(ggtree)
library(scatterpie) ## for pies on the map. Cite!
library(tidyr)
library(rgdal) ## for satellite images
library(OpenStreetMap)

coord_data <- read.xlsx("Haplogroups Eve.xlsx")

gsub(" E", "", coord_data$coordinate) %>% strsplit(" N, ") -> latlon
sapply(latlon, "[", 1) %>% as.numeric() -> lat
sapply(latlon, "[", 2) %>% as.numeric() -> lon

## get the borders
min(lat)
bbox <- c(left=floor(min(lon)), right=ceiling(max(lon)+1), bottom=floor(min(lat)), top=ceiling(max(lat)))


BaikalMap <- get_stamenmap(bbox, zoom=10, maptype = "terrain")
BaikalMapBW <- get_stamenmap(bbox, zoom=10, maptype = "toner")

#BaikalMapO <- openmap(c(lat=ceiling(max(lat)), lon=floor(min(lon))), 
#        c(lat=floor(min(lat)), lon=ceiling(max(lon)+1)),
#        minNumTiles=9,type="osm")



coord_data$lat <- lat; coord_data$lon <- lon

ggmap(BaikalMapBW) + xlab("Longitude") + ylab("Latitude") +
  geom_point(data = coord_data, aes(x = lon, y = lat), alpha = 1, size = 1, col = "red", shape = 1) +
  theme_classic(base_size = 22) + theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
ggsave("BaikalMapBW.png", height = 10, width = 6)


#ggmap(AngaraMap) + xlab("Longitude") + ylab("Latitude") +
#  scale_y_continuous(breaks = pretty_breaks(n = 2)) + 
#  xlab("") + ylab("") + 
#  geom_point(data = sampling_points, aes(x = lon, y = lat), col = sampling_points$col, alpha = 0.5, size = 5) + 
#  theme_classic(base_size = 24) + theme(panel.border = element_rect(colour = "grey50", fill=NA, size=1)) 


irkbox <- c(left=104.25, right=104.37, bottom=52.2, top=52.3)
IrkMap <- get_stamenmap(irkbox, zoom=14, maptype = "terrain") ##maybe also toner ## and terrain-bcgnd with less zoom?
#IrkMap <- get_googlemap(center=c(lat=104.3,lon=52.25), zoom=12, maptype = "satellite")
#IrkMap <- get_stamenmap(irkbox, zoom=12, maptype = "watercolor")
pIrkMap <- ggmap(IrkMap) + xlab("Longitude") + ylab("Latitude")
pIrkMap
  
## COI
gather(coord_data[,c("coordinate", "spot", "lat", "lon", "COI")], key, value, -c(lat, lon, coordinate, spot)) %>% 
  count(coordinate, spot, lat, lon, key, value) %>% 
  spread(value, n, fill = 0) -> pie_data_COI
pie_data_COI

ggmap(IrkMap) + ##, extent = "device") + 
  geom_scatterpie(data = pie_data_COI, cols=c("A", "S", "W"), color = "white",
                  aes(x=lon, y=lat, group=coordinate, r =.003)) + 
      coord_fixed() + ggtitle("COI") + 
      scale_fill_manual(values = c("forestgreen", "#4477AA"))
ggsave("COI_Irk.png", width = 8, height = 4)
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
ggmap(IrkMap) + 
  geom_scatterpie(data = coord_data, cols="COI", long_format = TRUE,
                  aes(x=lon, y=lat, r =.003)) + 
  coord_fixed() + ggtitle("COI")


## Angara map (upstream of Irkutsk)
upperbox <- c(left=104, right=105, bottom=51.5, top=52.2)
UpperMap <- get_stamenmap(upperbox, zoom=12, maptype = "terrain")
ggmap(UpperMap) + 
  geom_scatterpie(data = pie_data_18S, cols=c("A", "S", "W"),
                  aes(x=lon, y=lat, group=coordinate, r =.003)) + 
  coord_fixed() + ggtitle("18S") + 
  scale_fill_manual(values = c("green", "blue"))


