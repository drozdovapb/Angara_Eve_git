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

irkbox <- c(left=104.2, right=104.4, bottom=52.2, top=52.32)
IrkMap <- get_stamenmap(irkbox, zoom=11, maptype = "terrain") ##maybe also toner ## and terrain-bcgnd with less zoom?
#IrkMap <- get_googlemap(center=c(lat=104.3,lon=52.25), zoom=12, maptype = "satellite")
#IrkMap <- get_stamenmap(irkbox, zoom=12, maptype = "watercolor")
pIrkMap <- ggmap(IrkMap) + 
  xlab("Longitude") + ylab("Latitude") +
  theme_bw(base_size = 14) + 
  theme(plot.title = element_text(hjust = 0.5), plot.margin = margin(0.25, 0, 0, 0, "cm"))
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
      scale_fill_manual(values = c("#6CBB3C", "#4477AA"), name = "Haplogroup") + 
      ggtitle("COI") -> pIrkCOI
pIrkCOI
#ggsave("COI_Irk.png", width = 4, height = 4)
write.csv(pie_data_COI, "pie_data_COI.csv")


### and 18S
gather(coord_data[,c("coordinate", "spot", "lat", "lon", "18S")], key, value, -c(lat, lon, coordinate, spot)) %>% 
  count(coordinate, spot, lat, lon, key, value) %>% 
  spread(value, n, fill = 0) -> pie_data_18S

pie_data_18S
pIrkMap + 
  geom_scatterpie(data = pie_data_18S, cols=c("A", "S", "W"),
                  color = "white",
                  aes(x=lon, y=lat, group=coordinate, r =.005), pie_scale = .2) +
  ggtitle("18S") +  #coord_fixed() + 
  scale_fill_manual(values = c("#6CBB3C", "#4477AA"), name = "Haplogroup") -> pIrk18S
pIrk18S
ggsave("18S_Irk.png", width = 8, height = 4)
write.csv(pie_data_18S, "pie_data_18S.csv")



spots <- unique(coord_data[, c("lat", "lon", "spot")])
#spots$spotLR <- paste(spots$spot, ifelse(spots$coast=="left coast", "L", "R"))
#spots$nudge <- ifelse(spots$coast == "left", -.1, .1)
spots %>% group_by(spot) %>% summarise(lat=mean(lat),lon=mean(lon)) -> uniqSpots

library(ggrepel)
pIrkCOI + 
  geom_text_repel(data=uniqSpots, aes(x=lon, y=lat, label = spot), col = "grey20") -> pIrkCOI
pIrk18S + 
  geom_text_repel(data=uniqSpots, aes(x=lon, y=lat, label = spot), col = "grey20") -> pIrk18S

library(ggpubr)
ggarrange(pIrkCOI, pIrk18S, common.legend = TRUE)
ggsave("both_Irkutsk.png", width = 8, height=5)


## Angara map (upstream of Irkutsk)
upperbox <- c(left=104.4, right=105, bottom=51.7, top=52.2)
UpperMap <- get_stamenmap(upperbox, zoom=11, maptype = "terrain")

pUpper <- ggmap(UpperMap) + 
  xlab("Longitude") + ylab("Latitude") +
  theme_bw(base_size = 14) + 
  theme(plot.title = element_text(hjust = 0.5), plot.margin = margin(0.25, 0, 0, 0, "cm"))


pUpper + 
  geom_scatterpie(data = pie_data_18S, cols=c("A", "S", "W"), color = "white",
                  aes(x=lon, y=lat, group=coordinate, r =.025), pie_scale = .2) + 
  #coord_fixed() + 
  ggtitle("18S") + 
  scale_fill_manual(values = c("#6CBB3C", "#4477AA", "#F0E442"), name="Haplogroup") -> pUpper18S

pUpper + 
  geom_scatterpie(data = pie_data_COI, cols=c("A", "S", "W"), color = "white",
                  aes(x=lon, y=lat, group=coordinate, r =.025), pie_scale = .2) + 
  #coord_fixed() + 
  ggtitle("COI") + 
  scale_fill_manual(values = c("#6CBB3C", "#4477AA", "#F0E442"), name="Haplogroup") -> pUpperCOI

pUpper18S + geom_text_repel(data=uniqSpots, aes(x=lon, y=lat, label = spot), col = "grey20") -> pUpper18S
pUpperCOI + geom_text_repel(data=uniqSpots, aes(x=lon, y=lat, label = spot), col = "grey20") -> pUpperCOI

ggarrange(pUpperCOI, pUpper18S, common.legend = TRUE)
ggsave("both_upper.png", width = 8, height=5)


## Bratsk map (downstream of Irkutsk)
bratskbox <- c(left=101.5, right=102, bottom=56, top=56.4)
BratskMap <- get_stamenmap(bratskbox, zoom=11, maptype = "terrain")

pBratsk <- ggmap(BratskMap) + 
  xlab("Longitude") + ylab("Latitude") +
  theme_bw(base_size = 14) + 
  theme(plot.title = element_text(hjust = 0.5), plot.margin = margin(0.25, 0, 0, 0, "cm"))


pBratsk + 
  geom_scatterpie(data = pie_data_18S, cols=c("A", "S", "W"), color = "white",
                  aes(x=lon, y=lat, group=coordinate, r =.025), pie_scale = .2) + 
  #coord_fixed() + 
  ggtitle("18S") + 
  scale_fill_manual(values = c("#6CBB3C", "#4477AA", "#F0E442"), name="Haplogroup") -> pBratsk18S

pBratsk + 
  geom_scatterpie(data = pie_data_COI, cols=c("A", "S", "W"), color = "white",
                  aes(x=lon, y=lat, group=coordinate, r =.025), pie_scale = .2) + 
  #coord_fixed() + 
  ggtitle("COI") + 
  scale_fill_manual(values = c("#6CBB3C", "#4477AA", "#F0E442"), name="Haplogroup") -> pBratskCOI

pBratsk18S + geom_text_repel(data=uniqSpots, aes(x=lon, y=lat, label = spot), col = "grey20") -> pBratsk18S
pBratskCOI + geom_text_repel(data=uniqSpots, aes(x=lon, y=lat, label = spot), col = "grey20") -> pBratskCOI

ggarrange(pBratskCOI, pBratsk18S, common.legend = TRUE)
ggsave("both_Bratsk.png", width = 8, height=5)
