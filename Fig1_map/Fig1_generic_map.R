## load libraries
library(ggmap) ## for plotting the map. Cite!
library(openxlsx) ## for reading data in Excel format
library(dplyr); library(tidyr) ## may be needed for data rearrangement
## other packages 
#library(rgdal) ## for satellite images
#library(OpenStreetMap)

## read the data
coord_data <- read.xlsx("../data/Haplogroups Eve.xlsx")

## get latitude and longitude from coordinate string
gsub(" E", "", coord_data$coordinate) %>% strsplit(" N, ") -> latlon
sapply(latlon, "[", 1) %>% as.numeric() -> lat
sapply(latlon, "[", 2) %>% as.numeric() -> lon
## and put them back to the main dataframe
coord_data$lat <- lat; coord_data$lon <- lon

## 1. Baikal map
## get the borders
bbox <- c(left=floor(min(lon)), right=ceiling(max(lon)+1), bottom=floor(min(lat)), top=ceiling(max(lat)))

## download map tiles ((the higher the zoom  value, the more detailed is the map)
BaikalMap <- get_stamenmap(bbox, zoom=9, maptype = "terrain")
## here are the possible maptype options:  
#  "terrain", "terrain-background", "terrain-labels", "terrain-lines",
#  "toner", "toner-2010", "toner-2011", "toner-background", "toner-hybrid",
#  "toner-labels", "toner-lines", "toner-lite", "watercolor")
## for example:
#BaikalMapBW <- get_stamenmap(bbox, zoom=10, maptype = "toner")

pBaikalMap <- 
ggmap(BaikalMap) + xlab("Longitude") + ylab("Latitude") + 
  geom_rect(mapping=aes(xmin=104.45, xmax=105.05, ymin=51.75, ymax=52.15), fill='NA', color="black") + 
  geom_rect(mapping=aes(xmin=101.50, xmax=102.10, ymin=56.00, ymax=56.35), fill='NA', color="black") + 
  geom_point(data = coord_data, aes(x = lon, y = lat), alpha = .3, size = 5, col = "red", shape = '\U2691') +
  geom_rect(mapping=aes(xmin=104.19, xmax=104.41, ymin=52.18, ymax=52.32), fill='NA', color="black") + #irk
  theme_classic(base_size = 16) + theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

pBaikalMap


svg("BaikalMap.svg", width=5, height=8); print(pBaikalMap); dev.off()
#ggsave("BaikalMap.png", height = 10, width = 6)
#ggsave("BaikalMap.svg", height = 10, width = 6)
