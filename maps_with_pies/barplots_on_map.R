## Loosely based on this:
## https://stackoverflow.com/questions/36063043/how-to-plot-barchart-onto-ggplot2-map
## but there were many excessive packages, so I try to do it my way

library(ggmap) ## to draw the map
library(ggplot2) ## for drawing in general; ggmap should load it but still...
library(openxlsx) ## to read the data in Excel format
library(tidyr); library(dplyr) ## for data manipulation
library(reshape2)
##library(data.table) ## not sure
#library(rgeos)
#library(maptools)
library(grid) ## for inset
#library(gridExtra)
#library(ggrepel) ## tried for spot labels but didn't use


## Plot background maps of the three places
## Irkutsk
irkbox <- c(left=104.2, right=104.4, bottom=52.2, top=52.32)
#IrkMap <- get_map(irkbox, zoom=13, maptype = "terrain", source = "stamen", filename = "irkmap") ##terrain-background 10; with higher zoom some parts go missing
##terrain-background 10; with higher zoom some parts go missing
IrkMap <- get_map(irkbox, zoom=10, maptype = "terrain-background", source = "stamen", filename = "irkmap") 
pIrkMap <- ggmap(IrkMap) + 
  xlab("Longitude") + ylab("Latitude") +
  ## add arrow
  geom_segment(aes(x = 104.37, y = 52.22, xend = 104.335, yend = 52.235),
               arrow = arrow(length = unit(0.5, "cm")), 
               lineend='round', linejoin='bevel', linetype = "dotted",
               col="darkblue") +
  theme_bw(base_size = 14) + 
  theme(plot.title = element_text(hjust = 0.5, size=14), plot.margin = margin(0.25, 0, 0, 0, "cm"))
pIrkMap


## Angara map upstream of Irkutsk
upperbox <- c(left=104.45, right=104.95, bottom=51.75, top=52.15)
#UpperMap <- get_stamenmap(upperbox, zoom=11, maptype = "terrain")
UpperMap <- get_stamenmap(upperbox, zoom=11, maptype = "terrain-background")
pUpper <- ggmap(UpperMap) + 
  xlab("Longitude") + ylab("Latitude") +
  theme_bw(base_size = 14) + 
  geom_segment(aes(x = 104.72, y = 51.95, xend = 104.64, yend = 52),
               arrow = arrow(length = unit(0.5, "cm")), 
               lineend='round', linejoin='bevel', linetype = "dotted",
               col="darkblue") + 
  ggtitle ("Angara before Irkutsk") + 
  theme(plot.title = element_text(hjust = 0.5, size = 14), plot.margin = margin(0.25, 0, 0, 0, "cm"))
pUpper

## Bratsk map (downstream of Irkutsk)
bratskbox <- c(left=101.5, right=102, bottom=56, top=56.4)
#BratskMap <- get_stamenmap(bratskbox, zoom=11, maptype = "terrain")
BratskMap <- get_stamenmap(bratskbox, zoom=11, maptype = "terrain-background")
pBratsk <- ggmap(BratskMap) + 
  geom_segment(aes(x = 101.75, y = 56.1, xend = 101.75, yend = 56.2),
               arrow = arrow(length = unit(0.5, "cm")), 
               lineend='round', linejoin='bevel', linetype = "dotted",
               col="darkblue") + 
  xlab("Longitude") + ylab("Latitude") +
  theme_bw(base_size = 14/1) + 
  theme(plot.title = element_text(hjust = 0.5, size=14/1), plot.margin = margin(0.25, 0, 0, 0, "cm"))
pBratsk


## read the data
coord_data <- read.xlsx("../map/Haplogroups Eve.xlsx")
## get latitude and longitude from coordinate string
gsub(" E", "", coord_data$coordinate) %>% strsplit(" N, ") -> latlon
sapply(latlon, "[", 1) %>% as.numeric() -> lat
sapply(latlon, "[", 2) %>% as.numeric() -> lon
## and put them back to the main dataframe
coord_data$lat <- lat; coord_data$lon <- lon

## deal with spot names
coord_data$letter <- coord_data$abbreviation ## replaced with my ideas
spots <- unique(coord_data[, c("lat", "lon", "letter", "coast", "spot")]) 
## abbreviation=my abbrev, letter=SA's abbreviations
spots %>% group_by(letter,coast) %>% summarise(lat=mean(lat),lon=mean(lon)) -> uniqSpots
## decide how much to adjust coordinate for the spot names
uniqSpots$adjust.B <- ifelse(uniqSpots$coast=="right coast", 0.07, -0.07)
uniqSpots$adjust.Irk <- ifelse(uniqSpots$coast=="right coast", 0.015, -0.015)
uniqSpots$adjust.U <- ifelse(uniqSpots$coast=="right coast", 0.055, -0.055)


## data for COI (the same for 18S)
gather(coord_data[,c("coordinate", "spot", "coast", "lat", "lon", "COI")], key, value, -c(lat, lon, coordinate, coast, spot)) %>% 
  count(coordinate, spot, coast, lat, lon, key, value) %>% 
  spread(value, n, fill = 0) -> pie_data_COI
#pie_data_COI
## melt data to plot
pie_data_COIm <- melt(pie_data_COI, measure.vars = list("A", "S", "W"))
## decide how much to adjust coordinate for the pies
pie_data_COI$adjust.B <- ifelse(pie_data_COI$coast=="right coast", 0.015, -0.015)
pie_data_COI$adjust.Irk <- ifelse(pie_data_COI$coast=="right coast", 0.005, -0.005)
pie_data_COI$adjust.U <- ifelse(pie_data_COI$coast=="right coast", 0.02, -0.02)

## barplots: tried but it didn't look good
#bar.testplot_list <- 
#  lapply(unique(pie_data_COI$coordinate), function(x) { 
#    gt_plot <- ggplotGrob(
#      ggplot(pie_data_COIm[pie_data_COIm$coordinate == x,])+
#        geom_bar(aes(x=coordinate, y=value, fill=variable), #x=variable for dodge
#                 position= 'stack', stat='identity', col = "NA") +  ##position='dodge'
#        ylim(0, 12) + theme_minimal() + 
#        theme(line = element_blank()) +
#        scale_fill_manual(values = c("#66BB3C", "#4477AA", "#F0E442"))
#    )
#    panel_coords <- gt_plot$layout[gt_plot$layout$name == "panel",]
#    gt_plot[panel_coords$t:panel_coords$b, panel_coords$l:panel_coords$r]
#  })
#bar_annotation_list <- lapply(1:length(unique(pie_data_COI$coordinate)), function(x) 
#  inset(bar.testplot_list[[x]], 
#        xmin = pie_data_COI[x, "lon"] - 8e-3 + pie_data_COI[x, "adjust"],
#        xmax = pie_data_COI[x, "lon"] + 8e-3 + pie_data_COI[x, "adjust"],
#        ymin = pie_data_COI[x, "lat"] - 4e-3,
#        ymax = pie_data_COI[x, "lat"] + 4e-3) )
#pIrkMap.test2 <- Reduce("+", bar_annotation_list, pIrkMap.test)
#pIrkMap.test2 + geom_text_repel(data=uniqSpots, aes(x=lon, y=lat, label = letter), col = "black")
#ggsave("Irk_both_bars.svg", width = 6, height = 5)
#ggsave("Irk_both_bars.png", width = 5, height = 5)


pie.testplot_list <- 
  lapply(unique(pie_data_COI$coordinate), function(x) { 
    gt_plot <- ggplotGrob(
      ggplot(pie_data_COIm[pie_data_COIm$coordinate == x,])+
        geom_bar(aes(x=coordinate, y=value, fill=variable), 
                 stat='identity', col = "white") +  ##position='dodge'
        theme_minimal() + ## ylim(0, 12) can make equal
        coord_polar("y", start=0) + 
        theme(line = element_blank()) +
        scale_fill_manual(values = c("#66BB3C", "#4477AA", "#F0E442")) +
              theme(legend.position = "none", rect = element_blank(),
                    line = element_blank(), text = element_blank()) 
    )
    panel_coords <- gt_plot$layout[gt_plot$layout$name == "panel",]
    gt_plot[panel_coords$t:panel_coords$b, panel_coords$l:panel_coords$r]
  })

## the following shoould probably be made into a function...
pie_annotation_list <- lapply(1:length(unique(pie_data_COI$coordinate)), function(x) 
  inset(pie.testplot_list[[x]], 
                    xmin = pie_data_COI[x, "lon"] - 7e-3 + pie_data_COI[x, "adjust.Irk"],
                    xmax = pie_data_COI[x, "lon"] + 7e-3 + pie_data_COI[x, "adjust.Irk"],
                    ymin = pie_data_COI[x, "lat"] - 7e-3,
                    ymax = pie_data_COI[x, "lat"] + 7e-3) )


uniqSpots$lona.I <- uniqSpots$lon+uniqSpots$adjust.Irk
pIrkMap.test3 <- Reduce("+", pie_annotation_list, pIrkMap)
pIrkMap.test3 + geom_text(data=uniqSpots, aes(x=lona.I, y=lat, label = letter), col = "black") + 
  geom_point(data = pie_data_COIm[16:17,], aes(x = lat, y=lon, col=variable), size=6) + 
  scale_color_manual(values = c("#66BB3C", "#4477AA", "#F0E442"), name="Haplogroup") + 
  theme(legend.position = "right") -> pIrkMap.4
pIrkMap.4
#ggsave("Irk_both_pies.svg", width = 20, height = 20, units = "cm")
## ggsave png looks weird :(
#ggsave("Irk_both_pies.png", width = 20, height = 20, units = "cm", device=ragg::agg_png)
#png("Irk_both_pies.png", width=20, height=20, units="cm", res=300); print(pIrkMap.4); dev.off()

## this is the pie list for the bigger maps
pie_annotation_list <- lapply(1:length(unique(pie_data_COI$coordinate)), function(x) 
  inset(pie.testplot_list[[x]], 
        xmin = pie_data_COI[x, "lon"] - 2e-2 + pie_data_COI[x, "adjust.U"],
        xmax = pie_data_COI[x, "lon"] + 2e-2 + pie_data_COI[x, "adjust.U"],
        ymin = pie_data_COI[x, "lat"] - 2e-2,
        ymax = pie_data_COI[x, "lat"] + 2e-2) )

uniqSpots$lona <- uniqSpots$lon+uniqSpots$adjust.U

pUpper.2 <- Reduce("+", pie_annotation_list, pUpper)
#pUpper.2 + geom_text_repel(data=uniqSpots, aes(x=lon, y=lat, label = letter), col = "black")
pUpper.2 + 
  geom_text(data=uniqSpots, aes(x=lona, y=lat, label = letter), col = "black") +
  geom_point(data = pie_data_COIm, aes(x = lat, y=lon, col=variable), size=6) + 
  scale_color_manual(values = c("#66BB3C", "#4477AA", "#F0E442"), name="Haplogroup") + 
  theme(legend.position = "right") -> pUpper.3
pUpper.3
#ggsave("Upper_both_pies.svg", width = 20, height = 20, units="cm")
#ggsave("Upper_both_pies.png", width = 5, height = 5)
#png("Upper_both_pies.png", width=20, height=20, units="cm", res=300); print(pUpper.3); dev.off()


## and even bigger for Bratsk
pie_annotation_list <- lapply(1:length(unique(pie_data_COI$coordinate)), function(x) 
  inset(pie.testplot_list[[x]], 
        xmin = pie_data_COI[x, "lon"] - 3e-2 + pie_data_COI[x, "adjust.B"],
        xmax = pie_data_COI[x, "lon"] + 3e-2 + pie_data_COI[x, "adjust.B"],
        ymin = pie_data_COI[x, "lat"] - 3e-2,
        ymax = pie_data_COI[x, "lat"] + 3e-2) )
uniqSpots$lona <- uniqSpots$lon+uniqSpots$adjust.B
pBratsk.2 <- Reduce("+", pie_annotation_list, pBratsk)
#pBratsk.2 + geom_text_repel(data=uniqSpots, aes(x=lon, y=lat, label = letter), col = "black")
pBratsk.2 + geom_text(data=uniqSpots, aes(x=lona, y=lat, label = letter), col = "black")  + 
  geom_point(data = pie_data_COIm[pie_data_COIm$value>0 & pie_data_COI$spot=="Padun",], aes(x = lat, y=lon, col=variable), size=6) + 
  scale_color_manual(values = c("#66BB3C", "#4477AA", "#F0E442"), name="Haplogroup") + 
  theme(legend.position = "right") -> pBratsk.3
pBratsk.3
#ggsave("Bratsk_both_pies.svg", width = 20, height = 20, units="cm")
#ggsave("Bratsk_both_pies.png", width = 5, height = 5)
#png("Bratsk_both_pies.png", width=20, height=20, units="cm", res=300); print(pBratsk.3); dev.off()

library(ggpubr)

graphwidths <- c(1/(upperbox[2]-upperbox[1]), 1/(irkbox[2]-irkbox[1]), 1/(bratskbox[2]-bratskbox[1]))

#svg("all_maps_pies_terrain_bg.svg", width = 10, height = 7)
ggarrange(pUpper.3 + theme(legend.position='none'), 
          pIrkMap.4 + theme(legend.position = 'none') + ylab("") + ggtitle("Irkutsk"), 
          pBratsk.3+ylab("") + theme(legend.position = 'none') + ggtitle("Bratsk"), 
          common.legend = TRUE,
          nrow = 1, widths = c(1.11, 1.42, 1))
#dev.off()
#ggsave("all_maps_pies_terrain.svg", width = 25, height = 15, units = "cm")
#ggsave("all_maps_pies_terrain_bg.svg", width = 25, height = 15, units = "cm")
