## Loosel based on this:
## https://stackoverflow.com/questions/36063043/how-to-plot-barchart-onto-ggplot2-map
## but there were many excessive packages, so I try to do it my way

library(ggmap) ## to draw the map
#library(rgdal)
library(ggplot2) ## for drawing in general; ggmap should load it but still...
library(openxlsx) ## to read the data in Excel format
library(tidyr); library(dplyr) ## for data manipulation
library(reshape2)
##library(data.table) ## not sure
#library(rgeos)
#library(maptools)
library(grid) ## for inset
#library(gridExtra)


#Plot test map
irkbox <- c(left=104.2, right=104.4, bottom=52.2, top=52.32)
IrkMap <- get_stamenmap(irkbox, zoom=11, maptype = "terrain") ##maybe also toner ## and terrain-bcgnd with less zoom?
## test map
pIrkMap.test <- ggmap(IrkMap) + 
  xlab("Longitude") + ylab("Latitude") +
  theme_bw(base_size = 14) + 
  theme(plot.title = element_text(hjust = 0.5), plot.margin = margin(0.25, 0, 0, 0, "cm"))
pIrkMap.test



## read the data
coord_data <- read.xlsx("../map/Haplogroups Eve.xlsx")

## get latitude and longitude from coordinate string
gsub(" E", "", coord_data$coordinate) %>% strsplit(" N, ") -> latlon
sapply(latlon, "[", 1) %>% as.numeric() -> lat
sapply(latlon, "[", 2) %>% as.numeric() -> lon
## and put them back to the main dataframe
coord_data$lat <- lat; coord_data$lon <- lon

## data
gather(coord_data[,c("coordinate", "spot", "coast", "lat", "lon", "COI")], key, value, -c(lat, lon, coordinate, coast, spot)) %>% 
  count(coordinate, spot, coast, lat, lon, key, value) %>% 
  spread(value, n, fill = 0) -> pie_data_COI
pie_data_COI

pie_data_COIm <- melt(pie_data_COI, measure.vars = list("A", "S", "W"))

pie_data_COI$adjust <- ifelse(pie_data_COI$coast=="right coast", 0.005, -0.005)

## bars
bar.testplot_list <- 
  lapply(unique(pie_data_COI$coordinate), function(x) { 
    gt_plot <- ggplotGrob(
      ggplot(pie_data_COIm[pie_data_COIm$coordinate == x,])+
        geom_bar(aes(x=coordinate, y=value, fill=variable), #x=variable for dodge
                 position= 'stack', stat='identity', col = "NA") +  ##position='dodge'
        ylim(0, 12) + theme_minimal() + 
        theme(line = element_blank()) +
        scale_fill_manual(values = c("#66BB3C", "#4477AA", "#F0E442"))
    )
    panel_coords <- gt_plot$layout[gt_plot$layout$name == "panel",]
    gt_plot[panel_coords$t:panel_coords$b, panel_coords$l:panel_coords$r]
  })

bar_annotation_list <- lapply(1:length(unique(pie_data_COI$coordinate)), function(x) 
  inset(bar.testplot_list[[x]], 
        xmin = pie_data_COI[x, "lon"] - 6e-3 + pie_data_COI[x, "adjust"],
        xmax = pie_data_COI[x, "lon"] + 6e-3 + pie_data_COI[x, "adjust"],
        ymin = pie_data_COI[x, "lat"] - 3e-3,
        ymax = pie_data_COI[x, "lat"] + 3e-3) )

Reduce("+", bar_annotation_list, pIrkMap.test)
ggsave("Irk_both_bars.svg", width = 5, height = 5)
#ggsave("Irk_both.png", width = 5, height = 5)


## These would be dodge variables just in case
#bar.testplot_list <- 
#  lapply(unique(pie_data_COI$coordinate), function(x) { 
#    gt_plot <- ggplotGrob(
#      ggplot(pie_data_COIm[pie_data_COIm$coordinate == x,])+
#        geom_bar(aes(x=variable, y=value, fill=variable), #x=variable for dodge
#                 position= 'dodge', stat='identity', col = "white") +  ##position='dodge'
#        ylim(0, 12) + theme_minimal() + 
#        theme(line = element_blank()) +
#        scale_fill_manual(values = c("#66BB3C", "#4477AA", "#F0E442"))
      #+
      #labs(x = NULL, y = NULL) + 
      #        theme(legend.position = "none", rect = element_blank(),
      #              line = element_blank(), text = element_blank()) 
#    )
#    panel_coords <- gt_plot$layout[gt_plot$layout$name == "panel",]
#    gt_plot[panel_coords$t:panel_coords$b, panel_coords$l:panel_coords$r]
#  })

## Now pies, which are in fact rounded bars :)

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
      #+
      #labs(x = NULL, y = NULL) + 
              theme(legend.position = "none", rect = element_blank(),
                    line = element_blank(), text = element_blank()) 
    )
    panel_coords <- gt_plot$layout[gt_plot$layout$name == "panel",]
    gt_plot[panel_coords$t:panel_coords$b, panel_coords$l:panel_coords$r]
  })




pie_annotation_list <- lapply(1:length(unique(pie_data_COI$coordinate)), function(x) 
  inset(pie.testplot_list[[x]], 
                    xmin = pie_data_COI[x, "lon"] - 7e-3 + pie_data_COI[x, "adjust"],
                    xmax = pie_data_COI[x, "lon"] + 7e-3 + pie_data_COI[x, "adjust"],
                    ymin = pie_data_COI[x, "lat"] - 7e-3,
                    ymax = pie_data_COI[x, "lat"] + 7e-3) )

Reduce("+", pie_annotation_list, pIrkMap.test)
ggsave("Irk_both_pies.svg", width = 5, height = 5)
#ggsave("Irk_both_pies.png", width = 5, height = 5)
ggsave("Irk_both_pies.pdf", width = 5, height = 5)


## still need to add place names!