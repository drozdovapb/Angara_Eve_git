library(tanggle)
library(dplyr) ## for some data rearrangement
library(phangorn)

## read the data = nexus file recorded with SplitsTree4 (!)
Nnet <- read.nexus.networx("../data/COI_Eve_wnewout.fa.nex")

pn <- 
  ggsplitnet(Nnet, col="black") + 
  geom_treescale(x=-.04, y=.035, offset=.001) + 
  coord_fixed()
#  geom_tiplab2() + 
#  geom_point(aes(shape="21", color=isTip), size=2)

tips <- pn$data[pn$data$isTip, ]
tips$group <- sapply(tips$label, function(x) strsplit(x, split = "_")[[1]][2])
#tips$group[which(tips$group %in% c("Kr", "Ya", "Ko", "So", "Mp", "To", "BL"))] <- "Angara"
tips$group[which(nchar(tips$group) == 2)] <- "Angara"
tips$group[which(tips$group %in% c("Olkhon", "Baikalskoe"))] <- "W"
tips$group[which(tips$group %in% c("Listv"))] <- "out" ## this is Evi in our case
tips$group[which(tips$group %in% c("Davsha", "SvyatoyNos"))] <- "E"
tips$group[which(tips$group %in% c("Baikalsk", "Kultuk"))] <- "S"

tips %>% count(x, y, group) -> tips.occur

#tips$place <- sapply(tips$label, function(x) strsplit(x, split = "_")[[1]][1])
#tips$place <- ifelse(tips$place == "E" | tips$place == "Evi", "Lake Baikal", "Angara River")


tips$place <- sapply(tips$label, function(x) strsplit(x, split = "_")[[1]][1])
tips$place <- ifelse(startsWith(tips$place, "MK") | tips$place == "Evi", "Lake Baikal", "Angara River")


tips %>% count(x, y, group, place) -> tips.occur
tips.occur <- tips.occur[order(tips.occur$n), ]

pn + 
  geom_point(data = tips.occur, aes(x=x, y=y, color = place, fill=group, shape=place), size=6) + 
  scale_fill_manual(values = c("#66BB3C", "#D81B60", "grey50", "#4477AA", "#F0E442"), 
                    name="Haplogroup") +
  expand_limits(x=.03) + 
  scale_color_manual(values=c("white", "black"), name = "Place") + 
  #  scale_color_manual(values = c("#66BB3C", "#D81B60", "grey50", "#4477AA", "#F0E442"), 
  #                    name="Haplogroup") +
  scale_shape_manual(values = c(21, 22), name = "Place") +
  guides(fill = guide_legend(override.aes=list(shape=21)),
         color = guide_legend(override.aes=list(col="black"))) + 
  theme(legend.position = "left") -> pnCOI
pnCOI

## and 18S

## read the data = nexus file recorded with SplitsTree4 (!)
network <- read.nexus.networx("../data/18S.nex")

pn <- 
  ggsplitnet(network, col = "black") + 
  geom_treescale(x=.001, y=.0001, offset=.0001, width = 1e-3)+ 
  coord_fixed() #+
#  geom_tiplab2() + 
#  geom_point(aes(shape="21", color=isTip), size=2)
pn

tips <- pn$data[pn$data$isTip, ]
tips$group <- sapply(tips$label, function(x) strsplit(x, split = "_")[[1]][2])
#tips$group[which(tips$group %in% c("Kr", "Ya", "Ko", "So", "Mp", "To", "BL"))] <- "Angara"
tips$group[which(nchar(tips$group) == 2)] <- "Angara"
tips$group[which(tips$group %in% c("Olkhon", "Baikalskoe"))] <- "W"
tips$group[which(tips$group %in% c("Listv"))] <- "out" ## this is Evi in our case
tips$group[which(tips$group %in% c("Davsha", "SvyatoyNos"))] <- "E"
tips$group[which(tips$group %in% c("Baikalsk", "Kultuk"))] <- "S"

tips %>% count(x, y, group, place) -> tips.occur
tips.occur <- tips.occur[order(tips.occur$n), ]

tips.occur$group <- factor(tips.occur$group, levels = c("A", "E", "S", "W", "Outgroup"))

pn + 
  geom_point(data = tips.occur, aes(x=x, y=y, color = place, fill=group, shape=place), size=6) + 
  scale_fill_manual(values = c("#66BB3C", "#D81B60", "#4477AA", "#F0E442", "grey50"), 
                    name="Haplogroup") +
  #expand_limits(x=-.005) + 
  scale_color_manual(values=c("white", "black"), name = "Place") + 
  #  scale_color_manual(values = c("#66BB3C", "#D81B60", "grey50", "#4477AA", "#F0E442"), 
  #                    name="Haplogroup") +
  scale_shape_manual(values = c(21, 22), name = "Place") +
  guides(fill = guide_legend(override.aes=list(shape=c(21, 21, 21, 21, 22))),
         color = guide_legend(override.aes=list(col="black"))) +
  theme(legend.text = element_text(size = 12))   -> pn18S
pn18S

library(ggpubr)
png("Figure_2_color_by_place.png", width = 20, height = 10, res=600, units="cm")
ggarrange(pnCOI+theme(legend.position = "none"), pn18S, 
          labels = c("A", "B"))
dev.off()

svg("Figure_2_color_by_place.svg", width = 7.87, height = 3.94)
ggarrange(pnCOI+theme(legend.position = "none"), pn18S, 
          labels = c("A", "B"))
dev.off()


