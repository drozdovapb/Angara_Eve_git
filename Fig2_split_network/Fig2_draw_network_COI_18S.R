library("phangorn")
library("tanggle")

## read the data = nexus file recorded with SplitsTree4 (!)
network <- read.nexus.networx("../data/COI.nex")

## this just reproduces the network produced in SplitsTree4
plot(network, type="2D", show.tip.label=TRUE, show.node.label=TRUE,
     edge.width=0.5, cex=0.5, scale.bar = 1)#, tip.color=adjustcolor(lvc, alpha.f=0.75))


## 
## assign groups by shape
## unicode shapes
  ## https://www.fileformat.info/info/unicode/block/geometric_shapes/list.htm
shape.group <- c()#rep("", nn)
shape.group[network$translate$node] <- c("\U2B24") ##circle
these.refs <- c(grep("E_", network$translate$label, fixed=TRUE), 
                grep("Evi_", network$translate$label, fixed=TRUE))
shape.group[these.refs] <- c("\U25A0") ##squares for refs


## assign groups by color
## here's an example of our pattern
network$translate$label
group <- sapply(network$translate$label, function(x) {strsplit(x, split="_")[[1]][3]})
## vector for colors
color.group <- c()#adjustcolor(rep("black", nn), alpha.f=1)
## set color for W
#these.W <- grep("_W", network$translate$label, fixed=TRUE)
these.W <- which(group == "W")
color.group[network$translate$node[these.W]] <- "#F0E442"
## set color for S
these.S <- which(group == "S")
color.group[network$translate$node[these.S]] <- "#4477AA"
## set color for A
these.A <- which(group == "A")
color.group[network$translate$node[these.A]] <- "#228b22"
## set color for E
#these.E <- which(group == "E")
#color.group[network$translate$node[these.E]] <- "#D81B60"
### set color for Evi
#these.Evi <- grep("Evi", network$translate$label, fixed=TRUE)
#color.group[network$translate$node[these.Evi]] <- "grey50"

## save the figure
#svg(filename = "split_network_COI.svg", width = 5, height = 5) ## svg counts in inches
#png(filename = "split_network_COI.png", width = 20, height = 15, units = "cm", res=300)
#par(mar=c(0, 0, 0, 0))
#plot(network, type="2D", show.tip.label=FALSE, show.node.label=TRUE, node.label=shape.group, 
#     edge.width=0.5, cex=1, tip.color=adjustcolor(color.group, alpha.f=0.75))
#legend("bottomleft", border = FALSE, bty = "n" , cex = 1.2, pch=21,
#       title = "Haplogroup:",
#       legend = c("Angara", "Western", "Southern", "Eastern", "outgroup"),
#       col = c("#228b22", "#F0E442", "#4477AA", "#D81B60", "grey50"),
#       pt.bg = c("#228b22", "#F0E442", "#4477AA", "#D81B60", "grey50"))
#legend("bottomright", border = FALSE, bty = "n" , cex = 1.2,
#       title = "Location: ",
#       legend = c("Angara river", "Lake Baikal"), pch=c(21, 22))
#dev.off()


## Option 2
## read the data = nexus file recorded with SplitsTree4 (!)
Nnet <- read.nexus.networx("../data/COI.nex")

Nnet$edge.length <- Nnet$edge.length*467

pn <- 
  ggsplitnet(Nnet, col="black") + 
  geom_treescale(x=-.04, y=.035, offset=.001) + 
  coord_fixed()
#  geom_tiplab2() + 
#  geom_point(aes(shape="21", color=isTip), size=2)

tips <- pn$data[pn$data$isTip, ]
tips$group <- sapply(tips$label, function(x) strsplit(x, split = "_")[[1]][3])
tips$group[which(is.na(tips$group))] <- "out"


library(dplyr)
tips %>% count(x, y, group) -> tips.occur

tips$place <- sapply(tips$label, function(x) strsplit(x, split = "_")[[1]][1])
tips$place <- ifelse(tips$place == "E" | tips$place == "Evi", "Lake Baikal", "Angara River")

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

library(phangorn)
library(tanggle)
library(dplyr) ## for some data rearrangement

## read the data = nexus file recorded with SplitsTree4 (!)
network <- read.nexus.networx("../data/18S.nex")

### this is for base R plotting with Unicode symbols
### works but there are some troubles
### saved here just in case / compatibility
#plot(network, type="2D", show.tip.label=TRUE, show.node.label=TRUE,
#     edge.width=0.5, cex=0.5, scale.bar = 1)#, tip.color=adjustcolor(lvc, alpha.f=0.75))
### assign groups by shape
### Unicode shapes
### https://www.fileformat.info/info/unicode/block/geometric_shapes/list.htm
#shape.group <- c()#rep("", nn)
#shape.group[network$translate$node] <- c("\U2B24") ##circle
#these.refs <- c(grep("E_", network$translate$label, fixed=TRUE), 
#                grep("Evi_", network$translate$label, fixed=TRUE))
#shape.group[these.refs] <- c("\U25A0") ##squares for refs

### assign groups by color
### here's an example of our pattern
#network$translate$label
#group <- sapply(network$translate$label, function(x) {strsplit(x, split="_")[[1]][3]})
### vector for colors
#color.group <- c()#adjustcolor(rep("black", nn), alpha.f=1)
### set color for W
#these.W <- grep("_W", network$translate$label, fixed=TRUE)
#these.W <- which(group == "W")
#color.group[network$translate$node[these.W]] <- "#F0E442"
### set color for S
#these.S <- which(group == "S")
#color.group[network$translate$node[these.S]] <- "#4477AA"
### set color for A
#these.A <- which(group == "A")
#color.group[network$translate$node[these.A]] <- "#228b22"
### set color for E
#these.E <- which(group == "E")
#color.group[network$translate$node[these.E]] <- "#D81B60"
### set color for Evi
#these.Evi <- grep("Evi", network$translate$label, fixed=TRUE)
#color.group[network$translate$node[these.Evi]] <- "grey50"

### save the figure
#svg(filename = "split_network_18S.svg", width = 5, height = 5) ## svg counts in inches
##png(filename = "split_network_18S.png", width = 20, height = 15, units = "cm", res=300)
#par(mar=c(0, 0, 0, 0))
#plot(network, type="2D", show.tip.label=FALSE, show.node.label=TRUE, node.label=shape.group, 
#     edge.width=0.5, cex=1, tip.color=adjustcolor(color.group, alpha.f=0.75))
#legend("bottomright", border = FALSE, bty = "n" , cex = 1.2, pch=21,
#       title = "Haplogroup:",
#       legend = c("Angara", "Western", "Southern", "Eastern", "outgroup"),
#       col = c("#228b22", "#F0E442", "#4477AA", "#D81B60", "grey50"),
#       pt.bg = c("#228b22", "#F0E442", "#4477AA", "#D81B60", "grey50"))
#legend("topright", border = FALSE, bty = "n" , cex = 1.2,
#       title = "Location: ",
#       legend = c("Angara river", "Lake Baikal"), pch=c(21, 22))
#dev.off()

#network$edge.length <- Nnet$edge.length*467 ## for further processing of scale

pn <- 
  ggsplitnet(network, col = "black") + 
  geom_treescale(x=.001, y=.0001, offset=.0001, width = 1e-3)+ 
  coord_fixed() #+
#  geom_tiplab2() + 
#  geom_point(aes(shape="21", color=isTip), size=2)
pn


tips <- pn$data[pn$data$isTip, ]
tips$group <- sapply(tips$label, function(x) strsplit(x, split = "_")[[1]][3])
tips$group[which(is.na(tips$group))] <- "Outgroup"

tips$place <- sapply(tips$label, function(x) strsplit(x, split = "_")[[1]][1])
tips$place <- ifelse(tips$place == "E" | tips$place == "Evi", "Lake Baikal", "Angara River")

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
png("Figure_2.png", width = 20, height = 10, res=600, units="cm")
ggarrange(pnCOI+theme(legend.position = "none"), pn18S, 
          labels = c("A", "B"))
dev.off()
