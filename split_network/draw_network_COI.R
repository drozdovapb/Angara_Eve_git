library("phangorn")

## read the data = nexus file recorded with SplitsTree4 (!)
network <- read.nexus.networx("COI.nex")

plot(network, type="2D", show.tip.label=TRUE, show.node.label=TRUE,
     edge.width=0.5, cex=0.5, scale.bar = 1)#, tip.color=adjustcolor(lvc, alpha.f=0.75))

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
these.E <- which(group == "E")
color.group[network$translate$node[these.E]] <- "#D81B60"
## set color for Evi
these.Evi <- grep("Evi", network$translate$label, fixed=TRUE)
color.group[network$translate$node[these.Evi]] <- "grey50"

## save the figure
svg(filename = "split_network_COI.svg", width = 5, height = 5) ## svg counts in inches
#png(filename = "split_network.png", width = 480, height = 480)
par(mar=c(0, 0, 0, 0))
plot(network, type="2D", show.tip.label=FALSE, show.node.label=TRUE, node.label=shape.group, 
     edge.width=0.5, cex=1, tip.color=adjustcolor(color.group, alpha.f=0.75))
legend("bottomleft", border = FALSE, bty = "n" , cex = 1.2,
       legend = c("Angara", "Western", "Southern", "Eastern", "outgroup"),
       fill = c("#228b22", "#F0E442", "#4477AA", "#D81B60", "grey50"))
dev.off()
