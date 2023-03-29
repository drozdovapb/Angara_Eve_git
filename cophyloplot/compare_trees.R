library(ggtree)
tree18S <- read.tree("2a_iqtree_wo_EW/1a_trimmed_algns_wo_EW.treefile")[[1]]
treeCOI <- read.tree("2a_iqtree_wo_EW/1a_trimmed_algns_wo_EW.treefile")[[2]]

#as.polytomy(tree, feature='node.label', fun=function(x) as.numeric(x) < 70)
#tree18S2 <- as.polytomy(tree18S, feature='edge.length', fun=function(x) x < 0.00001)
tree18S2 <- as.polytomy(tree18S, feature='node.label', fun=function(x) as.numeric(x) < 30)
treeCOI2 <- as.polytomy(treeCOI, feature='node.label', fun=function(x) as.numeric(x) < 30)

sort(treeCOI$tip.label) == sort(tree18S$tip.label)
#treeCOI$tip.label <- 
treeCOI$tip.label <- sapply(treeCOI$tip.label, function(X) substr(X, 1, nchar(X)-2))
tree18S$tip.label <- sapply(tree18S$tip.label, function(X) substr(X, 1, nchar(X)-2))

library(ggplot2)
COItree <- 
  ggtree(treeCOI) + geom_tiplab(size=2.5) +
  geom_nodelab(color = "blue", nudge_x = -.05) + 
  xlim(0,.5) + ggtitle("COI")
r18Stree <- 
  ggtree(tree18S) + geom_tiplab(size=2.5) +
  geom_nodelab(color = "red", nudge_x = -.000) +
  xlim(0,.1) + ggtitle("18S")

library(ggpubr)
ggarrange(COItree, r18Stree)

## checking if polytomy did work
ggtree(tree18S, branch.length = 'none')
ggtree(tree18S2, branch.length = 'none')
ggtree(treeCOI, branch.length = 'none')
ggtree(treeCOI2, branch.length = 'none')

#library(ape)
#association <- cbind(treeCOI$tip.label, tree18S$tip.label)
#png("cophyloplot.png", width = 1000, height = 1200)
#cophyloplot(treeCOI, tree18S, assoc=association, length.line=30, space=28, gap=3, col = "red")
#dev.off()


library(phytools)
trees.cophylo<-cophylo(treeCOI, tree18S, rotate.multi = TRUE, print=TRUE)

COIcol <- 
ifelse(test = grepl("_W",treeCOI$tip.label), yes="yellow", 
       no = ifelse(grepl("_S",treeCOI$tip.label), yes="blue", 
                   no = ifelse(test=grepl("_E",treeCOI$tip.label), yes = "red",
                               no = ifelse(grepl("_A",treeCOI$tip.label), yes = "green", no ="black"))))

r18Scol <- 
  ifelse(test = grepl("_W",tree18S$tip.label), yes="yellow", 
         no = ifelse(grepl("_S",tree18S$tip.label), yes="blue", 
                     no = ifelse(test=grepl("_E",tree18S$tip.label), yes = "red",
                                 no = ifelse(grepl("_A",tree18S$tip.label), yes = "green", no ="black"))))

edgecol <- list(left=COIcol, right=r18Scol)

png("cophylo.png", width = 1200, height = 1800)
plot(trees.cophylo, link.type="curved",link.lwd=4, tip.col=edgecol, branch.lengths=F,
     link.lty="solid",link.col=make.transparent("red", 0.25), size = 1)
#abline(h=.475)
dev.off()

#trees.cophylo<-cophylo(treeCOI2, tree18S2, rotate.multi = TRUE, print=TRUE)
#png("cophylo.png", width = 1200, height = 1600)
#plot(trees.cophylo, link.type="curved",link.lwd=4, tip.col=edgecol, branch.lengths=F,
#     link.lty="solid",link.col=make.transparent("red", 0.25), size = 1)
#abline(h=.3)
#dev.off()
