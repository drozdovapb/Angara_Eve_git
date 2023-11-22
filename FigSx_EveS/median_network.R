#install.packages("pegas")
library(pegas)

## read data
EveCOI <- read.dna("COI EveS_aln_2.fa", format = "fasta")

region <- sapply(strsplit(rownames(EveCOI), split="_"), "[", 2)
region[region=="AR"] <- "Irkutsk"
region[region=="AL"] <- "Irkutsk"
region[region=="GR"] <- "Irkutsk"
region[region=="GL"] <- "Irkutsk"
region[region=="BR"] <- "Irkutsk"
region[region=="BL"] <- "Irkutsk"
region[region=="To"] <- "Irkutsk"
region[region=="Ya"] <- "Irkutsk"

EveCOINet <- haploNet(EveCOIHaps)
nt.labs <- attr(EveCOINet, "labels")

## convert data to the necessary format
EveCOIHaps <- haplotype(EveCOI)
R <- haploFreq(EveCOI, fac = region, haplo = EveCOIHaps)
R <- R[nt.labs, ]

#xy <- replot()

#library(RColorBrewer)
#blues <- brewer.pal(n = 6, name = "Blues")
#colors <-c(blues[5], blues[5], blues[1], blues[5], blues[5], blues[5],
#           blues[2], blues[4], blues[6], blues[3], blues[4], blues[5], blues[5])
#colors <- c(blues[1], blues[5], blues[2], blues[4],
#            blues[6], blues[3], blues[4])

colors <- c("darkseagreen1", "darkseagreen4", "cyan1", "cyan3", "cyan1")

png("Eve_haplotype_network.png", width = 800, height = 800)
plot(EveCOINet, size = attr(EveCOINet, "freq"), pie = R, legend = c(30, 20),
     scale.ratio = 3, label=F, bg = colors) #, xy=xy)
dev.off()

