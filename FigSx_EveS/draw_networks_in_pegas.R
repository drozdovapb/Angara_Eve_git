library(ape)

## Sources and libraries
## https://johnbhorne.wordpress.com/2016/09/15/still-making-haplotype-networks-the-old-way-how-to-do-it-in-r/
## also
## https://jimmyodonnell.wordpress.com/2014/06/16/legend-for-haplotype-networks-in-r/

#install.packages("pegas")
library(pegas)

## read data
EveCOI <- read.dna("Gurkov_S2_COI_Eve_BK_2011_2012_2013_2013_2016_trimmed.fa", format = "fasta")


## convert data to the necessary format
EveCOIHaps <- haplotype(EveCOI)
EveCOINet <- haploNet(EveCOIHaps)
plot(EveCOINet, size = attr(EveCOINet, "freq"), fast = FALSE)

ind.hap<-with(
  stack(setNames(attr(EveCOIHaps, "index"), rownames(EveCOIHaps))),
  table(hap=ind, individuals=rownames(EveCOI)[values]))

mydata <- as.data.frame(ind.hap)
good <- mydata[mydata$Freq == 1,]

good$individuals <- gsub("EveBK", "EveBK_RNAseq", good$individuals)
good$individuals <- gsub("E_verrucosus_Bolshie_Koty", "EveBK_Sanger", good$individuals)
#location <- strsplit(as.character(good$individuals), "_")
years <- substr(good$individuals, 7, 17)

new.hap <- table(good$hap, years)

png("Eve_haplotype_network.png", width = 800, height = 800)
plot(EveCOINet, size = attr(EveCOINet, "freq"), fast = FALSE, pie = new.hap, legend = c(-30, 10),
     scale.ratio = 2, label = F)
text(x=-20, y=15, "Eulimnogammarus verrucosus", cex = 1.5)
dev.off()


## Ecy block

EcyCOI <- read.dna("Gurkov_S2_COI_Ecy_BK_2012_2013_2016_trimmed.fa", format = "fasta")


## convert data to the necessary format
EcyCOIHaps <- haplotype(EcyCOI)
EcyCOINet <- haploNet(EcyCOIHaps)

ind.hap<-with(
  stack(setNames(attr(EcyCOIHaps, "index"), rownames(EcyCOIHaps))),
  table(hap=ind, individuals=rownames(EcyCOI)[values]))

mydata <- as.data.frame(ind.hap)
good <- mydata[mydata$Freq == 1,]

good$individuals <- gsub("EcyBK", "EcyBK_RNAseq", good$individuals)
good$individuals <- gsub("E_cyaneus_Bolshie_Koty", "EcyBK_Sanger", good$individuals)
#location <- strsplit(as.character(good$individuals), "_")
years <- substr(good$individuals, 7, 17)

new.hap <- table(good$hap, years)

png("Ecy_haplotype_network.png", width = 800, height = 800)
plot(EcyCOINet, size = attr(EcyCOINet, "freq"), fast = FALSE, pie = new.hap, legend = c(13, 0),
     scale.ratio = 2, label = F)
text(x=0, y=20, "Eulimnogammarus cyaneus", cex = 1.5)
dev.off()


## Gla block

GlaCOI <- read.dna("Gurkov_S2_COI_Gla_Lake14_2012_2013_2016_trimmed2.fa", format = "fasta")


## convert data to the necessary format
GlaCOIHaps <- haplotype(GlaCOI)
GlaCOINet <- haploNet(GlaCOIHaps)

ind.hap<-with(
  stack(setNames(attr(GlaCOIHaps, "index"), rownames(GlaCOIHaps))),
  table(hap=ind, individuals=rownames(GlaCOI)[values]))

mydata <- as.data.frame(ind.hap)
good <- mydata[mydata$Freq == 1,]

#good$individuals <- gsub("GlaBK", "GlaBK_RNAseq", good$individuals)
#good$individuals <- gsub("E_cyaneus_Bolshie_Koty", "GlaBK_Sanger", good$individuals)
#location <- strsplit(as.character(good$individuals), "_")
years <- substr(good$individuals, 13, 23)

new.hap <- table(good$hap, years)

png("Gla_haplotype_network.png", width = 800, height = 800)
par(mar=c(5,5,5,25))
plot(GlaCOINet, size = attr(GlaCOINet, "freq"), fast = FALSE, pie = new.hap, legend = c(11, 0),
     scale.ratio = 1, label = F)
text(x=-8, y=14, "Gammarus lacustris", cex = 1.5)
dev.off()
