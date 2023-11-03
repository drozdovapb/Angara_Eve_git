library(ggtree)

tree <- read.tree("both/alns.treefile")


ASAP_18S <- read.csv("18S/ASAP/18S_Eve.aln.Partition_1.csv", header = F, row.names = 1)
row.names(ASAP_18S) <- sub(" ", "", row.names(ASAP_18S))
names(ASAP_18S) <- c("cluster")
ASAP_18S$cluster <- paste("18S_", ASAP_18S$cluster)
ASAP_18S$cluster <- factor(ASAP_18S$cluster)


ASAP_COI <- read.csv("COI/ASAP/COI_Eve.fa.Partition_1.csv", header = F, row.names = 1)
row.names(ASAP_COI) <- sub(" ", "", row.names(ASAP_COI))
names(ASAP_COI) <- c("cluster")
ASAP_COI$cluster <- paste("COI_", ASAP_COI$cluster)
ASAP_COI$cluster <- factor(ASAP_COI$cluster)

genetic_lineage <- data.frame(row.names = row.names(ASAP_COI),
                              Lineage = (sapply(row.names(ASAP_COI), function(x) {strsplit(x, split="_")[[1]][3]})))
genetic_lineage$Lineage <- paste0("Lineage_", genetic_lineage$Lineage)

t <- ggtree(tree) + 
  geom_tiplab(size=2)

gheatmap(p = t, data=ASAP_18S, width = .1, offset = .05, 
         custom_column_labels = c("ASAP 18S"), colnames_position = "top") + 
  scale_fill_manual(values = c("#228b22", "#4477AA", "orange")) -> t2
#ggsave(filename = "test.png", width = 40, height = 80, units="cm")

t2

gheatmap(p = t2, data=ASAP_COI, width = .1, offset = .01,
         custom_column_labels = c("ASAP COI"), colnames_position = "top") + 
  scale_fill_manual(values = c("#228b22", "#4477AA", "orange",
                               "#228b22", "green3", "#4477AA", "#F0E442", "#D81B60")) +
  theme(legend.position = 'none') -> t3

t3

gheatmap(p = t3, data=genetic_lineage, width = .1, offset = .09,
         custom_column_labels = c("Genetic lineage"), colnames_position = "top") + 
  scale_fill_manual(values = c("#228b22", "#4477AA", "orange",
                               "#228b22", "green3", "#4477AA", "#F0E442", "#D81B60",
                               "#228b22", "#D81B60", "#4477AA", "#F0E442")) -> t4

t4

pdf(file = "FigS1_species_delimitation.pdf", width = 10, height = 15)
#svg(file = "delimitation.svg", width = 10, height = 20)
t4
dev.off()
