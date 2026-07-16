################################################
# R data analysis for metabarcoding
# by Alexander Keller (LMU München)
# keller@bio.lmu.de
#
# Created: Di  1 Jul 2025 11:44:38 CEST
# Project: COI_Merit-Niels
# Marker: COI-5P
# For: Merit-Niels (LMU)


###############################################

# Adapted: 05.08.25

################################################
# Clear workspace
rm(list = ls())

# Loading in necessary libraries
library(phyloseq) # version ‘1.50.0’
library(ggplot2)
library(bipartite)
library(tidyr)
library(reshape2)
library(speedyseq)
library(ggsci)
library(ape)
# Setting working directory (check path)
setwd('Data/COI_140725/')

# Custom functions inclusion
marker="COI-5P"
source('./metabarcoding_tools_0-1a.R')
# Loading in data
## Taxonomy
data.tax <- tax_table(as.matrix(read.table("taxonomy.vsearch", header=T,row.names=1,fill=T,sep=",")))

## Community table
data.otu <- otu_table(read.table("asv_table.merge.txt"), taxa_are_rows=T)

## if phylogeny included
# data.tre <- read.tree("asvs.tre")

## Sample metadata (second line optional if sample names include "-"):
#data.map <- 	sample_data(read.table("Metadata_birds_060825.csv", header=T, row.names=13,  sep=",", fill=T))
data.map <- 	sample_data(read.table("Metadata_birds_180825.csv", header=T, row.names=15,  sep=",", fill=T))
#data.map <- 	sample_data(read.table("Metadata_FRASS_150725.csv", header=T, row.names=13,  sep=",", fill=T))
#data.map$collection_date <- as.Date(dates, format = "%d.%m.%Y")


sample_names(data.map) <- gsub("_",".",sample_names(data.map))

sample_names(data.otu) <- gsub(".COI_S1","",sample_names(data.otu))

## check metadata vs. samples in sequencing data consistency
sample_names(data.map)[!(sample_names(data.map) %in% sample_names(data.otu))]
sample_names(data.otu)[!(sample_names(data.otu) %in% sample_names(data.map))]

intersect(sample_names(data.map),sample_names(data.otu))

## merge the three tables to a single phyloseq object
(data.comp <- merge_phyloseq(data.otu,data.tax,data.map))
## if phylogeny included: 
# (data.comp <- merge_phyloseq(data.otu,data.tax,data.map,data.tre))

# create fake metadata if needed
#data.comp <- fill_pseudo_metadata(data.comp)

# preprocessing data pt.1 :
## given hierarchical classification options at the end, we have to propagate the taxonomy over taxonomic levels to not throw out stuff only classified to higher tax levels
data.comp <- propagate_incomplete_taxonomy(data.comp)

## filtering irrelevant taxa, zb. unresolved, algae, fungi etc
data.comp.filter <- remove_unresolved_taxa(data.comp)

(data.comp.filter = subset_taxa(data.comp.filter, kingdom!=""))

## Make taxa labels nice for plots
data.comp.filter <- replace_tax_prefixes(data.comp.filter)
#taxa_names(data.comp.filter) <- interaction(taxa_names(data.comp.filter),tax_table(data.comp.filter)[,length(colnames(tax_table(data.comp.filter)))])

### Check the names
tail(tax_table(data.comp.filter))

## Multiple ASVs might represent the same species, here they are collated
#(data.species <- tax_glom(data.comp.filter,taxrank="species"))
#taxa_names(data.species) <- tax_table(data.species)[,"species"]

taxa_df <- as.data.frame(tax_table(data.comp.filter)) ## make df to look at it


# alternatively try if used postclustering: 
library("stringr")
library("dplyr")
data.species <- tax_glom_species_filtered(data.comp.filter,rank="species") ## aggregates data at specific level, here species
taxa_names(data.species) <- tax_table(data.species)[,"species"]

## (optional) Rename samples by type different metadata factors
#(data.species <- label_sample_by_host(data.species,"host","project"))

# Transform to relative data
data.species.rel = transform_sample_counts(data.species, function(x) x/sum(x))


## (optional) consider looking at controls and
control_samples = c("negative","positive")

#pdf("plots/controls_absolute.pdf", width=5, height=5)

#controls <- subset_samples(data.species, Type %in% control_samples)
#controls <- prune_taxa(taxa_sums(controls)>100, controls)
#controls.melt <- psmelt(controls)
#ggplot(controls.melt, aes(x=species, y=Abundance, col=Type))+geom_boxplot()+  
#  theme(axis.text.x=element_text(angle = -90, hjust = 0))
#dev.off()

#pdf("plots/controls_relative.pdf", width=15, height=5)

#controls.rel <- subset_samples(data.species.rel, Type %in% control_samples)
#controls.rel <- prune_taxa(taxa_sums(controls.rel)>0.1, controls.rel)
#controls.melt.rel <- psmelt(controls.rel)
#controls.melt.rel  <- controls.melt.rel %>% filter(Abundance > 0.001)

#ggplot(controls.melt.rel, aes(x=species, y=Abundance, col=Type))+geom_boxplot()+scale_y_log10()+ 
#  geom_hline(yintercept = c(0.01, 0.1, 0.001, 0.0001), 
#             linetype = "dashed", color = "red") +
#  annotate("text", x = 10, y = 0.01, 
#           label = "0.01", hjust = 1, vjust = -0.5, color = "red") +
#  annotate("text", x = 10, y = 0.1, 
#           label = "0.1", hjust = 1, vjust = -0.5, color = "red") +
#  annotate("text", x = 10, y = 0.001, 
#           label = "0.001", hjust = 1, vjust = -0.5, color = "red") +
#  annotate("text", x = 10, y = 0.0001, 
#           label = "0.0001", hjust = 1, vjust = -0.5, color = "red")+  
#  theme(axis.text.x=element_text(angle = -90, hjust = 0))
#dev.off()

data.species <- subset_samples(data.species, !(Type %in% control_samples))

### look at my data
taxa_df <- as.data.frame(tax_table(data.species))


# Filter irrelevant taxa 
filter_taxa.kingdom = c("Fungi","Plantae","", "Protista", "Bacteria")
filter_taxa.phylum = c("Rotifera", "Nematoda", "Platyhelminthes", "Bryozoa", "Cnidaria", "Echinodermata",
                       "Gastrotricha", "Porifera", "Tardigrada", "Nemertea", "Chordata")
filter_taxa.class = c("Actinopterygii", "Amphibia", "Bivalvia", "Ascidiacea", "Branchiopoda", "Aves", "Reptilia",
                      "Thecostraca", "Polyplacophora", "Polychaeta", "Animalia spc", "Arthropoda spc")
filter_taxa.order = c("Arachnida spc", "Insecta spc")
filter_taxa.species = c("Homo sapiens", "Meleagris gallopavo", "Cyornis spc 1", "Platorchestia platensis")

data.species.filter <- subset_taxa(data.species, !kingdom %in% filter_taxa.kingdom)
data.species.filter <- subset_taxa(data.species.filter, !phylum %in% filter_taxa.phylum)
data.species.filter <- subset_taxa(data.species.filter, !class %in% filter_taxa.class)
data.species.filter <- subset_taxa(data.species.filter, !order %in% filter_taxa.order)
data.species.filter <- subset_taxa(data.species.filter, !species %in% filter_taxa.species)


### look at my data
after_taxa_df <- as.data.frame(tax_table(data.species.filter))

## (optional) Label samples with low throughput with LT
(data.species.filter <- label_low_throughput(data.species.filter , 500))
sample_names(data.species.filter)

# Transform to relative abundance data after removal of irrelevant taxa
data.species.rel = transform_sample_counts(data.species.filter, function(x) x/sum(x)) # this is done on species level


# low abundance filtering
otu_table(data.species.rel)[otu_table(data.species.rel)<0.001 ]<-0
otu_table(data.species)[otu_table(data.species.rel)<0.001 ]<-0
data.species.filter		= prune_taxa(taxa_sums(data.species)>0, data.species)
(data.species.rel.filter = prune_taxa(rowSums(otu_table(data.species.rel))>0, data.species.rel))


### look at my data
lowab_taxa_df <- as.data.frame(tax_table(data.species.rel.filter))


########################################################################################### look at families ----


#orders_of_interest <- c("Lepidoptera", "Coleoptera", "Hymenoptera")

#subset <- lowab_taxa_df %>%
#  filter(order %in% orders_of_interest)

#families <- unique(subset$family)
#families

#frass_families <- c(
#  # Lepidoptera
#  "Noctuidae", "Erebidae", "Geometridae", "Tineidae", "Lasiocampidae",
#  "Chrysomelidae",  # beetle larvae
#  "Ypsolophidae", "Tortricidae", "Lycaenidae", "Pyralidae",
#  "Drepanidae", "Gelechiidae", "Argyresthiidae", "Lypusidae",
#  "Notodontidae", "Pterophoridae", "Nymphalidae", "Nolidae",
#  "Glyphipterigidae", "Adelidae", "Hesperiidae", "Incurvariidae",
#  "Coleophoridae", "Psychidae", "Eriocraniidae", "Hepialidae",
#  "Limacodidae", "Crambidae", "Xyloryctidae", "Cosmopterigidae",
  
  # Hymenoptera: Symphyta (sawflies)
#  "Tenthredinidae", "Argidae", "Pamphiliidae", "Cimbicidae"
#)


#shared_families <- c(
#  "Noctuidae","Erebidae","Geometridae","Tineidae","Lasiocampidae",
#  "Chrysomelidae","Ypsolophidae","Tortricidae","Lycaenidae","Pyralidae",
#  "Drepanidae","Gelechiidae","Argyresthiidae","Lypusidae","Notodontidae",
#  "Nolidae","Glyphipterigidae","Adelidae","Hesperiidae","Coleophoridae",
#  "Psychidae","Eriocraniidae","Crambidae","Argidae","Pamphiliidae",
#  "Tenthredinidae"
#)


################################################################################## add diversity measures species 

div_df <- estimate_richness(data.species.rel.filter, measures = "Shannon") # calc Shannon
div_df$EffectiveRichness <- exp(div_df$Shannon) # calc effective species richness

### add to object
sample_data(data.species.rel.filter)$ShannonIndex <- div_df$Shannon
sample_data(data.species.rel.filter)$EffectiveRichness <- div_df$EffectiveRichness

sample_metadata <- as(sample_data(data.species.rel.filter), "data.frame")
sample_metadata$ShannonIndex <- div_df$Shannon
sample_metadata$EffectiveRichness <- div_df$EffectiveRichness

sample_metadata1 <- sample_metadata[, c("ShannonIndex", "EffectiveRichness", "WithinNestRep", "TubeLetter")]

#write.csv(sample_metadata, 'D:/Pokriefke/Documents/Data_analysis/Chapter2/Data/Extracted/birds_110825.csv',row.names = FALSE)


############################################################################################ add RRA order level

data.order <- tax_glom(data.comp.filter, taxrank = "order") # now each taxon is an order not species
# colnames(tax_table(data.comp.filter)) check spelling
taxa_names(data.order) <- tax_table(data.order)[, "order"]

#data.order <- transform_sample_counts(data.order, function(x) x / sum(x)) # redo abundances as glom may change that
data.order <- subset_samples(data.order, !(Type %in% control_samples)) #remove control samples

### filter again
data.order.filter <- subset_taxa(data.order, !kingdom %in% filter_taxa.kingdom)
data.order.filter <- subset_taxa(data.order.filter, !phylum %in% filter_taxa.phylum)
data.order.filter <- subset_taxa(data.order.filter, !class %in% filter_taxa.class)
data.order.filter <- subset_taxa(data.order.filter, !order %in% filter_taxa.order)
data.order.filter <- subset_taxa(data.order.filter, !species %in% filter_taxa.species)


# take out low throughput samples again
data.order.filter <- label_low_throughput(data.order.filter, threshold = 500)

data.order.rel <- transform_sample_counts(data.order.filter, function(x) x / sum(x)) # wenn nochmal nach low abundance, 
#dann würde wieder auf 100 scalieren, muss aber nicht, es sei denn gut begründbar dass low abundance nicht zählt (eg nur prädatoren)

# low abundance filtering
otu_table(data.order.rel)[otu_table(data.order.rel) < 0.001] <- 0
data.order.rel.filter <- prune_taxa(rowSums(otu_table(data.order.rel)) > 0, data.order.rel)

order_rra <- as.data.frame(otu_table(data.order.rel.filter)) # get sample by order matrix


if (taxa_are_rows(data.order.rel.filter)) {
  order_rra <- t(order_rra)
  order_rra <- as.data.frame(order_rra)
}

order_names <- tax_table(data.order.rel.filter)[, "order"]
colnames(order_rra) <- make.names(order_names)

# sample order matching
order_rra <- order_rra[match(sample_names(data.order.rel.filter), rownames(order_rra)), ]


for (order in colnames(order_rra)) {
  sample_data(data.order.rel.filter)[[paste0("RRA_", order)]] <- order_rra[, order]
}

sample_metadata2 <- as(sample_data(data.order.rel.filter), "data.frame")
sample_metadata2 <- sample_metadata2[ , c("RRA_Lepidoptera", "RRA_Diptera", "RRA_Araneae", "RRA_Hymenoptera",
                                          "RRA_Coleoptera", "WithinNestRep", "TubeLetter")]


############################################################################################ add RRA family level

data.family <- tax_glom(data.comp.filter, taxrank = "family") # now each taxon is an family not species
# colnames(tax_table(data.comp.filter)) check spelling
taxa_names(data.family) <- tax_table(data.family)[, "family"]

#data.family <- transform_sample_counts(data.family, function(x) x / sum(x)) # redo abundances as glom may change that
data.family <- subset_samples(data.family, !(Type %in% control_samples)) #remove control samples

### filter again
data.family.filter <- subset_taxa(data.family, !kingdom %in% filter_taxa.kingdom)
data.family.filter <- subset_taxa(data.family.filter, !phylum %in% filter_taxa.phylum)
data.family.filter <- subset_taxa(data.family.filter, !class %in% filter_taxa.class)
data.family.filter <- subset_taxa(data.family.filter, !order %in% filter_taxa.order)
data.family.filter <- subset_taxa(data.family.filter, !species %in% filter_taxa.species)


# take out low throughput samples again
data.family.filter <- label_low_throughput(data.family.filter, threshold = 500)

data.family.rel <- transform_sample_counts(data.family.filter, function(x) x / sum(x)) # wenn nochmal nach low abundance, 
#dann würde wieder auf 100 scalieren, muss aber nicht, es sei denn gut begründbar dass low abundance nicht zählt (eg nur prädatoren)

# low abundance filtering
otu_table(data.family.rel)[otu_table(data.family.rel) < 0.001] <- 0
data.family.rel.filter <- prune_taxa(rowSums(otu_table(data.family.rel)) > 0, data.family.rel)

family_rra <- as.data.frame(otu_table(data.family.rel.filter)) # get sample by family matrix


if (taxa_are_rows(data.family.rel.filter)) {
  family_rra <- t(family_rra)
  family_rra <- as.data.frame(family_rra)
}

family_names <- tax_table(data.family.rel.filter)[, "family"]
colnames(family_rra) <- make.names(family_names)

# sample family matching
family_rra <- family_rra[match(sample_names(data.family.rel.filter), rownames(family_rra)), ]


for (family in colnames(family_rra)) {
  sample_data(data.family.rel.filter)[[paste0("RRA_", family)]] <- family_rra[, family]
}

sample_metadata3 <- as(sample_data(data.family.rel.filter), "data.frame")


write.csv(sample_metadata3, 'D:/Pokriefke/Documents/Data_analysis/Chapter2/Git/DietVar/Data/Extracted/birds3_020925.csv',row.names = FALSE)




sample_metadata4 <- sample_metadata3[ , c("NestBox","BroodID","RingNumber","Plot","year","Species","Type",
                                          "Age","Sex","nSamples","dateSample","AdateSample","timeSample",
                                          "tube_ID","ClutchSize","ClutchNumber","NumberFledged","SubplotYear",
                                          "TreatmentB","TreatmentG","Doublebox","ExpScore","TimeAfterSunrise",
                                          "TimeAfterSunrise_c","Sequence","TubeLetter","TubeLetterValue",
                                          "SpecType","Sum.OakNr.","MeanCircumference","MaxCircumference",
                                          "WithinNestRep",
                                          "RRA_Noctuidae","RRA_Erebidae","RRA_Geometridae","RRA_Tineidae",
                                          "RRA_Lasiocampidae","RRA_Chrysomelidae","RRA_Ypsolophidae",
                                          "RRA_Tortricidae","RRA_Lycaenidae","RRA_Pyralidae", "RRA_Drepanidae",
                                          "RRA_Gelechiidae","RRA_Argyresthiidae","RRA_Lypusidae",
                                          "RRA_Notodontidae","RRA_Nolidae","RRA_Glyphipterigidae","RRA_Adelidae",
                                          "RRA_Hesperiidae","RRA_Coleophoridae","RRA_Psychidae",
                                          "RRA_Eriocraniidae","RRA_Crambidae","RRA_Argidae","RRA_Pamphiliidae",
                                          "RRA_Tenthredinidae" )]


final_sample_metadata1 <- merge(sample_metadata1, sample_metadata2, by = c("WithinNestRep", "TubeLetter"))
final_sample_metadata2 <- merge(final_sample_metadata1, sample_metadata4, by = c("WithinNestRep", "TubeLetter"))


write.csv(final_sample_metadata2, 'D:/Pokriefke/Documents/Data_analysis/Chapter2/Git/DietVar/Data/Extracted/birds2_270825.csv',row.names = FALSE)


############################################################################### add familiy level Shannon and EFR

# Shannon index
div_family_df <- estimate_richness(data.family.rel.filter, measures = "Shannon")

# Effective richness
div_family_df$EffectiveRichness <- exp(div_family_df$Shannon)

# Add diversity metrics to phyloseq object
sample_data(data.family.rel.filter)$ShannonIndex_Family <- div_family_df$Shannon
sample_data(data.family.rel.filter)$EffectiveRichness_Family <- div_family_df$EffectiveRichness

# Extract metadata
sample_metadata_family <- as(sample_data(data.family.rel.filter), "data.frame")

# Keep only relevant columns
sample_metadata_family <- sample_metadata_family[, c("ShannonIndex_Family",
                                                     "EffectiveRichness_Family",
                                                     "NestBox","BroodID","RingNumber","Plot","year","Species","Type",
                                                     "Age","Sex","nSamples","dateSample","AdateSample","timeSample",
                                                     "tube_ID","ClutchSize","ClutchNumber","NumberFledged","SubplotYear",
                                                     "TreatmentB","TreatmentG","Doublebox","ExpScore","TimeAfterSunrise",
                                                     "TimeAfterSunrise_c","Sequence","TubeLetter","TubeLetterValue",
                                                     "SpecType","Sum.OakNr.","MeanCircumference","MaxCircumference",
                                                     "WithinNestRep")]



write.csv(sample_metadata_family, 'D:/Pokriefke/Documents/Data_analysis/Chapter2/Git/DietVar/Data/Extracted/EFR_010925.csv',row.names = FALSE)


#################################################################################################################


# define parameters for plot definitions
ntaxa <- length(taxa_names(data.species.rel.filter))
data.species.rel.filter <- build_pseudophylo_hclust(data.species.rel.filter)


# # check GBIF location records
# species_records_long <- get_species_occurences_long(data.species.rel.filter, notInGBIF=T)

# species_records_long$regCountry <- (interaction(species_records_long$region,species_records_long$sub.region))
# species_records_long <- species_records_long[order(species_records_long$region),]
# species_records_long$regCountry <- factor(species_records_long$regCountry, levels = unique(sort(as.character(species_records_long$regCountry))))

# species_records_long <- cbind(species_records_long,tax_table(data.species.rel.filter)[species_records_long$Species,])

# species_records_long$SpecAbund <- interaction(species_records_long$Species,sprintf("%.3f", round(species_records_long$Abundance, digits=4)), sep=" | ")

# pdf("plots/species_occurence.pdf", width=15, height=ntaxa/5)

# ggplot(species_records_long, aes(fill=regCountry, y=value, x=SpecAbund, alpha=log(Abundance*1000,10))) + 
#   facet_grid(order+family~region, space = "free", scales = "free",switch = "y")+
#   geom_bar(position="stack", stat="identity")+
#   theme(strip.text.y.left = element_text(angle = 0))+
#   theme(axis.text.x = element_text(angle = 60, hjust = 1),axis.title.x = element_text(family = "sans", size = 15)) + 
#   xlab("Species | cumulative relative Abundance")+
#   ylab("GBIF record abundance (log)")+
#   theme(legend.position="bottom")+
#   coord_flip()+ 
#   scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), 
#                      breaks = scales::trans_breaks("log10", function(x) 10^x), 
#                      labels = scales::trans_format("log10", scales::math_format(10^.x))
#   )#+ annotation_logticks(sides = "b", )  
# dev.off()


# First diversity and community metrics and graphs
# Distribution of major taxa, accumulated over all samples
par(mar=c(4,15,1,1), mfrow=c(1,1))
barplot(t(as.data.frame(sort(taxa_sums(data.species.rel.filter), decreasing=T)[1:20])), las=2, horiz=T)

## with ggplot

pdf("D:/Pokriefke/Documents/Data_analysis/Chapter2/Git/DietVar/Output/Plots/birds_cum_rel_abundance_120825.pdf", width=7, height=8)

my_colors <- c(
  "#00441B", "#238B45", "#66C2A4", "#CCECE6", 
  "#08306B", "#2171B5", "#6BAED6", "#C6DBEF"                                     
)

plot_top_taxa_by_metadata(data.species.rel.filter, top_n = 50, metadata_var = "SpecType", group = "order",
                          fill_colors = my_colors)
dev.off()




## Distribution over samples
data.melt <- psmelt(data.species.rel.filter)
data.melt$Sample <- as.factor(data.melt$Sample)

plot_top_taxa_by_metadata(data.species.rel.filter, top_n = 20, metadata_var = "Type")

pdf("plots/sample_rel_abundance.pdf", width=20, height=ntaxa/5)
ggplot(data.melt, aes(OTU, Abundance, fill= family)) +
  facet_grid(order+family~Sex, space = "free", scales = "free",switch = "y")+
  theme_bw()+
  scale_y_continuous(limits = c(0,1), expand = c(0, 0)) +
  geom_bar(stat="identity")+
  theme(legend.position="bottom")+
  theme(strip.text.y.left = element_text(angle = 0))+
  theme(strip.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1),axis.title.x = element_text(family = "sans", size = 15)) + xlab("Species")+
  coord_flip()+ scale_y_continuous(trans=scales::pseudo_log_trans(base = 10))

dev.off()


## Richness (Observed) / Shannon Diversity (replace x/col by group in metadata)
pdf("plots/sample_diversity.pdf", width=25, height=10)
p <- plot_richness(data.species.rel.filter,x= "collection_date" , col= "type", measures=c("Shannon"))+
  theme_bw()+ 
  theme(axis.text.x = element_text(angle = 60, hjust = 1) ) + 
  geom_boxplot()+
  geom_point(size=4, alpha=0.4,position = position_dodge(width = 0.75)) 

p$layers <- p$layers[-1]
p
dev.off()


# effective species richness
div_df <- estimate_richness(data.species.rel.filter, measures = "Shannon")
dfun_df <- dfun(t(otu_table(data.species.rel.filter)))

div_df$SampleID <- rownames(div_df)
meta_df <- as(sample_data(data.species.filter), "data.frame") %>%
  tibble::rownames_to_column("SampleID")

dfun_df <-data.frame(SampleID=rownames(t(t(dfun_df$dprime))), dprime= t(t(dfun_df$dprime)))

merged_df <- left_join(div_df, meta_df, by = "SampleID")
merged_df <- left_join(merged_df, dfun_df, by = "SampleID")

merged_df$EffectiveRichness <- exp(merged_df$Shannon)

p <- ggplot(merged_df, aes(x = Type, y = EffectiveRichness, color = Sex)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(size = 3, width = 0.2, alpha = 0.5) +
  theme_bw() +
  labs(y = "Effective Species Richness (e^H)", x = "Type") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  scale_y_log10()

p
ggsave("plots/effective_species_richness.pdf", plot = p, width = 10, height = 6)

plot(merged_df$Shannon~merged_df$ExpScore)
plot(merged_df$dprime~merged_df$ExpScore)

summary(lm(merged_df$Shannon~merged_df$ExpScore))
summary(lm(merged_df$dprime~merged_df$ExpScore))
  #dprime = specialization from 0 (generalist) -> 1 (specialist)


## Ordination
data.species.filter.nmds <-  ordinate(data.species.rel.filter, method="PCoA", "Unifrac")#,k=3, trymax=50) # change to NMDS
sample_data(data.species.rel.filter)$SpeciesName <- as.factor(sample_data(data.species.rel.filter)$Species)
sample_data(data.species.rel.filter)$SexName <- as.factor(sample_data(data.species.rel.filter)$Sex)

plot_ordination(data.species.rel.filter, data.species.filter.nmds , color="Sex", shape="SpeciesName")+
  geom_point(size=2)+
  theme_bw()#+geom_label(label=sample_names(data.species.rel.filter))


#outliers =c("ND.1093","ND.0192", "ND.0715","ND.0609","ND.0482","ND.0182","ND.0747")

outliers =c("ND.0182","ND.0747")
data.species.rel.filter.outliers <- subset_samples(data.species.rel.filter, (sample_names(data.species.rel.filter) %in% outliers))

par(mar=c(4,15,1,1), mfrow=c(1,1))
barplot(t(as.data.frame(sort(taxa_sums(data.species.rel.filter.outliers), decreasing=T)[1:20])), las=2, horiz=T)




data.species.rel.filter.sub <- subset_samples(data.species.rel.filter, !(sample_names(data.species.rel.filter) %in% outliers))
data.species.filter.nmds <-   ordinate(data.species.rel.filter.sub, method="NMDS", "bray",k=4, trymax=20) # change to NMDS

pdf("plots/sample_ordination.pdf", width=12, height=10)
plot_ordination(data.species.rel.filter.sub, data.species.filter.nmds , color="Type", shape="SpeciesName")+
  geom_point(size=6)+
  theme_bw()#+geom_label(label=sample_names(data.species.rel.filter.sub))
dev.off()


## if phylogeny included:
# pdf("plots/sample_ordination_PCOA_unifrac_uw.pdf", width=12, height=10)

# unifrac.dist <- UniFrac(data.species.rel.filter)
# ordi = ordinate(data.species.rel.filter, "PCoA", "unifrac", weighted=F)
# plot_ordination(data.species.rel.filter, ordi, color="Plot")
# dev.off()

## Networks (replace id by group if you want to have them merged by metadata)
netmat <- t(otu_table(data.species.rel.filter))
sample_data(data.species.rel.filter)$SexSpecies <- interaction(sample_data(data.species.rel.filter)$Sex, sample_data(data.species.rel.filter)$Species)
netmat <- otu_table(merge_samples(data.species.rel.filter,group="SexSpecies"))

### (optional) keep only major links, otherwise it often becomes overwhelming
otu_table(netmat)[otu_table(netmat)<0.10 ]<-0
netmat		= prune_taxa(taxa_sums(netmat)>0, netmat)

### plotting
pdf("plots/sample_network.pdf", width=25, height=10)
plotweb(data.frame(t(otu_table(netmat))))
dev.off()











