# ExStream 2022
# Multivariate statistical analysis of microphytobenthos data
# testing the response of algae to salinity, temperature and flow velocity
# Microscopy & 18S-V9
# 01.10.2023
# Serge Mayombo

# load the libraries
library(tidyverse)
library(vegan)
#library(ggstatsplot)

# read data
# read environmental dataset
# Microscopy
setwd("/Users/imp/Documents/Promotion/Manuscripts/co-authorship/02_Serge_STOTEN/analysis_check")
diat <- read.csv("exstream2022_IMP_microscopy.csv", sep = ",",
                 header = T)
#diat1 <- diat[,-c(60,61)]
#view(diat1)

# make channel column as rownames
diat1 <- diat %>% column_to_rownames(var="Channel") # change row names
eda <- colSums(diat1)
view(eda)
sum(diat1)
ach_min <- (15316/28054)*100
ach_jack<- (2824/28054)*100
nav_greg <- (1221/28054)*100
Plan_lanc <- (596/28054)*100
# read exstream metadata
metadata <- read.csv("metadata_IMP2022.csv")
# edit channel treatments : salt in C07 and background in C08
# Assuming df is your dataset
metadata[3, 6] <- "salt"
metadata[3, 8] <- "NSI"
metadata[4, 6] <- "background"
metadata[4, 8] <- "RBI"

#metadata1 <- metadata[,-6]
head(metadata)
#metadata_IMP <- metadata1 %>%
# filter(!is.na(F), System == "IMP")
metadata$Temperature[metadata$Temperature == "normal"] <- "control"
metadata$treatments[metadata$treatments == "NBN"] <- "NBC"
metadata$treatments[metadata$treatments == "NSN"] <- "NSC"
metadata$treatments[metadata$treatments == "RBN"] <- "RBC"
metadata$treatments[metadata$treatments == "RSN"] <- "RSC"
metadata$Temperature <- factor(metadata$Temperature)
metadata$Salinity <- factor(metadata$Salinity)
metadata$Velocity <- factor(metadata$Velocity)
metadata$Phase <- factor(metadata$Phase)
metadata$treatments <- factor(metadata$treatments)

# Reorder the factor levels
metadata$treatments <- factor(metadata$treatments, 
                              levels = c("NBC","RBC","NSC","NBI",
                                         "RSC","RBI","NSI","RSI"))

# make channel column as rownames
meta <- metadata %>% column_to_rownames(var="Channel") # change row names
str(meta)

#change order of factors in meta table
meta1 <- meta %>%
  mutate(Phase = fct_relevel(Phase,"stress", "recovery"))

# Combine var1 and var2 with an underscore separator
meta1$combined_var <- factor(paste(meta1$treatments, meta1$Phase, sep = "_"))


# plotting relative abundances of dominant taxa

########
# taxa plot
########
# Taxa plot
# working with microscopy dataset
# make the dataset tidy
diat2 <- diat %>% 
  pivot_longer(!Channel, names_to = "taxa", values_to = "count")


# Keep only taxa with proportion >= 4, or 1% of the total relative abundance per sample.

diat3 <- subset(diat2, count >= 4)
# Now we can spread the dataset of the most dominant taxa with at least 1% of proportion per sample

diat4 <- diat3 %>%
  pivot_wider(names_from = taxa, values_from = count, values_fn = sum, values_fill = 0)

# make Channel column as rownames
diat5 <- diat4 %>% column_to_rownames(var="Channel") # change row names

#####
# Replace all taxa with lower than 1% occurrence in the sample with Others
diat6 <- diat2 %>% mutate(taxa = replace(taxa, count < 12, "Others <3%"))

# Now we can spread the dataset of the most dominant taxa including the rare ones as other <3%

diat7 <- diat6 %>%
  pivot_wider(names_from = taxa, values_from = count, values_fn = sum, values_fill = 0)

# make Channel column as rownames
diat8 <- diat7 %>% column_to_rownames(var="Channel") # change row names

# plotting relative abundances of dominant taxa

set.seed(123)
x<-diat8/rowSums(diat8)
x<-x[,order(colSums(x),decreasing=TRUE)]
head(x)

#Extract list of top N Taxa
set.seed(123)
N<-35
taxa_list<-diat8
N<-length(taxa_list)

#Create a custom color scale
library(RColorBrewer)
# Define the number of colors you want
nb.cols <- 40
mycolors <- colorRampPalette(brewer.pal(12, "Paired"))(nb.cols)

#install.packages("randomcoloR")
library(randomcoloR)
n <- 40
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
pie(rep(1,n), col=sample(col_vector, n))
palette <- distinctColorPalette(n)

# Make a new dataframe
df<-NULL
for (i in 1:dim(x)[2]){
  tmp<-data.frame(row.names=NULL,Sample=rownames(meta1), Salinity=meta1$Salinity,
                  Velocity=meta$Velocity, Temperature=meta1$Temperature, 
                  Phase=meta1$Phase, Treatments=meta1$treatments,
                  Taxa=rep(colnames(x)[i],dim(x)[1]),Value=x[,i])
  if(i==1){df<-tmp} else {df<-rbind(df,tmp)}
}

# Taxa plot based on Phase

exstream2022_dm <- ggplot(df, aes(x=Sample,y=Value, fill=Taxa))+geom_bar(stat="identity")+
  facet_grid(. ~ Phase, drop=TRUE,scale="free",space="free_x")+
  scale_fill_manual(values=palette[1:(N+1)]) + theme_bw()+ylab("Relative abundance") +
  scale_y_continuous(expand = c(0,0))+theme(strip.background = element_rect(fill="gray85"))+
  theme(panel.margin = unit(0.3, "lines")) +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=0.5, size = 6, face = "bold"), 
        strip.text = element_text(size = 14, face = "bold"))+
  theme(legend.key.size = unit(0.6, "cm"), legend.key.width = unit(0.6,"cm"), 
        legend.text = element_text(size = 11))+
  guides(fill=guide_legend(ncol=1))

exstream2022_dm
# save the plot
#ggsave("exstream2022_dm.tiff", units="in", width=12, height=12, dpi=300, compression = 'lzw')

# facet_warp treatments
exstream2022_dm_edited <- ggplot(df, aes(x=Sample,y=Value, fill=Taxa))+geom_bar(stat="identity")+
  facet_grid(Phase ~ Treatments, drop=TRUE,scale="free",space="free_x")+
  scale_fill_manual(values=palette[1:(N+1)]) + theme_bw()+ylab("Relative abundance") +
  scale_y_continuous(expand = c(0,0))+theme(strip.background = element_rect(fill="gray85"))+
  theme(panel.margin = unit(0.3, "lines")) +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=0.5, size = 7, face = "bold"), 
        strip.text = element_text(size = 9, face = "bold"))+
  theme(legend.position = "bottom", legend.key.size = unit(0.4, "cm"), legend.key.width = unit(0.4,"cm"), 
        legend.text = element_text(size = 9))+
  guides(fill=guide_legend(nrow = 5))

exstream2022_dm_edited
# save the plot
ggsave("exstream2022_dm_edited.tiff", units="in", width=14, height=7, dpi=600, compression = 'lzw')

exstream2022_dna_edited <- ggplot(df_18S_others, aes(x=Sample,y=Value, fill=Taxa))+geom_bar(stat="identity")+
  facet_grid(Phase ~ Treatments, drop=TRUE,scale="free",space="free_x")+
  scale_fill_manual(values=palette[1:(N+1)]) + theme_bw()+ylab("Relative read abundance") +
  scale_y_continuous(expand = c(0,0))+theme(strip.background = element_rect(fill="gray85"))+
  theme(panel.margin = unit(0.3, "lines")) +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=0.7, size = 7, face = "bold"), 
        strip.text = element_text(size = 9, face = "bold"))+
  theme(legend.position = "bottom", legend.key.size = unit(0.4, "cm"), legend.key.width = unit(0.4,"cm"), 
        legend.text = element_text(size = 9))+
  guides(fill=guide_legend(nrow = 3))

exstream2022_dna_edited
# save the plot
ggsave("exstream2022_dna_treat2.tiff", units="in", width=17, height=7, dpi=300, compression = 'lzw')
#install.packages("ggpubr")
library(ggpubr)
combined_taxa_plotEdited <- ggarrange(exstream2022_dm_edited, exstream2022_dna_edited, 
                                      ncol=1, labels = c("A", "B"))
combined_taxa_plotEdited

ggsave("combined_taxa_plotEdited.tiff", units="in", width=14, height=14, dpi=600, compression = 'lzw')

# plotting relative abundances of dominant taxa with taxa <3% included as others

set.seed(123)
x<-diat8/rowSums(diat8)
x<-x[,order(colSums(x),decreasing=TRUE)]
head(x)

#Extract list of top N Taxa
N<-65
taxa_list<-diat8
N<-length(taxa_list)

#Create a custom color scale
library(RColorBrewer)
# Define the number of colors you want
nb.cols <- 65
mycolors <- colorRampPalette(brewer.pal(12, "Paired"))(nb.cols)

#install.packages("randomcoloR")
library(randomcoloR)
n <- 65
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
pie(rep(1,n), col=sample(col_vector, n))
palette <- distinctColorPalette(n)

# Make a new dataframe
df<-NULL
for (i in 1:dim(x)[2]){
  tmp<-data.frame(row.names=NULL,Sample=rownames(x), Salinity=meta1$Salinity,
                  Velocity=meta$Velocity, Temperature=meta1$Temperature, Phase=meta1$Phase,
                  Treatments=meta1$treatments, Taxa=rep(colnames(x)[i],dim(x)[1]),Value=x[,i])
  if(i==1){df<-tmp} else {df<-rbind(df,tmp)}
}

# Taxa plot based on Phase

exstream2022_dm1 <- ggplot(df, aes(x=sample,y=Value, fill=Taxa))+geom_bar(stat="identity")+
  facet_grid(. ~ Treatments, drop=TRUE,scale="free",space="free_x")+
  scale_fill_manual(values=palette[1:(N+1)]) + theme_bw()+ylab("Relative abundance") +
  scale_y_continuous(expand = c(0,0))+theme(strip.background = element_rect(fill="gray85"))+
  theme(panel.margin = unit(0.3, "lines")) +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=0.5, size = 6, face = "bold"), 
        strip.text = element_text(size = 14, face = "bold"))+
  theme(legend.key.size = unit(0.6, "cm"), legend.key.width = unit(0.6,"cm"),  
        legend.text = element_text(size = 11, face = "italic"))+
  guides(fill=guide_legend(ncol=1))

exstream2022_dm1
# save the plot
#ggsave("exstream2022_dm1.tiff", units="in", width=12, height=12, dpi=300, compression = 'lzw')

################################
# nMDS --------------------------------------------------------------------
# digital microscopy data
# microscopy

library(vegan)

##### IMP comment: here you need to make sure that the order of the metadata 
##### and the order of the distance matrix is identical. Currently, it is not:
set.seed(123)
meta1 %>% rownames # starts with C05
diat5 %>% rownames # starts with C61

meta1 <- meta1[order(row.names(meta1)),]
diat5 <- diat5[order(row.names(diat5)),]

#### IMP comment end

spp.log <- decostand(diat5, method = "log")

# Hellinger transformation
spp.hell <- decostand(diat5, method = "hellinger")

spp.log <- decostand(diat5, method = "log")

# presence/absence
spp.pa_dm <- decostand(diat5, method = "pa")

spp.log.dis <- vegdist(spp.log, method = "bray")
# hellinger transformed
spp.hell.dis <- vegdist(spp.hell, method = "bray")
spp.log
spp.log.dis
# check
decorana (spp.log) # The length of the first DCA axis is 2.7 S.D. (i.e. < 3. S.D.), 
# and these data are thus suitable for linear ordination methods.
#library(vegan)

# Logarithmic transformation as suggested by Anderson et al. (2006): log_b (x) + 1 for x > 0, where b is the base of 
# the logarithm; zeros are left as zeros. 



# betadisper --------------------------------------------------------------

# Before doing the PERMANOVA, first we check to see if the dispersion is the same
# Homogeneity of groups
# betadisper studies the differences in group homogeneities
# analogous to Levene's test of the equality of variances
# can only use one factor as an independent variable

# treatments
(mod.treatments <- with(meta1, betadisper(spp.hell.dis, meta1$treatments)))
plot(mod.treatments, sub = NULL) 
boxplot(mod.treatments)
anova(mod.treatments)   # this says that within-group variances can be considered homogeneous - in fact a good news!
# next step could be to compare the location of centroids and see if there is
# significant difference between groups in that respect!
permutest(mod.treatments)
# temperature
(mod.temperature <- with(meta1, betadisper(spp.hell.dis, meta1$Temperature)))
plot(mod.temperature, sub = NULL) 
boxplot(mod.temperature)
anova(mod.temperature)   # this says that within-group variances can be considered homogeneous - in fact a good news!
# next step could be to compare the location of centroids and see if there is
# significant difference between groups in that respect!
permutest(mod.temperature)
# salinity
(mod.salinity <- with(meta1, betadisper(spp.hell.dis, meta1$Salinity)))
plot(mod.salinity, sub = NULL) 
boxplot(mod.salinity)
anova(mod.salinity)   # this says that within-group variances can be considered homogeneous - in fact a good news!
# next step could be to compare the location of centroids and see if there is
# significant difference between groups in that respect!

##### IMP comment
# here we have a problem with salt having different dispersion 
##### IMP comment end

permutest(mod.salinity)
# velocity
(mod.velocity <- with(meta1, betadisper(spp.hell.dis, meta1$Velocity)))
plot(mod.velocity, sub = NULL) 
boxplot(mod.velocity)
anova(mod.velocity)   # this says that within-group variances can be considered homogeneous - in fact a good news!
# next step could be to compare the location of centroids and see if there is
# significant difference between groups in that respect!
permutest(mod.velocity)

# Phase
(mod.phase <- with(meta1, betadisper(spp.hell.dis, meta1$Phase)))
plot(mod.phase, sub = NULL) 
boxplot(mod.phase)
anova(mod.phase)   # this says that within-group variances can be considered homogeneous - in fact a good news!
# next step could be to compare the location of centroids and see if there is
# significant difference between groups in that respect!
permutest(mod.phase)

##### IMP comment
# also the phases have different dispersions 
#####

# PERMANOVA ---------------------------------------------------------------

# Permutational multivariate analysis of variance using distance matrices
# (Bray-Curtis similarities by default). ANOSIM uses only ranks of Bray-Curtis,
# so the former preserves more information.
# stressor phase

##### IMP comment start
# adonis2 seems not to know any subset argument and simply ignores it.
# Instead, you need to create different distance matrices for each of the 
# phases. For simplicity, I only do this below for the hellinger transformtion
##### IMP comments end

# make the metadata table only for stress and the distance matrix only for stress
stress.meta <- meta1 %>% filter(Phase == "stress")
stress.rows <- which(meta1$Phase == "stress")
stress.spp.hell <- spp.hell[stress.rows,]
# check channels
stress.meta %>% rownames
stress.spp.hell %>% rownames

# make the metadata table only for recovery and the distance matrix only for recovery
rec.meta <- meta1 %>% filter(Phase == "recovery")
rec.rows <- which(meta1$Phase == "recovery")
rec.spp.hell <- spp.hell[rec.rows,]
# check channels
rec.meta %>% rownames
rec.spp.hell %>% rownames

# create distance matrices
spp.hell.dis <- vegdist(spp.hell, method = "bray")
stress.spp.hell.dis <- vegdist(stress.spp.hell, method = "bray")
rec.spp.hell.dis <- vegdist(rec.spp.hell, method = "bray")

# make tests for stressor phase
# here, since we have an unbalanced design, we should not rely on the 
# anova type I sum of squares. The p-values will change depending on the order
# of the factors. Type II does not have this property in unbalanced designs

# type I
perm.1 <- adonis2(stress.spp.hell.dis ~ Temperature * Salinity * Velocity, 
                  data = stress.meta, permutations = 999); perm.1

# type II
perm.2a <- adonis2(stress.spp.hell.dis ~ Temperature * Salinity * Velocity, 
                  by = "margin", data = stress.meta, permutations = 999); perm.2a
perm.2b <- adonis2(stress.spp.hell.dis ~ Temperature * Salinity + Temperature * Velocity + Salinity * Velocity, 
                   by = "margin", data = stress.meta, permutations = 999); perm.2b
perm.2c <- adonis2(stress.spp.hell.dis ~ Temperature + Salinity + Velocity, 
                   by = "margin", data = stress.meta, permutations = 999); perm.2c

# make tests for recovery phase

# type I (should be stable for the rec phase, though)
perm.3 <- adonis2(rec.spp.hell.dis ~ Temperature * Salinity * Velocity, 
                  data = rec.meta, permutations = 999); perm.3

# type II
perm.4a <- adonis2(rec.spp.hell.dis ~ Temperature * Salinity * Velocity, 
                   by = "margin", data = rec.meta, permutations = 999); perm.4a
perm.4b <- adonis2(rec.spp.hell.dis ~ Temperature * Salinity + Temperature * Velocity + Salinity * Velocity, 
                   by = "margin", data = rec.meta, permutations = 999); perm.4b
perm.4c <- adonis2(rec.spp.hell.dis ~ Temperature + Salinity + Velocity, 
                   by = "margin", data = rec.meta, permutations = 999); perm.4c

# For the difference between phases
perm.5 <- adonis2(spp.hell.dis ~ Phase * treatments, 
                   data = meta1, permutations = 999); perm.5

perm.6a <- adonis2(spp.hell.dis ~ Phase * treatments, by = "margin",
                  data = meta1, permutations = 999); perm.6a

perm.6b <- adonis2(spp.hell.dis ~ Phase + treatments, by = "margin",
                   data = meta1, permutations = 999); perm.6b

###################
# Pairwise PERMANOVA comparisons of diatom communities at the end of recovery
library(pairwiseAdonis)
pairwise_result <- pairwise.adonis(rec.spp.hell.dis, rec.meta$treatments)
print(pairwise_result)

#########################################################################################
# PERMANOVA on 18S-V9 amplicon sequencing data
###################################################################################
set.seed(123)
meta1 %>% rownames # starts with C05
dna_data4 %>% rownames # starts with C61

dim(dna_data4)
meta1 <- meta1[order(row.names(meta1)),]
dna_data4 <- dna_data4[order(row.names(dna_data3)),]

spp.log.alg <- decostand(dna_data4, method = "log")
spp.hell.alg <- decostand(dna_data4, method = "hellinger")
spp.log.alg.dis <- vegdist(spp.log.alg, method = "bray")
spp.hell.alg.dis <- vegdist(spp.hell.alg, method = "bray")

#### IMP comment end
# log transformation
spp.log.alg <- decostand(dna_data4, method = "log")
# Hellinger transformation
spp.hell.alg <- decostand(dna_data4, method = "hellinger")

# presence/absence
spp.pa.alg <- decostand(dna_data4, method = "pa")

#distance matrix
spp.log.alg.dis <- vegdist(spp.log.alg, method = "bray")
# hellinger transformed
spp.hell.alg.dis <- vegdist(spp.hell.alg, method = "bray")
spp.log.alg
spp.log.alg.dis
# check
decorana (spp.log.alg) # The length of the first DCA axis is 2.7 S.D. (i.e. < 3. S.D.), 
# and these data are thus suitable for linear ordination methods.
#library(vegan)

# Logarithmic transformation as suggested by Anderson et al. (2006): log_b (x) + 1 for x > 0, where b is the base of 
# the logarithm; zeros are left as zeros. 



# betadisper --------------------------------------------------------------

# Before doing the PERMANOVA, first we check to see if the dispersion is the same
# Homogeneity of groups
# betadisper studies the differences in group homogeneities
# analogous to Levene's test of the equality of variances
# can only use one factor as an independent variable

# treatments
(mod.treatments <- with(meta1, betadisper(spp.hell.alg.dis, meta1$treatments)))
plot(mod.treatments, sub = NULL) 
boxplot(mod.treatments)
anova(mod.treatments)   # this says that within-group variances can be considered homogeneous - in fact a good news!
# next step could be to compare the location of centroids and see if there is
# significant difference between groups in that respect!
permutest(mod.treatments)
# temperature
(mod.temperature <- with(meta1, betadisper(spp.hell.alg.dis, meta1$Temperature)))
plot(mod.temperature, sub = NULL) 
boxplot(mod.temperature)
anova(mod.temperature)   # this says that within-group variances can be considered homogeneous - in fact a good news!
# next step could be to compare the location of centroids and see if there is
# significant difference between groups in that respect!
permutest(mod.temperature)
# salinity
(mod.salinity <- with(meta1, betadisper(spp.hell.alg.dis, meta1$Salinity)))
plot(mod.salinity, sub = NULL) 
boxplot(mod.salinity)
anova(mod.salinity)   # this says that within-group variances can be considered homogeneous - in fact a good news!
# next step could be to compare the location of centroids and see if there is
# significant difference between groups in that respect!

##### IMP comment
# here we have a problem with salt having different dispersion 
##### IMP comment end

permutest(mod.salinity)
# velocity
(mod.velocity <- with(meta1, betadisper(spp.hell.alg.dis, meta1$Velocity)))
plot(mod.velocity, sub = NULL) 
boxplot(mod.velocity)
anova(mod.velocity)   # this says that within-group variances can be considered homogeneous - in fact a good news!
# next step could be to compare the location of centroids and see if there is
# significant difference between groups in that respect!
permutest(mod.velocity)

# Phase
(mod.phase <- with(meta1, betadisper(spp.log.alg.dis, meta1$Phase)))
plot(mod.phase, sub = NULL) 
boxplot(mod.phase)
anova(mod.phase)   # this says that within-group variances can be considered homogeneous - in fact a good news!
# next step could be to compare the location of centroids and see if there is
# significant difference between groups in that respect!
permutest(mod.phase)

##### IMP comment
# also the phases have different dispersions 
#####

# PERMANOVA ---------------------------------------------------------------

# Permutational multivariate analysis of variance using distance matrices
# (Bray-Curtis similarities by default). ANOSIM uses only ranks of Bray-Curtis,
# so the former preserves more information.
# stressor phase

##### IMP comment start
# adonis2 seems not to know any subset argument and simply ignores it.
# Instead, you need to create different distance matrices for each of the 
# phases. For simplicity, I only do this below for the hellinger transformtion
##### IMP comments end

# make the metadata table only for stress and the distance matrix only for stress
stress.meta <- meta1 %>% filter(Phase == "stress")
stress.rows <- which(meta1$Phase == "stress")
stress.spp.hell.alg <- spp.hell.alg[stress.rows,]
# check channels
stress.meta %>% rownames
stress.spp.hell %>% rownames

# make the metadata table only for recovery and the distance matrix only for recovery
rec.meta <- meta1 %>% filter(Phase == "recovery")
rec.rows <- which(meta1$Phase == "recovery")
rec.spp.hell.alg <- spp.hell.alg[rec.rows,]
# check channels
rec.meta %>% rownames
rec.spp.hell.alg %>% rownames

# create distance matrices
spp.hell.alg.dis <- vegdist(spp.hell.alg, method = "bray")
stress.spp.hell.alg.dis <- vegdist(stress.spp.hell.alg, method = "bray")
rec.spp.hell.alg.dis <- vegdist(rec.spp.hell.alg, method = "bray")

# make tests for stressor phase
# here, since we have an unbalanced design, we should not rely on the 
# anova type I sum of squares. The p-values will change depending on the order
# of the factors. Type II does not have this property in unbalanced designs

# type I
perm.1 <- adonis2(stress.spp.hell.alg.dis ~ Temperature * Salinity * Velocity, 
                  data = stress.meta, permutations = 999); perm.1

# type II
perm.2a <- adonis2(stress.spp.hell.alg.dis ~ Temperature * Salinity * Velocity, 
                   by = "margin", data = stress.meta, permutations = 999); perm.2a
perm.2b <- adonis2(stress.spp.hell.alg.dis ~ Temperature * Salinity + Temperature * Velocity + Salinity * Velocity, 
                   by = "margin", data = stress.meta, permutations = 999); perm.2b
perm.2c <- adonis2(stress.spp.hell.alg.dis ~ Temperature + Salinity + Velocity, 
                   by = "margin", data = stress.meta, permutations = 999); perm.2c

# make tests for recovery phase

# type I (should be stable for the rec phase, though)
perm.3 <- adonis2(rec.spp.hell.alg.dis ~ Temperature * Salinity * Velocity, 
                  data = rec.meta, permutations = 999); perm.3

# type II
perm.4a <- adonis2(rec.spp.hell.alg.dis ~ Temperature * Salinity * Velocity, 
                   by = "margin", data = rec.meta, permutations = 999); perm.4a
perm.4b <- adonis2(rec.spp.hell.alg.dis ~ Temperature * Salinity + Temperature * Velocity + Salinity * Velocity, 
                   by = "margin", data = rec.meta, permutations = 999); perm.4b
perm.4c <- adonis2(rec.spp.hell.alg.dis ~ Temperature + Salinity + Velocity, 
                   by = "margin", data = rec.meta, permutations = 999); perm.4c
##############################################################################
# Pairwise PERMANOVA comparisons of diatom communities at the end of recovery
library(pairwiseAdonis)
pairwise_result <- pairwise.adonis(rec.spp.hell.alg.dis, rec.meta$treatments)
print(pairwise_result)

#########################################
# Extract PERMANOVA results
permanova_result <- adonis2(species_data ~ predictor1 * predictor2, data = your_data, permutations = 999)
#library(devtools)
#install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
library(cluster)
library(pairwiseAdonis)
# Perform pairwise post-hoc analysis
pairwise_test <- pairwise.adonis(rec.spp.hell.alg.dis ~ Temperature * Velocity, data = rec.meta, permutations = 999)
pairwise.adonis2(rec.spp.hell.alg.dis ~ Temperature * Velocity, grouping = rec.meta$Temperature, method = "bray", data = rec.meta)

# Adjust p-values for multiple comparisons
adjusted_pvalues <- p.adjust(pairwise_test$pvals, method = "bonferroni")

# Print the results
pairwise_results <- cbind(pairwise_test$comparisons, pairwise_test$pvals, adjusted_pvalues)
colnames(pairwise_results) <- c("Comparison", "Unadjusted p-value", "Adjusted p-value")
print(pairwise_results)


##############################################################################
# For the difference between phases
perm.5 <- adonis2(spp.hell.alg.dis ~ Phase * treatments, 
                  data = meta1, permutations = 999); perm.5

perm.6a <- adonis2(spp.hell.alg.dis ~ Phase * treatments, by = "margin",
                   data = meta1, permutations = 999); perm.6a

perm.6b <- adonis2(spp.hell.alg.dis ~ Phase + treatments, by = "margin",
                   data = meta1, permutations = 999); perm.6b
####################################################
# indicator species analysis
########

library(indicspecies)
# indicator diatom species (DM)
# make the metadata table only for stress and the distance matrix only for stress
stress.meta <- meta1 %>% filter(Phase == "stress")
stress.diat <- which(meta1$Phase == "stress")
stress.diat <- diat5[stress.diat,]
# check channels
stress.meta %>% rownames
stress.diat %>% rownames

# make the metadata table only for recovery and the distance matrix only for recovery
rec.meta <- meta1 %>% filter(Phase == "recovery")
rec.diat <- which(meta1$Phase == "recovery")
rec.diat <- diat5[rec.diat,]
# check channels
rec.meta %>% rownames
rec.diat %>% rownames
stress.indval <- multipatt(stress.diat, stress.meta$Salinity, 
                    control = how(nperm=999))
summary(stress.indval) 
summary(stress.indval, indvalcomp=TRUE)

#### Indicator microalgae species (18S-V9)
# make the metadata table only for stress and the distance matrix only for stress
stress.meta <- meta1 %>% filter(Phase == "stress")
stress.dna <- which(meta1$Phase == "stress")
stress.dna <- dna_data3[stress.dna,]
# check channels
stress.meta %>% rownames
stress.dna %>% rownames

# make the metadata table only for recovery and the distance matrix only for recovery
rec.meta <- meta1 %>% filter(Phase == "recovery")
rec.dna <- which(meta1$Phase == "recovery")
rec.dna <- dna_data3[rec.dna,]
# check channels
rec.meta %>% rownames
rec.dna %>% rownames
stress.indval <- multipatt(rec.dna, stress.meta$Temperature, 
                           control = how(nperm=999))
summary(stress.indval) 
summary(stress.indval, indvalcomp=TRUE)

