# ExStream 2022
# Multivariate statistical analysis of microphytobenthos data
# testing the response of algae to salinity, temperature and flow velocity
# Microscopy & 18S-V9
# 01.10.2023
# Serge Mayombo

# load the libraries
library(tidyverse)
library(vegan)

# read data
# read environmental dataset

diat <- read.csv("exstream2022_IMP_microscopy.csv", sep = ",",
                          header = T)
# make channel column as rownames
diat1 <- diat %>% column_to_rownames(var="Channel") # change row names

# read exstream metadata
metadata <- read.csv("metadata_IMP2022.csv")
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
# make channel column as rownames
meta <- metadata %>% column_to_rownames(var="Channel") # change row names
str(meta)

#change order of factors in meta table
meta1 <- meta %>%
  mutate(Phase = fct_relevel(Phase,"stress", "recovery"))


# plotting relative abundances of dominant taxa

########
# taxa plot
########
# Taxa plot
# working with microscopy dataset
# make the dataset tidy
diat2 <- diat %>% 
  pivot_longer(!Channel, names_to = "taxa", values_to = "count")

# Keep only taxa with proportion >= 3, or 3% of the total relative abundance per sample.
diat3 <- subset(diat2, count >= 12)
# Now we can spread the dataset of the most dominant taxa with at least 5% of proportion per sample

diat4 <- diat3 %>%
  pivot_wider(names_from = taxa, values_from = count, values_fn = sum, values_fill = 0)

# make Channel column as rownames
diat5 <- diat4 %>% column_to_rownames(var="Channel") # change row names

#####
# Replace all taxa with lower than 3% occurrence in the sample with Others
diat6 <- diat2 %>% mutate(taxa = replace(taxa, count <= 12, "Others <3%"))

# Now we can spread the dataset of the most dominant taxa including the rare ones as other <3%

diat7 <- diat6 %>%
  pivot_wider(names_from = taxa, values_from = count, values_fn = sum, values_fill = 0)

# make Channel column as rownames
diat8 <- diat7 %>% column_to_rownames(var="Channel") # change row names

# plotting relative abundances of dominant taxa

set.seed(123)
x<-diat5/rowSums(diat5)
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
  tmp<-data.frame(row.names=NULL,Channel=rownames(x), Salinity=meta1$Salinity,
                  Velocity=meta$Velocity, Temperature=meta1$Temperature, 
                  Phase=meta1$Phase, Treatments=meta1$treatments,
                  Taxa=rep(colnames(x)[i],dim(x)[1]),Value=x[,i])
  if(i==1){df<-tmp} else {df<-rbind(df,tmp)}
}

# Taxa plot based on Phase

exstream2022_dm <- ggplot(df, aes(x=Channel,y=Value, fill=Taxa))+geom_bar(stat="identity")+
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
ggsave("exstream2022_dm.tiff", units="in", width=12, height=12, dpi=300, compression = 'lzw')

# facet_warp treatments
exstream2022_dm_treat <- ggplot(df, aes(x=Channel,y=Value, fill=Taxa))+geom_bar(stat="identity")+
  facet_grid(. ~ Treatments, drop=TRUE,scale="free",space="free_x")+
  scale_fill_manual(values=palette[1:(N+1)]) + theme_bw()+ylab("Relative abundance") +
  scale_y_continuous(expand = c(0,0))+theme(strip.background = element_rect(fill="gray85"))+
  theme(panel.margin = unit(0.3, "lines")) +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=0.5, size = 6, face = "bold"), 
        strip.text = element_text(size = 14, face = "bold"))+
  theme(legend.key.size = unit(0.6, "cm"), legend.key.width = unit(0.6,"cm"), 
        legend.text = element_text(size = 11))+
  guides(fill=guide_legend(ncol=1))

exstream2022_dm_treat
# save the plot
ggsave("exstream2022_dm_treat.tiff", units="in", width=12, height=12, dpi=300, compression = 'lzw')


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
  tmp<-data.frame(row.names=NULL,Channel=rownames(x), Salinity=meta1$Salinity,
                  Velocity=meta$Velocity, Temperature=meta1$Temperature, Phase=meta1$Phase,
                  Taxa=rep(colnames(x)[i],dim(x)[1]),Value=x[,i])
  if(i==1){df<-tmp} else {df<-rbind(df,tmp)}
}

# Taxa plot based on Phase

exstream2022_dm1 <- ggplot(df, aes(x=Channel,y=Value, fill=Taxa))+geom_bar(stat="identity")+
  facet_grid(. ~ Phase, drop=TRUE,scale="free",space="free_x")+
  scale_fill_manual(values=palette[1:(N+1)]) + theme_bw()+ylab("Relative abundance") +
  scale_y_continuous(expand = c(0,0))+theme(strip.background = element_rect(fill="gray85"))+
  theme(panel.margin = unit(0.3, "lines")) +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=0.5, size = 6, face = "bold"), 
        strip.text = element_text(size = 14, face = "bold"))+
  theme(legend.key.size = unit(0.6, "cm"), legend.key.width = unit(0.6,"cm"), 
        legend.text = element_text(size = 11))+
  guides(fill=guide_legend(ncol=1))

exstream2022_dm1
# save the plot
ggsave("exstream2022_dm1.tiff", units="in", width=12, height=12, dpi=300, compression = 'lzw')

################################
# nMDS --------------------------------------------------------------------
# digital microscopy data
# microscopy
spp.log <- decostand(diat1, method = "log")
# presence/absence
spp.pa_dm <- decostand(diat1, method = "pa")

spp.log.dis <- vegdist(spp.log, method = "bray")

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
(mod.treatments <- with(meta1, betadisper(spp.log.dis, meta1$treatments)))
plot(mod.treatments, sub = NULL) 
boxplot(mod.treatments)
anova(mod.treatments)   # this says that within-group variances can be considered homogeneous - in fact a good news!
# next step could be to compare the location of centroids and see if there is
# significant difference between groups in that respect!
permutest(mod.treatments)
# temperature
(mod.temperature <- with(meta1, betadisper(spp.log.dis, meta1$Temperature)))
plot(mod.temperature, sub = NULL) 
boxplot(mod.temperature)
anova(mod.temperature)   # this says that within-group variances can be considered homogeneous - in fact a good news!
# next step could be to compare the location of centroids and see if there is
# significant difference between groups in that respect!
permutest(mod.temperature)
# salinity
(mod.salinity <- with(meta1, betadisper(spp.log.dis, meta1$Salinity)))
plot(mod.salinity, sub = NULL) 
boxplot(mod.salinity)
anova(mod.salinity)   # this says that within-group variances can be considered homogeneous - in fact a good news!
# next step could be to compare the location of centroids and see if there is
# significant difference between groups in that respect!
permutest(mod.salinity)
# velocity
(mod.velocity <- with(meta1, betadisper(spp.log.dis, meta1$Velocity)))
plot(mod.velocity, sub = NULL) 
boxplot(mod.velocity)
anova(mod.velocity)   # this says that within-group variances can be considered homogeneous - in fact a good news!
# next step could be to compare the location of centroids and see if there is
# significant difference between groups in that respect!
permutest(mod.velocity)

# PERMANOVA ---------------------------------------------------------------

# Permutational multivariate analysis of variance using distance matrices
# (Bray-Curtis similarities by default). ANOSIM uses only ranks of Bray-Curtis,
# so the former preserves more information.

(perm.1 <- adonis2(spp.log.dis~(Temperature*Salinity*Velocity),
                   method = perm, data = meta1, permutations = 999))

(perm.1 <- adonis2(spp.log.dis~(Temperature+Salinity+Velocity),
                   method = perm, data = meta1))
# Nothing truns out to have significant effect in driving 
# diatom community structure in exstream 2022
##################################################
# NMDS
set.seed(123)
spp.nmds <- metaMDS(spp.log, k = 2,trymax = 100, permutation = 9999,
                    distance = "bray", wascores = TRUE)

spp.nmds
stressplot(spp.nmds)

##########################################
#############################
#spp.mds <- metaMDS((spe_count), distance = "bray", autotransform = F)
#spp.mds
env.fit <- envfit(spp.nmds, data, permutations = 999, na.rm = TRUE) # this fits environmental vectors
spp.fit <- envfit(spp.nmds, spe4, permutations = 999) # this fits species vectors

env.fit
spp.fit
# To plot the output from the mds using ggplot a new datasheet needs to be created which contains the x,y points for each site. 
#You can do this by calling the scores of you mds.

sample.scrs <- as.data.frame(scores(spp.nmds, display = "sites")) #save NMDS results into dataframe
sample.scrs <- cbind(sample.scrs, surrounding = data$surrounding) #add grouping variable "Management" to dataframe
#sample.scrs <- cbind(sample.scrs, year.of.restor. = metadata1$year.of.r) #add grouping variable of cluster grouping to dataframe
#site.scrs <- cbind(site.scrs, Site = rownames(site.scrs)) #add site names as variable if you want to display on plot
#sample.scrs <- cbind(sample.scrs, location = env$site)

head(sample.scrs)
#view(sample.scrs)

# A new dataset containing species data also needs to be made to look at species vectors.
#This is not necessary if you don't want to show the species on the final graph.
spp.scrs <- as.data.frame(scores(spp.fit, display = "vectors")) #save species intrinsic values into dataframe
spp.scrs <- cbind(spp.scrs, Species = rownames(spp.scrs)) #add species names to dataframe
spp.scrs <- cbind(spp.scrs, pval = spp.fit$vectors$pvals) #add pvalues to dataframe so you can select species which are significant
#spp.scrs<- cbind(spp.scrs, abrev = abbreviate(spp.scrs$Species, minlength = 6)) #abbreviate species names
sig.spp.scrs <- subset(spp.scrs, pval<=0.05) #subset data to show species significant at 0.05

head(spp.scrs)

sig.spp.scrs
# To show biologically extrinsic variables another datasheet needs to be created

env.scores <- as.data.frame(scores(env.fit, display = "vectors")) #extracts relevant scores from envifit
env.scores <- cbind(env.scores, env.variables = rownames(env.scores)) #and then gives them their names

env.scores <- cbind(env.scores, pval = env.fit$vectors$pvals) # add pvalues to dataframe
sig.env.scrs <- subset(env.scores, pval<=0.05) #subset data to show variables significant at 0.05

head(env.scores)
sig.env.scrs
env.scores
#Now we have the relevant information for plotting the ordination in ggplot! Lets get plotting!
#######################################
# add the hull
surrounding.rural <- sample.scrs[sample.scrs$surrounding == "rural", ][chull(sample.scrs[sample.scrs$surrounding == 
                                                                                           "rural", c("NMDS1", "NMDS2")]), ]  # hull values for rural areas
surrounding.urban <- sample.scrs[sample.scrs$surrounding == "urban", ][chull(sample.scrs[sample.scrs$surrounding == 
                                                                                           "urban", c("NMDS1", "NMDS2")]), ]  # hull values for urban areas


hull.data <- rbind(surrounding.rural, surrounding.urban)  #combine sampling site groups in the same dataset
hull.data

#########################################
# Basic ordination plot (restoration_date and season)
nmds.plot.diatoms <- ggplot(sample.scrs, aes(x=NMDS1, y=NMDS2), size = 7)+ #sets up the plot
  geom_point(aes(NMDS1, NMDS2, colour = factor(surrounding), shape = factor(surrounding)), size = 6)+ #adds site points to plot, shape determined season, colour determined by restorartion_date
  #geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,fill=surrounding,group=surrounding),alpha=0.30) +
  stat_ellipse(aes(fill=surrounding), alpha=.2,type='t',size =1, geom="polygon")+ ## add ellipses
  #geom_text(data=sample.scrs,aes(x=NMDS1,y=NMDS2,label=sample), vjust=0) +  # add the site labels
  coord_fixed()+
  theme_classic()+ 
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
  labs(colour = "surrounding", shape = "surrounding")+ # add legend labels for Management and Landuse
  theme(legend.position = "right", legend.text = element_text(size = 14), legend.title = element_text(size = 14), axis.text = element_text(size = 16)) + # add legend at right of plot
  annotate(geom = "label", x = -1.5, y = -2, size = 5,
           label = paste("Stress: ", round(spp.nmds$stress, digits = 3)))# add stress value

nmds.plot.diatoms + labs(title = "") # displays plot

# Significant species
nmds1 <- nmds.plot.diatoms +
  geom_segment(data = sig.spp.scrs, aes(x = 0, xend=NMDS1, y=0, yend=NMDS2), arrow = arrow(length = unit(0.25, "cm")), colour = "grey10", lwd=0.3) + #add vector arrows of significant species
  ggrepel::geom_text_repel(data = sig.spp.scrs, aes(x=NMDS1, y=NMDS2, label = Species), cex = 3, direction = "both", segment.size = 0.25)+ #add labels for species, use ggrepel::geom_text_repel so that labels do not overlap
  labs(title = "")

print(nmds1)

# Significant abiotic variables
nmds.plot.diatoms +
  geom_segment(data = sig.env.scrs, aes(x = 0, xend=NMDS1, y = 0, yend=NMDS2), arrow = arrow(length = unit(0.25, "cm")), colour = "grey10", lwd=0.3) + #add vector arrows of significant env variables
  ggrepel::geom_text_repel(data = sig.env.scrs, aes(x=NMDS1, y=NMDS2, label = env.variables), cex = 4, direction = "both", segment.size = 0.25)+ #add labels for env variables
  labs(title="")

# add both significant taxa and abiotic factors
kinzig2021_nmds_DM_paper <- nmds1 +
  geom_segment(data = sig.env.scrs, aes(x = 0, xend=NMDS1, y = 0, yend=NMDS2), arrow = arrow(length = unit(0.25, "cm")), colour = "red", lwd=0.3) + #add vector arrows of significant env variables
  ggrepel::geom_text_repel(data = sig.env.scrs, aes(x=NMDS1, y=NMDS2, label = env.variables), cex = 4, direction = "both", colour = "red", segment.size = 0.25)+ #add labels for env variables
  labs(title="")
kinzig2021_nmds_DM_paper

# save the plot
ggsave("kinzig2021_nmds_DM3%_paper2.tiff", units="in", width=7, height=7, dpi = 300, compression = 'lzw')

# Compute alpha diversity indices of the diatom communities
# *******************************************************

# Get help on the diversity() function
#library(vegan)
?diversity

N0 <- rowSums(diat1 > 0)         # Species richness
N0a <- specnumber(diat1)     # Species richness (alternate)
H <- diversity(diat1)       # Shannon entropy (base e)
Hb2 <- diversity(diat1, base = 2) # Shannon entropy (base 2)
N1 <- exp(H)                   # Shannon diversity (number of abundant species) (base e)
N1b2 <- 2^Hb2                 # Shannon diversity (base 2)
N2 <- diversity(diat1, "inv")    # Simpson diversity (number of dominant species)
J <- H/log(N0)                 # Pielou evenness
E10 <- N1/N0                   # Shannon evenness (Hill's ratio)
E20 <- N2/N0                   # Simpson evenness (Hill's ratio)
(div <- data.frame(N0, N0a, H, Hb2, N1, N1b2, N2, E10, E20, J))
# make div dataset rownames as first column
div1 <- tibble::rownames_to_column(div, "Channel")
# assigning new column names to some columns in div dataframe
colnames(div1)[2] <- "Richness"
colnames(div1)[3] <- "Richness (alternate)"
colnames(div1)[4] <- "Shannon"
colnames(div1)[5] <- "Shannon entropy"
colnames(div1)[6] <- "Shannon diversity (base e)"
colnames(div1)[7] <- "Shannon diversity (base2)"
colnames(div1)[8] <- "Simpson"
colnames(div1)[9] <- "Shannon evenness"
colnames(div1)[10] <- "Simpson evenness"
colnames(div1)[11] <- "Pielou"

#write.csv(div1, file = "div_kinzig2021.csv")

# Combine this div dataset with environmental variables dataset
meta2 <- tibble::rownames_to_column(meta1, "Channel")
div2 <- left_join(div1, meta2)

range(div2$Richness)
range(div2$Shannon)
# select only rural sites
div_stress <- div2 %>%
  filter(Phase == "stress")
range(div_stress$Richness)
range(div_stress$Shannon)
mean(div_stress$Richness)
mean(div_stress$Shannon)
# Select only urban sites 
div_rec <- div2 %>%
  filter(Phase == "recovery")
range(div_rec$Richness)
range(div_rec$Shannon)
mean(div_rec$Richness)
mean(div_rec$Shannon)

# remove column 3 in div2
div2 <- div2[,-3]


# run multiple comparisons for analysis of variance (t-test or ANOVA)
# Multiple comparisons
# install.packages("ggstatsplot")
library(ggstatsplot)
# Comparison between species

# edit from here
x <- "Velocity"
cols <- 2:11 # the 4 continuous dependent variables
type <- "parametric" # given the large number of observations, we use the parametric version
paired <- FALSE # FALSE for independent samples, TRUE for paired samples

# edit until here

# edit at your own risk
plotlist <-
  purrr::pmap(
    .l = list(
      data = list(as_tibble(div2)),
      x = x,
      y = as.list(colnames(div2)[cols]),
      plot.type = "box", # for boxplot
      type = type, # parametric or nonparametric
      pairwise.comparisons = FALSE, # to run post-hoc tests if more than 2 groups
      pairwise.display = "significant", # show only significant differences
      bf.message = FALSE, # remove message about Bayes Factor
      centrality.plotting = TRUE # remove central measure
    ),
    .f = ifelse(paired, # automatically use ggwithinstats if paired samples, ggbetweenstats otherwise
                ggstatsplot::ggwithinstats,
                ggstatsplot::ggbetweenstats
    )
  )

# print all plots together with statistical results
for (i in 1:length(plotlist)) {
  print(plotlist[[i]] +
          labs(caption = NULL)) # remove caption
}



# Sequencing data
# @Serge: I simplified the below part a bit
# I hope I got the important steps!
dna <- read.table("output_exstream2022_18S_clean.csv", sep=",", 
                  header=T, row.names = 1)

view(dna[grep("Metazoa", dna$Division),])

view(dna[grep("lata$", dna$Division),])
# keep only algae
algae <- view(dna[grep("phyta$|lata$", dna$Division),])
# recovery
recovery <- algae[, grep("_REC$", names(algae))]
#view(algae[grep("plas", algae$Division),])
#view(dna[grep("Metazoa", algae$Division),])
diat_18S <- view(algae[grep("Bacillariophyta", algae$Class),])

# readcounts dataframe

readcounts <- algae[,1:96]
# remove recovery samples
# Selecting columns with names ending in "_rec"
readcounts1 <- readcounts[, -grep("_REC$", names(readcounts))]
dim(readcounts1)
# transpose dataset

dna_spe_trans <- t(readcounts1)
total_reads_count <- rowSums(dna_spe_trans)
view(total_reads_count)
sum(total_reads_count)
range(total_reads_count)
mean(total_reads_count)


sapply(dna_spe_trans, class)

#taxa
spec <- algae[,98:105]
spec_trans <- t(spec)
sapply(spec_trans, class)

spp.log_alg <- decostand(dna_spe_trans, method = "log")
spp.log.dis_alg <- vegdist(spp.log_alg, method = "bray")


# betadisper --------------------------------------------------------------

# Before doing the PERMANOVA, first we check to see if the dispersion is the same
# Homogeneity of groups
# betadisper studies the differences in group homogeneities
# analogous to Levene's test of the equality of variances
# can only use one factor as an independent variable

# Temperature
(mod.temperature <- with(meta1, betadisper(spp.log.dis_alg, meta$Temperature)))
plot(mod.temperature, sub = NULL) 
boxplot(mod.temperature)
anova(mod.temperature)   # this says that within-group variances can be considered homogeneous - in fact a good news!
# next step could be to compare the location of centroids and see if there is
# significant difference among groups in that respect!
permutest(mod.temperature)

#salinity
(mod.salinity <- with(meta1, betadisper(spp.log.dis_alg, meta1$Salinity)))
plot(mod.salinity)
boxplot(mod.salinity)
anova(mod.salinity)
permutest(mod.salinity)

# flow velocity
(mod.velocity <- with(meta1, betadisper(spp.log.dis_alg, meta1$Velocity)))
plot(mod.velocity)
boxplot(mod.velocity)
anova(mod.velocity)
permutest(mod.velocity)

# PERMANOVA ---------------------------------------------------------------

# Permutational multivariate analysis of variance using distance matrices
# (Bray-Curtis similarities by default). ANOSIM uses only ranks of Bray-Curtis,
# so the former preserves more information.

(perm.1 <- adonis2(spp.log.dis_alg~(Temperature*Salinity*Velocity),
                   method = perm, data = meta1, permutations = 999))

(perm.1 <- adonis2(spp.log.dis_alg~(Temperature+Salinity+Velocity),
                   method = perm, data = meta1))
# Nothing truns out to have significant effect in driving 
# diatom community structure in exstream 2022
