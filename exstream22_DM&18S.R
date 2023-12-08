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
diat <- read.csv("exstream2022_IMP_microscopy.csv", sep = ",",
                 header = T)
diat1 <- diat[,-c(60,61)]
view(diat1)

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
<<<<<<< HEAD
                              levels = c("NBC","RBC","NSC","NBI",
                                         "RSC","RBI","NSI","RSI"))
=======
                          levels = c("NBC","RBC","NSC","NBI",
                                     "RSC","RBI","NSI","RSI"))
>>>>>>> 5997fedc77a4a453d580e63d133156747a2321ea
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

<<<<<<< HEAD
# Keep only taxa with proportion >= 4, or 1% of the total relative abundance per sample.
=======
# Keep only taxa with proportion >= 4, or 3% of the total relative abundance per sample.
>>>>>>> 5997fedc77a4a453d580e63d133156747a2321ea
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
nb.cols <- 80
mycolors <- colorRampPalette(brewer.pal(12, "Paired"))(nb.cols)

#install.packages("randomcoloR")
library(randomcoloR)
n <- 80
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
ggsave("exstream2022_dm.tiff", units="in", width=12, height=12, dpi=300, compression = 'lzw')

# facet_warp treatments
exstream2022_dm_treat2 <- ggplot(df, aes(x=Sample,y=Value, fill=Taxa))+geom_bar(stat="identity")+
  facet_grid(Phase ~ Treatments, drop=TRUE,scale="free",space="free_x")+
  scale_fill_manual(values=palette[1:(N+1)]) + theme_bw()+ylab("Relative abundance") +
  scale_y_continuous(expand = c(0,0))+theme(strip.background = element_rect(fill="gray85"))+
  theme(panel.margin = unit(0.3, "lines")) +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=0.5, size = 7, face = "bold"), 
        strip.text = element_text(size = 9, face = "bold"))+
  theme(legend.position = "bottom", legend.key.size = unit(0.4, "cm"), legend.key.width = unit(0.4,"cm"), 
        legend.text = element_text(size = 9))+
  guides(fill=guide_legend(nrow = 5))

exstream2022_dm_treat2
# save the plot
ggsave("exstream2022_dm_treat2.tiff", units="in", width=13, height=7, dpi=300, compression = 'lzw')


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
ggsave("exstream2022_dm1.tiff", units="in", width=12, height=12, dpi=300, compression = 'lzw')

################################
# nMDS --------------------------------------------------------------------
# digital microscopy data
# microscopy
<<<<<<< HEAD
library(vegan)
spp.log <- decostand(diat5, method = "log")
# Hellinger transformation
spp.log.hell <- decostand(diat5, method = "hellinger")
=======
spp.log <- decostand(diat5, method = "log")
>>>>>>> 5997fedc77a4a453d580e63d133156747a2321ea
# presence/absence
spp.pa_dm <- decostand(diat5, method = "pa")

spp.log.dis <- vegdist(spp.log, method = "bray")
# hellinger transformed
spp.log.hell.dis <- vegdist(spp.log.hell, method = "bray")
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

# Phase
(mod.phase <- with(meta1, betadisper(spp.log.dis, meta1$Phase)))
plot(mod.phase, sub = NULL) 
boxplot(mod.phase)
anova(mod.phase)   # this says that within-group variances can be considered homogeneous - in fact a good news!
# next step could be to compare the location of centroids and see if there is
# significant difference between groups in that respect!
permutest(mod.phase)

# PERMANOVA ---------------------------------------------------------------

# Permutational multivariate analysis of variance using distance matrices
# (Bray-Curtis similarities by default). ANOSIM uses only ranks of Bray-Curtis,
# so the former preserves more information.
# stressor phase
<<<<<<< HEAD
(perm.1 <- adonis2(spp.log.dis~(Temperature*Salinity*Velocity*Phase), 
                   subset = Phase == "stress",
                   method = perm, data = meta1, permutations = 999))

# Hellinger transformed
(perm.1 <- adonis2(spp.log.hell.dis~(Temperature*Salinity*Velocity*Phase), 
                   subset = Phase == "stress",
                   method = perm, data = meta1, permutations = 999))

(perm.1 <- adonis2(spp.log.hell.dis~(Temperature*Salinity*Velocity*Phase), 
                   subset = Phase == "recovery",
                   method = perm, data = meta1, permutations = 999))


(perm.1 <- adonis2(spp.log.hell.dis~(Temperature*Salinity*Velocity*Phase), 
                   method = perm, data = meta1, permutations = 999))

# recovery phase
(perm.1 <- adonis2(spp.log.dis~(Temperature*Salinity*Velocity), 
                   subset = Phase == "recovery",
                   method = perm, data = meta1, permutations = 999))

(perm.1 <- adonis2(spp.log.dis~Velocity, 
                   subset = Phase == "recovery",
                   method = perm, data = meta1, permutations = 999))
# all together
(perm.1 <- adonis2(spp.log.dis~(Temperature*Salinity*Velocity), 
                   method = perm, data = meta1, permutations = 999))

(perm.1 <- adonis2(spp.log.dis~Velocity, 
                   method = perm, data = meta1, permutations = 999))

(perm.1 <- adonis2(spp.log.dis~(treatments*Phase), 
                   method = perm, data = meta1, permutations = 999))

(perm.1 <- adonis2(spp.log.dis~treatments, 
                   method = perm, data = meta1, permutations = 999))
#install.packages("emmeans")

(perm.1 <- adonis2(spp.log.dis~Phase, 
                   method = perm, data = meta1, permutations = 999))

install.packages("pairwiseAdonis")
library(pairwiseAdonis)
library(emmeans)
# Perform post-hoc tests
# Fit the constrained ordination model (CCA)
cca_model <- cca(diat5 ~ treatments, data = meta1)

# Perform anova.cca
anova_result <- anova.cca(cca_model, by = "terms")

# Perform post-hoc tests
posthoc_result <- ordiR2step(cca_model, scope = ~ treatments, perm.max = 999)

# Display results
print(anova_result)
print(posthoc_result)
# Round the values to 2 decimal places
result_perm1 <- lapply(perm.1, function(x) round(x, 2))
print(result_perm1)

=======
(perm.1 <- adonis2(spp.log.dis~(Temperature*Salinity*Velocity), 
                   subset = Phase == "stress",
                   method = perm, data = meta1, permutations = 999))

(perm.1 <- adonis2(spp.log.dis~Velocity, 
                   subset = Phase == "stress",
                   method = perm, data = meta1, permutations = 999))


# recovery phase
(perm.1 <- adonis2(spp.log.dis~(Temperature*Salinity*Velocity), 
                   subset = Phase == "recovery",
                   method = perm, data = meta1, permutations = 999))

(perm.1 <- adonis2(spp.log.dis~Velocity, 
                   subset = Phase == "recovery",
                   method = perm, data = meta1, permutations = 999))
# all together
(perm.1 <- adonis2(spp.log.dis~(Temperature*Salinity*Velocity), 
                   method = perm, data = meta1, permutations = 999))

(perm.1 <- adonis2(spp.log.dis~Velocity, 
                   method = perm, data = meta1, permutations = 999))

(perm.1 <- adonis2(spp.log.dis~(treatments*Phase), 
                   method = perm, data = meta1, permutations = 999))

(perm.1 <- adonis2(spp.log.dis~treatments, 
                   method = perm, data = meta1, permutations = 999))
#install.packages("emmeans")

(perm.1 <- adonis2(spp.log.dis~Phase, 
                   method = perm, data = meta1, permutations = 999))

install.packages("pairwiseAdonis")
library(pairwiseAdonis)
library(emmeans)
# Perform post-hoc tests
# Fit the constrained ordination model (CCA)
cca_model <- cca(diat5 ~ treatments, data = meta1)

# Perform anova.cca
anova_result <- anova.cca(cca_model, by = "terms")

# Perform post-hoc tests
posthoc_result <- ordiR2step(cca_model, scope = ~ treatments, perm.max = 999)

# Display results
print(anova_result)
print(posthoc_result)
# Round the values to 2 decimal places
result_perm1 <- lapply(perm.1, function(x) round(x, 2))
print(result_perm1)

>>>>>>> 5997fedc77a4a453d580e63d133156747a2321ea
(perm.1 <- adonis2(spp.log.dis~treatments*Phase*combined_var,
                   method = perm, data = meta1, permutations = 999))

(perm.1 <- adonis2(spp.log.dis~(Temperature+Salinity+Velocity),
                   method = perm, data = meta1))
###################################################
# comparing each single treatment against control
# rownames to columns:
# during stressor phase
meta_str <- meta1 %>% subset(Phase=="stress")

# select the 8 channels in the community dataset
# RBC ~ NBC
# select treatments for comparison of the communities
RBC_NBC <- meta_str %>% subset(treatments=="RBC"|treatments=="NBC")
RBC_NBC$treatments <- factor(RBC_NBC$treatments)
diat_veloc <- diat5[c(16,18,23,25,28,30,34,38),]
spp.log1 <- decostand(diat_veloc, method = "log")
spp.log.dis1 <- vegdist(spp.log1, method = "bray")
(perm.1 <- adonis2(spp.log.dis1~treatments, 
                   method = perm, data = RBC_NBC, permutations = 999))
# NSC ~ NBC
NSC_NBC <- meta_str %>% subset(treatments=="NSC"|treatments=="NBC")
NSC_NBC$treatments <- factor(NSC_NBC$treatments)
str(NSC_NBC)
diat_salt <- diat5[c(16,18,20,21,30,33,38,42),]
spp.log2 <- decostand(diat_salt, method = "log")
spp.log.dis2 <- vegdist(spp.log2, method = "bray")
(perm.2 <- adonis2(spp.log.dis2~treatments, 
                   method = perm, data = NSC_NBC, permutations = 999))
# NBI ~ NBC
NBI_NBC <- meta_str %>% subset(treatments=="NBI"|treatments=="NBC")
NBI_NBC$treatments <- factor(NBI_NBC$treatments)
str(NBI_NBC)
diat_temp <- diat5[c(16,18,30,38,56,57,59,60),]
spp.log3 <- decostand(diat_temp, method = "log")
spp.log.dis3 <- vegdist(spp.log3, method = "bray")
(perm.3 <- adonis2(spp.log.dis3~treatments, 
                   method = perm, data = NBI_NBC, permutations = 999))
# RSC~NBC
RSC_NBC <- meta_str %>% subset(treatments=="RSC"|treatments=="NBC")
RSC_NBC$treatments <- factor(RSC_NBC$treatments)
str(RSC_NBC)
diat_RSC <- diat5[c(12,16,18,19,30,35,38,43),]
spp.log4 <- decostand(diat_RSC, method = "log")
spp.log.dis4 <- vegdist(spp.log4, method = "bray")
(perm.4 <- adonis2(spp.log.dis4~treatments, 
                   method = perm, data = RSC_NBC, permutations = 999))
# RBI~NBC
RBI_NBC <- meta_str %>% subset(treatments=="RBI"|treatments=="NBC")
RBI_NBC$treatments <- factor(RBI_NBC$treatments)
str(RBI_NBC)
diat_RBI <- diat5[c(6,16,18,30,38,51,54,62),]
spp.log5 <- decostand(diat_RBI, method = "log")
spp.log.dis5 <- vegdist(spp.log5, method = "bray")
(perm.5 <- adonis2(spp.log.dis5~treatments, 
                   method = perm, data = RBI_NBC, permutations = 999))
# NSI~NBC
NSI_NBC <- meta_str %>% subset(treatments=="NSI"|treatments=="NBC")
NSI_NBC$treatments <- factor(NSI_NBC$treatments)
str(NSI_NBC)
diat_NSI <- diat5[c(16,18,30,38,44,46,63,64),]
spp.log6 <- decostand(diat_NSI, method = "log")
spp.log.dis6 <- vegdist(spp.log6, method = "bray")
(perm.6 <- adonis2(spp.log.dis6~treatments, 
                   method = perm, data = NSI_NBC, permutations = 999))
# RSI~NBC
RSI_NBC <- meta_str %>% subset(treatments=="RSI"|treatments=="NBC")
RSI_NBC$treatments <- factor(RSI_NBC$treatments)
str(RSI_NBC)
diat_RSI <- diat5[c(1,8,16,18,30,38,45,61),]
spp.log7 <- decostand(diat_RSI, method = "log")
spp.log.dis7 <- vegdist(spp.log7, method = "bray")
(perm.7 <- adonis2(spp.log.dis7~treatments, 
                   method = perm, data = RSI_NBC, permutations = 999))
# what makes this difference in RSI treatment compared to the control?
library(indicspecies)

set.seed(123)

diat.ISA <- multipatt(x = diat_RSI,
                      cluster = RSI_NBC$treatments,
                      duleg = TRUE)
summary(diat.ISA)

# during recovery phase
meta_rec <- meta1 %>% subset(Phase=="recovery")

# select the 8 channels in the community dataset
# RBC ~ NBC
# select treatments for comparison of the communities
RBC_NBC <- meta_rec %>% subset(treatments=="RBC"|treatments=="NBC")
RBC_NBC$treatments <- factor(RBC_NBC$treatments)
diat_veloc <- diat5[c(13,14,17,24,29,32,37,40),]
spp.log1 <- decostand(diat_veloc, method = "log")
spp.log.dis1 <- vegdist(spp.log1, method = "bray")
(perm.1 <- adonis2(spp.log.dis1~treatments, 
                   method = perm, data = RBC_NBC, permutations = 999))
# NSC ~ NBC
NSC_NBC <- meta_rec %>% subset(treatments=="NSC"|treatments=="NBC")
NSC_NBC$treatments <- factor(NSC_NBC$treatments)
str(NSC_NBC)
diat_salt <- diat5[c(13,14,22,26,31,32,37,39),]
spp.log2 <- decostand(diat_salt, method = "log")
spp.log.dis2 <- vegdist(spp.log2, method = "bray")
(perm.2 <- adonis2(spp.log.dis2~treatments, 
                   method = perm, data = NSC_NBC, permutations = 999))
# NBI ~ NBC
NBI_NBC <- meta_rec %>% subset(treatments=="NBI"|treatments=="NBC")
NBI_NBC$treatments <- factor(NBI_NBC$treatments)
str(NBI_NBC)
diat_temp <- diat5[c(5,7,13,14,32,37,47,49),]
spp.log3 <- decostand(diat_temp, method = "log")
spp.log.dis3 <- vegdist(spp.log3, method = "bray")
(perm.3 <- adonis2(spp.log.dis3~treatments, 
                   method = perm, data = NBI_NBC, permutations = 999))
# RSC~NBC
RSC_NBC <- meta_rec %>% subset(treatments=="RSC"|treatments=="NBC")
RSC_NBC$treatments <- factor(RSC_NBC$treatments)
str(RSC_NBC)
diat_RSC <- diat5[c(13,14,15,27,32,36,37,41),]
spp.log4 <- decostand(diat_RSC, method = "log")
spp.log.dis4 <- vegdist(spp.log4, method = "bray")
(perm.4 <- adonis2(spp.log.dis4~treatments, 
                   method = perm, data = RSC_NBC, permutations = 999))
# RBI~NBC
RBI_NBC <- meta_rec %>% subset(treatments=="RBI"|treatments=="NBC")
RBI_NBC$treatments <- factor(RBI_NBC$treatments)
str(RBI_NBC)
diat_RBI <- diat5[c(2,9,13,14,32,37,50,52),]
spp.log5 <- decostand(diat_RBI, method = "log")
spp.log.dis5 <- vegdist(spp.log5, method = "bray")
(perm.5 <- adonis2(spp.log.dis5~treatments, 
                   method = perm, data = RBI_NBC, permutations = 999))
# NSI~NBC
NSI_NBC <- meta_rec %>% subset(treatments=="NSI"|treatments=="NBC")
NSI_NBC$treatments <- factor(NSI_NBC$treatments)
str(NSI_NBC)
diat_NSI <- diat5[c(10,11,13,14,32,37,48,55),]
spp.log6 <- decostand(diat_NSI, method = "log")
spp.log.dis6 <- vegdist(spp.log6, method = "bray")
(perm.6 <- adonis2(spp.log.dis6~treatments, 
                   method = perm, data = NSI_NBC, permutations = 999))
# RSI~NBC
RSI_NBC <- meta_rec %>% subset(treatments=="RSI"|treatments=="NBC")
RSI_NBC$treatments <- factor(RSI_NBC$treatments)
str(RSI_NBC)
diat_RSI <- diat5[c(3,4,13,14,32,37,53,58),]
spp.log7 <- decostand(diat_RSI, method = "log")
spp.log.dis7 <- vegdist(spp.log7, method = "bray")
(perm.7 <- adonis2(spp.log.dis7~treatments, 
                   method = perm, data = RSI_NBC, permutations = 999))

##################################################
# Nothing turns out to have significant effect in driving 
# diatom community structure in exstream 2022
##################################################
# NMDS
set.seed(123)
spp.nmds <- metaMDS(spp.log.dis, k = 3,trymax = 100, permutation = 999,
                    distance = "bray", wascores = TRUE)

spp.nmds
stressplot(spp.nmds)

##########################################
#############################
#spp.mds <- metaMDS((spe_count), distance = "bray", autotransform = F)
#spp.mds
env.fit <- envfit(spp.nmds, meta1, permutations = 999, na.rm = TRUE) # this fits environmental vectors
spp.fit <- envfit(spp.nmds, diat5, permutations = 999) # this fits species vectors

env.fit
spp.fit
# To plot the output from the mds using ggplot a new datasheet needs to be created which contains the x,y points for each site. 
#You can do this by calling the scores of you mds.

sample.scrs <- as.data.frame(scores(spp.nmds, display = "sites")) #save NMDS results into dataframe
head(sample.scrs)
# make rownames as first column

sample.scrs <- sample.scrs %>% rownames_to_column(var="Channel") # change row names
#view(sample.scrs)
sample.scrs <- left_join(sample.scrs, metadata, by = "Channel")
sample.scrs
# Reorder the factor levels
sample.scrs$treatments <- factor(sample.scrs$treatments, 
<<<<<<< HEAD
                                 levels = c("NBC","RBC","NSC","NBI",
                                            "RSC","RBI","NSI","RSI"))
=======
                               levels = c("NBC","RBC","NSC","NBI",
                                          "RSC","RBI","NSI","RSI"))
>>>>>>> 5997fedc77a4a453d580e63d133156747a2321ea


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
env.scores
env.scores <- cbind(env.scores, pval = env.fit$vectors$pvals) # add pvalues to dataframe
sig.env.scrs <- subset(env.scores, pval<=0.05) #subset data to show variables significant at 0.05

head(env.scores)
sig.env.scrs
env.scores
#Now we have the relevant information for plotting the ordination in ggplot! Lets get plotting!
#######################################
# add the hull
NBC <- sample.scrs[sample.scrs$treatments == "NBC", ][chull(sample.scrs[sample.scrs$treatments == 
<<<<<<< HEAD
                                                                          "NBC", c("NMDS1", "NMDS2")]), ]  # hull values for control channels
RBC <- sample.scrs[sample.scrs$treatments == "RBC", ][chull(sample.scrs[sample.scrs$treatments == 
                                                                          "RBC", c("NMDS1", "NMDS2")]), ]  # hull values for reduced flow treatment
NSC <- sample.scrs[sample.scrs$treatments == "NSC", ][chull(sample.scrs[sample.scrs$treatments == 
                                                                          "NSC", c("NMDS1", "NMDS2")]), ]  # hull values increased salt channels
NBI <- sample.scrs[sample.scrs$treatments == "NBI", ][chull(sample.scrs[sample.scrs$treatments == 
                                                                          "NBI", c("NMDS1", "NMDS2")]), ]  # hull values for increased temp. treatment
=======
                                                                                           "NBC", c("NMDS1", "NMDS2")]), ]  # hull values for control channels
RBC <- sample.scrs[sample.scrs$treatments == "RBC", ][chull(sample.scrs[sample.scrs$treatments == 
                                                                                           "RBC", c("NMDS1", "NMDS2")]), ]  # hull values for reduced flow treatment
NSC <- sample.scrs[sample.scrs$treatments == "NSC", ][chull(sample.scrs[sample.scrs$treatments == 
                                                                                           "NSC", c("NMDS1", "NMDS2")]), ]  # hull values increased salt channels
NBI <- sample.scrs[sample.scrs$treatments == "NBI", ][chull(sample.scrs[sample.scrs$treatments == 
                                                                                           "NBI", c("NMDS1", "NMDS2")]), ]  # hull values for increased temp. treatment
>>>>>>> 5997fedc77a4a453d580e63d133156747a2321ea
RSC <- sample.scrs[sample.scrs$treatments == "RSC", ][chull(sample.scrs[sample.scrs$treatments == 
                                                                          "RSC", c("NMDS1", "NMDS2")]), ]  # hull values for reduced flow and increased salt channels
RBI <- sample.scrs[sample.scrs$treatments == "RBI", ][chull(sample.scrs[sample.scrs$treatments == 
                                                                          "RBI", c("NMDS1", "NMDS2")]), ]  # hull values for reduced flow and increased temp. treatment
NSI <- sample.scrs[sample.scrs$treatments == "NSI", ][chull(sample.scrs[sample.scrs$treatments == 
                                                                          "NSI", c("NMDS1", "NMDS2")]), ]  # hull values for increased salt and temperature treatment
RSI <- sample.scrs[sample.scrs$treatments == "RSI", ][chull(sample.scrs[sample.scrs$treatments == 
                                                                          "RSI", c("NMDS1", "NMDS2")]), ]  # hull values for reduced flow & increased salt and temperature treatment
<<<<<<< HEAD





=======





>>>>>>> 5997fedc77a4a453d580e63d133156747a2321ea
hull.data <- rbind(NBC, RBC, NSC, NBI, RSC, RBI, NSI, RSI)  #combine sampling treatment groups in the same dataset
hull.data

#########################################
# Basic ordination plot (restoration_date and season)
nmds.plot.diatoms <- ggplot(sample.scrs, aes(x=NMDS1, y=NMDS2), size = 7)+ #sets up the plot
  geom_point(aes(NMDS1, NMDS2, colour = factor(treatments), shape = factor(Phase)), size = 6)+ #adds site points to plot, shape determined season, colour determined by restorartion_date
  #geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,fill=treatments,group=treatments),alpha=0.30) +
  #stat_ellipse(aes(fill=treatments), alpha=.2,type='t',size =1, geom="polygon")+ ## add ellipses
  #geom_text(data=sample.scrs,aes(x=NMDS1,y=NMDS2,label=Channel), vjust=0, color="blue") +  # add the site labels
  coord_fixed()+
  theme_classic()+ 
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
  labs(colour = "treatments", shape = "Phase")+ # add legend labels for Management and Landuse
  theme(legend.position = "none", legend.text = element_text(size = 14), legend.title = element_text(size = 14), axis.text = element_text(size = 16)) + # add legend at right of plot
  annotate(geom = "label", x = -0.5, y = -1.0, size = 5,
           label = paste("Stress: ", round(spp.nmds$stress, digits = 3)))# add stress value

nmds.plot.diatoms + labs(title = "") # displays plot

# Significant species
nmds1 <- nmds.plot.diatoms +
  geom_segment(data = sig.spp.scrs, aes(x = 0, xend=NMDS1, y=0, yend=NMDS2), arrow = arrow(length = unit(0.25, "cm")), colour = "grey10", lwd=0.3) + #add vector arrows of significant species
  ggrepel::geom_text_repel(data = sig.spp.scrs, aes(x=NMDS1, y=NMDS2, label = Species), max.overlaps = Inf, cex = 3, direction = "both", segment.size = 0.25)+ #add labels for species, use ggrepel::geom_text_repel so that labels do not overlap
  labs(title = "")

print(nmds1)

# Significant abiotic variables
nmds.plot.diatoms +
  geom_segment(data = sig.env.scrs, aes(x = 0, xend=NMDS1, y = 0, yend=NMDS2), arrow = arrow(length = unit(0.25, "cm")), colour = "grey10", lwd=0.3) + #add vector arrows of significant env variables
  ggrepel::geom_text_repel(data = sig.env.scrs, aes(x=NMDS1, y=NMDS2, label = env.variables), max.overlaps = Inf, cex = 4, direction = "both", segment.size = 0.25)+ #add labels for env variables
  labs(title="")

# add both significant taxa and abiotic factors
exstream2022_nmdsDM1 <- nmds1 +
  geom_segment(data = sig.env.scrs, aes(x = 0, xend=NMDS1, y = 0, yend=NMDS2), arrow = arrow(length = unit(0.25, "cm")), colour = "red", lwd=0.3) + #add vector arrows of significant env variables
  ggrepel::geom_text_repel(data = sig.env.scrs, aes(x=NMDS1, y=NMDS2, label = env.variables), cex = 4, direction = "both", colour = "red", segment.size = 0.25)+ #add labels for env variables
  labs(title="A")

exstream2022_nmdsDM1

# save the plot
ggsave("exstream2022_nmdsDM1.tiff", units="in", width=7, height=7, dpi = 300, compression = 'lzw')

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
# select only stressor phase samples
div_stress <- div2 %>%
  filter(Phase == "stress")
range(div_stress$Richness)
range(div_stress$Shannon)
mean(div_stress$Richness)
mean(div_stress$Shannon)
# Select only phase samples 
div_rec <- div2 %>%
  filter(Phase == "recovery")
range(div_rec$Richness)
range(div_rec$Shannon)
mean(div_rec$Richness)
mean(div_rec$Shannon)

# remove column 3 in div2
#div2 <- div2[,-3]


# run multiple comparisons for analysis of variance (t-test or ANOVA)
# Multiple comparisons
#update.packages(ask = FALSE)
library(tidyverse)
#install.packages("ggstatsplot")
library(ggstatsplot)
# Comparison between species

# edit from here
x <- "treatments"
cols <- 2:11 # the 4 continuous dependent variables
type <- "parametric" # given the large number of observations, we use the parametric version
paired <- FALSE # FALSE for independent samples, TRUE for paired samples

#stressor phase
# edit until here

# edit at your own risk
plotlist <-
  purrr::pmap(
    .l = list(
      data = list(as_tibble(div_stress)),
      x = x,
      y = as.list(colnames(div_stress)[cols]),
      plot.type = "box", # for boxplot
      type = type, # parametric or nonparametric
      pairwise.comparisons = TRUE, # to run post-hoc tests if more than 2 groups
      pairwise.display = "significant", # show only significant differences
      bf.message = TRUE, # remove message about Bayes Factor
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


# recovery phase
# edit until here
<<<<<<< HEAD

# edit at your own risk
plotlist <-
  purrr::pmap(
    .l = list(
      data = list(as_tibble(div_rec)),
      x = x,
      y = as.list(colnames(div_rec)[cols]),
      plot.type = "box", # for boxplot
      type = type, # parametric or nonparametric
      pairwise.comparisons = TRUE, # to run post-hoc tests if more than 2 groups
      pairwise.display = "significant", # show only significant differences
      bf.message = TRUE, # remove message about Bayes Factor
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
## indicator species analysis (ISA) with microscopy data
library(indicspecies)

set.seed(123)

=======

# edit at your own risk
plotlist <-
  purrr::pmap(
    .l = list(
      data = list(as_tibble(div_rec)),
      x = x,
      y = as.list(colnames(div_rec)[cols]),
      plot.type = "box", # for boxplot
      type = type, # parametric or nonparametric
      pairwise.comparisons = TRUE, # to run post-hoc tests if more than 2 groups
      pairwise.display = "significant", # show only significant differences
      bf.message = TRUE, # remove message about Bayes Factor
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
## indicator species analysis (ISA) with microscopy data
library(indicspecies)

set.seed(123)

>>>>>>> 5997fedc77a4a453d580e63d133156747a2321ea
diat.ISA <- multipatt(x = diat5,
                      cluster = meta1$treatments,
                      duleg = TRUE)
summary(diat.ISA)
##############################################################
##############################################################
# Sequencing data (18S-V9)
# @Serge: I simplified the below part a bit
# I hope I got the important steps!
library(tidyverse)
library(vegan)
dna <- read.table("output_exstream2022_18S_clean.csv", sep=",", 
                  header=T, row.names = 1)
#######
protists <- read.table("full_table.csv", sep=",", 
                       header=T, row.names = 1)
totalreads_counts <- rowSums(protists[,2:217])
NC_counts <- rowSums(protists[,194:217])

view(totalreads_counts)
view(NC_counts)
sum(totalreads_counts)
sum(NC_counts)
####

# NC counts in % of the total
# (NC_counts/total_counts)*100
(785609/34063298)*100


view(dna[grep("Metazoa", dna$Division),])

view(dna[grep("lata$", dna$Division),])
# keep only algae
algae <- view(dna[grep("phyta$|lata$", dna$Division),])
#write.csv(algae, "exstream2022_algae.csv")

# recovery
recovery <- algae[, grep("_REC$", names(algae))]

##########
## Select only scraped area samples 
# Find column indices to discard
columns_to_discard <- which(grepl("^IMP", names(algae)) & !grepl("_REC$", names(algae)))
view(columns_to_discard)
# Select the complement of the identified columns
scraped_area <- algae[, -columns_to_discard]

# View the selected columns
view(scraped_area)

#view(algae[grep("plas", algae$Division),])
#view(dna[grep("Metazoa", algae$Division),])
diat_18S <- view(algae[grep("Bacillariophyta", algae$Class),])
diat_18S_scraped <- view(scraped_area[grep("Bacillariophyta", scraped_area$Class),])
total_diat_scraped <- rowSums(diat_18S_scraped[,1:32])
sum(total_diat_scraped)

# readcounts dataframe

set.seed(123)
readcounts <- algae[,1:96]
scraped_count <- scraped_area[, 1:32]
taxonomy <- algae[,98:105]
# remove recovery samples
# Selecting columns with names ending in "_rec"
readcounts1 <- readcounts[, -grep("_REC$", names(readcounts))]
view(readcounts1)
dim(readcounts1)
algae1 <- algae[, -grep("_REC$", names(algae))]
dim(algae1)
view(algae1)
diat_18S <- view(algae[grep("Bacillariophyta", algae$Class),])
total_diatom_readscount <- rowSums(diat_18S[,1:64])
total_diatom_readscount
sum(total_diatom_readscount)

total_reads_scraped <- rowSums(recovery[, 1:32])
sum(total_reads_scraped)
diat_18S_scraped <- view(recovery[grep("Bacillariophyta", recovery$Class),])
(6245423/6568787)*100

(11696476/13034618)*100
(11750187/19603405)*100
(13034618/19603405)*100
totalreads_counts <- rowSums(algae[,1:96])
#NC_counts <- rowSums(protists[,194:217])

view(totalreads_counts)
#view(NC_counts)
sum(totalreads_counts)
sum(NC_counts)

# transpose dataset

dna_spe_trans <- t(readcounts1)
scraped_count_trans <- t(scraped_count)
total_reads_count <- rowSums(dna_spe_trans)
view(total_reads_count)
sum(total_reads_count)
range(total_reads_count)
mean(total_reads_count)
19603405-13034618

sapply(dna_spe_trans, class)

#taxa
spec <- algae1[,66:73]
spec_trans <- t(spec)
sapply(spec_trans, class)

spp.log_alg <- decostand(dna_spe_trans, method = "log")
spp.hell.alg <- decostand(dna_spe_trans, method = "hellinger")
spp.log.dis_alg <- vegdist(spp.log_alg, method = "bray")
spp.hell.alg.dis <- vegdist(spp.hell.alg, method = "bray")

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

# combination of treatments
# flow velocity
(mod.treatments <- with(meta1, betadisper(spp.log.dis_alg, meta1$treatments)))
plot(mod.treatments)
boxplot(mod.treatments)
anova(mod.treatments)
permutest(mod.treatments)

# flow velocity
(mod.phase <- with(meta1, betadisper(spp.log.dis_alg, meta1$Phase)))
plot(mod.phase)
boxplot(mod.phase)
anova(mod.phase)
permutest(mod.phase)
<<<<<<< HEAD


=======


>>>>>>> 5997fedc77a4a453d580e63d133156747a2321ea
#######
# Permutational multivariate analysis of variance using distance matrices
# (Bray-Curtis similarities by default). ANOSIM uses only ranks of Bray-Curtis,
# so the former preserves more information.

(perm.2 <- adonis2(spp.log.dis_alg~(Temperature*Salinity*Velocity), 
<<<<<<< HEAD
                   method = perm, data = meta1, permutations = 999))

(perm.2 <- adonis2(spp.hell.alg.dis~(Temperature*Salinity*Velocity*Phase), 
                   method = perm, data = meta1, permutations = 999))
# Round the values to 2 decimal places
result_perm2 <- lapply(perm.2, function(x) round(x, 2))

print(result_perm2)

(perm.2 <- adonis2(spp.log.dis_alg~treatments*Phase, 
                   method = perm, data = meta1, permutations = 999))

=======
                   method = perm, data = meta1, permutations = 999))
# Round the values to 2 decimal places
result_perm2 <- lapply(perm.2, function(x) round(x, 2))

print(result_perm2)

(perm.2 <- adonis2(spp.log.dis_alg~treatments*Phase, 
                   method = perm, data = meta1, permutations = 999))

>>>>>>> 5997fedc77a4a453d580e63d133156747a2321ea
(perm.2 <- adonis2(spp.log.dis_alg~(Temperature),
                   method = perm, data = meta1))
# Nothing turns out to have significant effect in driving 
# diatom community structure in exstream 2022
###################################################
# Pairwise comparison of each treatment 
# against control for 18S-V9 data
# rownames to column as rownames
dna_data <- as.data.frame(dna_spe_trans) %>% rownames_to_column(var="Sample") # change row names
# Use separate to split the full_column into two columns based on "_"
dna_data1 <- separate(dna_data, Sample, into = c("System", "Sample"), sep = "_")
dna_data2 <- dna_data1[,-1]
# first column as rownames
dna_data3 <- dna_data2 %>% column_to_rownames(var="Sample") # change row names

# during stressor phase

meta_str <- meta1 %>% subset(Phase=="stress")

# select the 8 channels in the community dataset
# RBC ~ NBC
# select treatments for comparison of the communities
RBC_NBC <- meta_str %>% subset(treatments=="RBC"|treatments=="NBC")
RBC_NBC$treatments <- factor(RBC_NBC$treatments)
dna_RBC <- dna_data3[c(21,23,28,30,33,35,39,43),]
<<<<<<< HEAD
spp.log1 <- decostand(dna_RBC, method = "log")
=======
spp.log1 <- decostand(diat_veloc, method = "log")
>>>>>>> 5997fedc77a4a453d580e63d133156747a2321ea
spp.log.dis1 <- vegdist(spp.log1, method = "bray")
(perm.1 <- adonis2(spp.log.dis1~treatments, 
                   method = perm, data = RBC_NBC, permutations = 999))
# NSC ~ NBC
NSC_NBC <- meta_str %>% subset(treatments=="NSC"|treatments=="NBC")
NSC_NBC$treatments <- factor(NSC_NBC$treatments)
str(NSC_NBC)
<<<<<<< HEAD
dna_NSC <- dna_data3[c(21,23,25,26,35,38,43,47),]
spp.log2 <- decostand(dna_NSC, method = "log")
=======
diat_salt <- diat5[c(16,18,20,21,30,33,38,42),]
spp.log2 <- decostand(diat_salt, method = "log")
>>>>>>> 5997fedc77a4a453d580e63d133156747a2321ea
spp.log.dis2 <- vegdist(spp.log2, method = "bray")
(perm.2 <- adonis2(spp.log.dis2~treatments, 
                   method = perm, data = NSC_NBC, permutations = 999))
# NBI ~ NBC
NBI_NBC <- meta_str %>% subset(treatments=="NBI"|treatments=="NBC")
NBI_NBC$treatments <- factor(NBI_NBC$treatments)
str(NBI_NBC)
<<<<<<< HEAD
dna_NBI <- dna_data3[c(5,7,21,23,35,43,62,63),]
spp.log3 <- decostand(dna_NBI, method = "log")
=======
diat_temp <- diat5[c(16,18,30,38,56,57,59,60),]
spp.log3 <- decostand(diat_temp, method = "log")
>>>>>>> 5997fedc77a4a453d580e63d133156747a2321ea
spp.log.dis3 <- vegdist(spp.log3, method = "bray")
(perm.3 <- adonis2(spp.log.dis3~treatments, 
                   method = perm, data = NBI_NBC, permutations = 999))
# RSC~NBC
RSC_NBC <- meta_str %>% subset(treatments=="RSC"|treatments=="NBC")
RSC_NBC$treatments <- factor(RSC_NBC$treatments)
str(RSC_NBC)
<<<<<<< HEAD
dna_RSC <- dna_data3[c(17,21,23,24,35,40,43,48),]
spp.log4 <- decostand(dna_RSC, method = "log")
=======
diat_RSC <- diat5[c(12,16,18,19,30,35,38,43),]
spp.log4 <- decostand(diat_RSC, method = "log")
>>>>>>> 5997fedc77a4a453d580e63d133156747a2321ea
spp.log.dis4 <- vegdist(spp.log4, method = "bray")
(perm.4 <- adonis2(spp.log.dis4~treatments, 
                   method = perm, data = RSC_NBC, permutations = 999))
# RBI~NBC
RBI_NBC <- meta_str %>% subset(treatments=="RBI"|treatments=="NBC")
RBI_NBC$treatments <- factor(RBI_NBC$treatments)
str(RBI_NBC)
<<<<<<< HEAD
dna_RBI <- dna_data3[c(6,10,21,23,35,43,56,59),]
spp.log5 <- decostand(dna_RBI, method = "log")
=======
diat_RBI <- diat5[c(6,16,18,30,38,51,54,62),]
spp.log5 <- decostand(diat_RBI, method = "log")
>>>>>>> 5997fedc77a4a453d580e63d133156747a2321ea
spp.log.dis5 <- vegdist(spp.log5, method = "bray")
(perm.5 <- adonis2(spp.log.dis5~treatments, 
                   method = perm, data = RBI_NBC, permutations = 999))
# NSI~NBC
NSI_NBC <- meta_str %>% subset(treatments=="NSI"|treatments=="NBC")
NSI_NBC$treatments <- factor(NSI_NBC$treatments)
str(NSI_NBC)
<<<<<<< HEAD
dna_NSI <- dna_data3[c(13,15,21,23,35,43,49,51),]
spp.log6 <- decostand(dna_NSI, method = "log")
=======
diat_NSI <- diat5[c(16,18,30,38,44,46,63,64),]
spp.log6 <- decostand(diat_NSI, method = "log")
>>>>>>> 5997fedc77a4a453d580e63d133156747a2321ea
spp.log.dis6 <- vegdist(spp.log6, method = "bray")
(perm.6 <- adonis2(spp.log.dis6~treatments, 
                   method = perm, data = NSI_NBC, permutations = 999))
# RSI~NBC
RSI_NBC <- meta_str %>% subset(treatments=="RSI"|treatments=="NBC")
RSI_NBC$treatments <- factor(RSI_NBC$treatments)
str(RSI_NBC)
<<<<<<< HEAD
dna_RSI <- dna_data3[c(8,11,21,23,35,43,50,61),]
spp.log7 <- decostand(dna_RSI, method = "log")
=======
diat_RSI <- diat5[c(1,8,16,18,30,38,45,61),]
spp.log7 <- decostand(diat_RSI, method = "log")
>>>>>>> 5997fedc77a4a453d580e63d133156747a2321ea
spp.log.dis7 <- vegdist(spp.log7, method = "bray")
(perm.7 <- adonis2(spp.log.dis7~treatments, 
                   method = perm, data = RSI_NBC, permutations = 999))
# what makes this difference in RSI treatment compared to the control?
library(indicspecies)

set.seed(123)

diat.ISA <- multipatt(x = diat_RSI,
                      cluster = RSI_NBC$treatments,
                      duleg = TRUE)
summary(diat.ISA)

# during recovery phase
meta_rec <- meta1 %>% subset(Phase=="recovery")

# select the 8 channels in the community dataset
# RBC ~ NBC
# select treatments for comparison of the communities
RBC_NBC <- meta_rec %>% subset(treatments=="RBC"|treatments=="NBC")
RBC_NBC$treatments <- factor(RBC_NBC$treatments)
<<<<<<< HEAD
str(RBC_NBC)
dna_RBC <- dna_data3[c(18,19,22,29,34,37,42,45),]
spp.log1 <- decostand(dna_RBC, method = "log")
=======
diat_veloc <- diat5[c(13,14,17,24,29,32,37,40),]
spp.log1 <- decostand(diat_veloc, method = "log")
>>>>>>> 5997fedc77a4a453d580e63d133156747a2321ea
spp.log.dis1 <- vegdist(spp.log1, method = "bray")
(perm.1 <- adonis2(spp.log.dis1~treatments, 
                   method = perm, data = RBC_NBC, permutations = 999))
# NSC ~ NBC
NSC_NBC <- meta_rec %>% subset(treatments=="NSC"|treatments=="NBC")
NSC_NBC$treatments <- factor(NSC_NBC$treatments)
str(NSC_NBC)
<<<<<<< HEAD
dna_NSC <- dna_data3[c(18,19,27,31,36,37,42,44),]
spp.log2 <- decostand(dna_NSC, method = "log")
=======
diat_salt <- diat5[c(13,14,22,26,31,32,37,39),]
spp.log2 <- decostand(diat_salt, method = "log")
>>>>>>> 5997fedc77a4a453d580e63d133156747a2321ea
spp.log.dis2 <- vegdist(spp.log2, method = "bray")
(perm.2 <- adonis2(spp.log.dis2~treatments, 
                   method = perm, data = NSC_NBC, permutations = 999))
# NBI ~ NBC
NBI_NBC <- meta_rec %>% subset(treatments=="NBI"|treatments=="NBC")
NBI_NBC$treatments <- factor(NBI_NBC$treatments)
str(NBI_NBC)
<<<<<<< HEAD
dna_NBI <- dna_data3[c(4,9,18,19,37,42,52,54),]
spp.log3 <- decostand(dna_NBI, method = "log")
=======
diat_temp <- diat5[c(5,7,13,14,32,37,47,49),]
spp.log3 <- decostand(diat_temp, method = "log")
>>>>>>> 5997fedc77a4a453d580e63d133156747a2321ea
spp.log.dis3 <- vegdist(spp.log3, method = "bray")
(perm.3 <- adonis2(spp.log.dis3~treatments, 
                   method = perm, data = NBI_NBC, permutations = 999))
# RSC~NBC
RSC_NBC <- meta_rec %>% subset(treatments=="RSC"|treatments=="NBC")
RSC_NBC$treatments <- factor(RSC_NBC$treatments)
str(RSC_NBC)
<<<<<<< HEAD
dna_RSC <- dna_data3[c(18,19,20,32,37,41,42,46),]
spp.log4 <- decostand(dna_RSC, method = "log")
=======
diat_RSC <- diat5[c(13,14,15,27,32,36,37,41),]
spp.log4 <- decostand(diat_RSC, method = "log")
>>>>>>> 5997fedc77a4a453d580e63d133156747a2321ea
spp.log.dis4 <- vegdist(spp.log4, method = "bray")
(perm.4 <- adonis2(spp.log.dis4~treatments, 
                   method = perm, data = RSC_NBC, permutations = 999))
# RBI~NBC
RBI_NBC <- meta_rec %>% subset(treatments=="RBI"|treatments=="NBC")
RBI_NBC$treatments <- factor(RBI_NBC$treatments)
str(RBI_NBC)
<<<<<<< HEAD
dna_RBI <- dna_data3[c(1,12,18,19,37,42,55,57),]
spp.log5 <- decostand(dna_RBI, method = "log")
=======
diat_RBI <- diat5[c(2,9,13,14,32,37,50,52),]
spp.log5 <- decostand(diat_RBI, method = "log")
>>>>>>> 5997fedc77a4a453d580e63d133156747a2321ea
spp.log.dis5 <- vegdist(spp.log5, method = "bray")
(perm.5 <- adonis2(spp.log.dis5~treatments, 
                   method = perm, data = RBI_NBC, permutations = 999))
# NSI~NBC
NSI_NBC <- meta_rec %>% subset(treatments=="NSI"|treatments=="NBC")
NSI_NBC$treatments <- factor(NSI_NBC$treatments)
str(NSI_NBC)
<<<<<<< HEAD
dna_NSI <- dna_data3[c(14,16,18,19,37,42,53,60),]
spp.log6 <- decostand(dna_NSI, method = "log")
=======
diat_NSI <- diat5[c(10,11,13,14,32,37,48,55),]
spp.log6 <- decostand(diat_NSI, method = "log")
>>>>>>> 5997fedc77a4a453d580e63d133156747a2321ea
spp.log.dis6 <- vegdist(spp.log6, method = "bray")
(perm.6 <- adonis2(spp.log.dis6~treatments, 
                   method = perm, data = NSI_NBC, permutations = 999))
# RSI~NBC
RSI_NBC <- meta_rec %>% subset(treatments=="RSI"|treatments=="NBC")
RSI_NBC$treatments <- factor(RSI_NBC$treatments)
str(RSI_NBC)
<<<<<<< HEAD
dna_RSI <- dna_data3[c(2,3,18,19,37,42,58,64),]
spp.log7 <- decostand(dna_RSI, method = "log")
=======
diat_RSI <- diat5[c(3,4,13,14,32,37,53,58),]
spp.log7 <- decostand(diat_RSI, method = "log")
>>>>>>> 5997fedc77a4a453d580e63d133156747a2321ea
spp.log.dis7 <- vegdist(spp.log7, method = "bray")
(perm.7 <- adonis2(spp.log.dis7~treatments, 
                   method = perm, data = RSI_NBC, permutations = 999))

###########
# Filtering only diatoms (Bacillariophyta from the dna dataset
# Remove samples with 0 diatom abundances
set.seed(123)
alg_read_ab <- algae1[as.logical(rowSums(algae1[,1:64]!= 0)), ]
colSums(alg_read_ab[,1:64])
alg_18S <- alg_read_ab[,1:64]
alg_18S_t <- t(alg_18S)
x <- alg_18S_t
#diat<-diat/rowSums(diat)
x<-x/rowSums(x)
x<-x[,order(colSums(x),decreasing=TRUE)]
#  x1<-spe5/rowSums(spe5)*100
#diat<-diat[,order(colSums(diat),decreasing=TRUE)]
#  x1<-x1[,order(colSums(x1),decreasing=TRUE)]
colSums(x)
rowSums(x)
#colSums(x1)
#head(diat)
#head(x1)
#x <- t(x) #transpose
#Extract list of top N Taxa
N<-35
taxa_list_18S<-algae1[,73]
#taxa_list1<-colnames(x1)[1:N]
N<-length(taxa_list_18S)
#N1<-length(taxa_list1)

#Create a custom color scale
library(RColorBrewer)
# Define the number of colors you want
nb.cols <- 35
mycolors <- colorRampPalette(brewer.pal(12, "Paired"))(nb.cols)

# other colours
#library(RColorBrewer)
#install.packages("randomcoloR")
library(randomcoloR)
n <- 35
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
pie(rep(1,n), col=sample(col_vector, n))
palette <- distinctColorPalette(n)
#Colours <- brewer.pal(50,"Set")
#names(Colours) <- levels(diatom_ab$Taxa)
#colScale <- scale_colour_manual(name = "Taxa",values = Colours)

#Generate a new table with everything added to Others
#new_x<-data.frame(x[,colnames(x) %in% taxa_list],Others=rowSums(x[,!colnames(x) %in% taxa_list]))
#You can change the Type=grouping_info[,1] should you desire any other grouping of panels

df_18S<-NULL
for (i in 1:dim(x)[2]){
  tmp<-data.frame(row.names=NULL,Sample=rownames(meta1), Treatments=meta1$treatments, Salinity=meta1$Salinity,
                  Temperature=meta1$Temperature, Velocity=meta1$Velocity, Phase=meta1$Phase,
                  Taxa=rep(algae1$Species[i],dim(x)[1]),Value=x[,i])
  if(i==1){df_18S<-tmp} else {df_18S<-rbind(df_18S,tmp)}
}

df_Class<-NULL
for (i in 1:dim(x)[2]){
  tmp<-data.frame(row.names=NULL,Sample=rownames(meta1), Treatments=meta1$treatments, Salinity=meta1$Salinity,
                  Temperature=meta1$Temperature, Velocity=meta1$Velocity, Phase=meta1$Phase,
                  Class=rep(algae1$Class[i],dim(x)[1]),Value=x[,i])
  if(i==1){df_Class<-tmp} else {df_Class<-rbind(df_Class,tmp)}
}

# Replace all the taxa with lower than 1% occurrence in the sample with Others
df_18S_others <- df_18S %>% mutate(Taxa = replace(Taxa, Value <= 0.01, "Others (<1%)"))

# Based on site experiment phase: plot community composition in the channels
# subset stressor phase
#set.seed(123)
df_18S_stress <- subset(df_18S_others, Phase == "stress")

taxa18S_stress <- ggplot(df_18S_stress, aes(x=Sample,y=Value, fill=Taxa))+geom_bar(stat="identity")+
  facet_grid(. ~ Treatments, drop=TRUE,scale="free",space="free_x")+
  scale_fill_manual(values=palette[1:(N+1)]) + theme_bw()+ylab("Reads abundance") +
  scale_y_continuous(expand = c(0,0))+theme(strip.background = element_rect(fill="gray85"))+
  theme(panel.margin = unit(0.3, "lines")) +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=0.5, size = 5, face = "bold"), 
        strip.text = element_text(size = 10, face = "bold"))+
  theme(legend.key.size = unit(0.4, "cm"), legend.key.width = unit(0.4,"cm"), 
        legend.text = element_text(size = 8, face = "italic"))+
  guides(fill=guide_legend(ncol=1))

taxa18S_stress
ggsave("taxa18S_stress.tiff", units="in", width=7, height=7, dpi = 300, compression = 'lzw')

# subset recovery phase
df_18S_rec <- subset(df_18S_others, Phase == "recovery")

taxa18S_recovery <- ggplot(df_18S_rec, aes(x=Sample,y=Value, fill=Taxa))+geom_bar(stat="identity")+
  facet_grid(. ~ Treatments, drop=TRUE,scale="free",space="free_x")+
  scale_fill_manual(values=palette[1:(N+1)]) + theme_bw()+ylab("Reads abundance") +
  scale_y_continuous(expand = c(0,0))+theme(strip.background = element_rect(fill="gray85"))+
  theme(panel.margin = unit(0.3, "lines")) +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=0.5, size = 9, face = "bold"), 
        strip.text = element_text(size = 14, face = "bold"))+
  theme(legend.key.size = unit(0.7, "cm"), legend.key.width = unit(0.7,"cm"), 
        legend.text = element_text(size = 14, face = "italic"))+
  guides(fill=guide_legend(ncol=1))

taxa18S_recovery
ggsave("taxa18S_recovery.tiff", units="in", width=7, height=7, dpi = 300, compression = 'lzw')

# facet_warp treatments
exstream2022_dna_treat2 <- ggplot(df_18S_others, aes(x=Sample,y=Value, fill=Taxa))+geom_bar(stat="identity")+
  facet_grid(Phase ~ Treatments, drop=TRUE,scale="free",space="free_x")+
  scale_fill_manual(values=palette[1:(N+1)]) + theme_bw()+ylab("Relative read abundance") +
  scale_y_continuous(expand = c(0,0))+theme(strip.background = element_rect(fill="gray85"))+
  theme(panel.margin = unit(0.3, "lines")) +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=0.7, size = 7, face = "bold"), 
        strip.text = element_text(size = 9, face = "bold"))+
  theme(legend.position = "bottom", legend.key.size = unit(0.4, "cm"), legend.key.width = unit(0.4,"cm"), 
        legend.text = element_text(size = 7))+
  guides(fill=guide_legend(nrow = 4))

exstream2022_dna_treat2
# save the plot
ggsave("exstream2022_dna_treat2.tiff", units="in", width=17, height=7, dpi=300, compression = 'lzw')
#install.packages("ggpubr")
library(ggpubr)
combined_taxa_plot <- ggarrange(exstream2022_dm_treat2, exstream2022_dna_treat2, 
                                ncol=1, labels = c("A", "B"))
combined_taxa_plot

ggsave("combined_taxa_plot.tiff", units="in", width=14, height=14, dpi=300, compression = 'lzw')

# facet_warp treatments
exstream2022_dna_Class <- ggplot(df_Class, aes(x=Sample,y=Value, fill=Class))+geom_bar(stat="identity")+
  facet_grid(Phase ~ Treatments, drop=TRUE,scale="free",space="free_x")+
  scale_fill_manual(values=palette[1:(N+1)]) + theme_bw()+ylab("Relative abundance of OTU reads") +
  scale_y_continuous(expand = c(0,0))+theme(strip.background = element_rect(fill="gray85"))+
  theme(panel.margin = unit(0.3, "lines")) +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=0.5, size = 5, face = "bold"), 
        strip.text = element_text(size = 9, face = "bold"))+
  theme(legend.key.size = unit(0.4, "cm"), legend.key.width = unit(0.4,"cm"), 
        legend.text = element_text(size = 9))+
  guides(fill=guide_legend(ncol=1))

exstream2022_dna_Class
ggsave("exstream2022_dna_class.tiff", units="in", width=9, height=9, dpi=300, compression = 'lzw')
### a heatmap of 18S data
# Run the following two lines in order to install the package
#if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
#BiocManager::install("ComplexHeatmap")

# set things up for the panel of plots (Figure 3)
# pdf(file = "D:\Diatoms_Github\Diatom_counts../Figure__3.pdf", width = 8, height = 7)

# algae 18SV9 NMDS

spp.nmds_alg <- metaMDS(spp.log.dis_alg, k = 3,trymax = 1000, permutation = 999,
                        distance = "bray", wascores = TRUE)

#spp.nmds_dna_pa <- metaMDS(spp.pa_dna, k = 2,trymax = 1000, permutation = 9999,
<<<<<<< HEAD
#                          distance = "bray", wascores = TRUE)
=======
 #                          distance = "bray", wascores = TRUE)
>>>>>>> 5997fedc77a4a453d580e63d133156747a2321ea

#dev.off()
#dev.new()
spp.nmds_alg
stressplot(spp.nmds_alg)

##########################################
#############################
#spp.mds <- metaMDS((spe_count), distance = "bray", autotransform = F)
#spp.mds
env.fit_alg <- envfit(spp.nmds_alg, meta1, permutations = 999, na.rm = TRUE) # this fits environmental vectors
spp.fit_alg <- envfit(spp.nmds_alg, dna_spe_trans, permutations = 999) # this fits species vectors

env.fit_alg
spp.fit_alg
# To plot the output from the mds using ggplot a new datasheet needs to be created which contains the x,y points for each site. 
#You can do this by calling the scores of you mds.

sample.scrs_alg <- as.data.frame(scores(spp.nmds_alg, display = "sites")) #save NMDS results into dataframe
#sample.scrs_alg <- cbind(sample.scrs_alg, Treatments = meta1$treatments) #add grouping variable "Management" to dataframe
#sample.scrs <- cbind(sample.scrs, year.of.restor. = metadata1$year.of.r) #add grouping variable of cluster grouping to dataframe
#site.scrs <- cbind(site.scrs, Site = rownames(site.scrs)) #add site names as variable if you want to display on plot
#sample.scrs <- cbind(sample.scrs, location = env$site)
head(sample.scrs_alg)
#view(sample.scrs)
# make rownames as first column

sample.scrs_alg <- sample.scrs_alg %>% rownames_to_column(var="Channel") # change row names
view(sample.scrs_alg)
# separate the channel column by the "_" into sytsem and Channel
sample.scrs_alg <- separate(sample.scrs_alg, Channel, into = c("System", "Channel"), sep = "_")
sample.scrs_alg
sample.scrs_alg <- left_join(sample.scrs_alg, metadata, by = "Channel")
sample.scrs_alg
# Reorder the factor levels
sample.scrs_alg$treatments <- factor(sample.scrs_alg$treatments, 
<<<<<<< HEAD
                                     levels = c("NBC","RBC","NSC","NBI",
                                                "RSC","RBI","NSI","RSI"))
=======
                                 levels = c("NBC","RBC","NSC","NBI",
                                            "RSC","RBI","NSI","RSI"))
>>>>>>> 5997fedc77a4a453d580e63d133156747a2321ea
sample.scrs_alg
# A new dataset containing species data also needs to be made to look at species vectors.
#This is not necessary if you don't want to show the species on the final graph.
spp.scrs_alg <- as.data.frame(scores(spp.fit_alg, display = "vectors")) #save species intrinsic values into dataframe
spp.scrs_alg <- cbind(spp.scrs_alg, OTUs = rownames(spp.scrs_alg)) #add species names to dataframe
spp.scrs_alg <- cbind(spp.scrs_alg, pval = spp.fit_alg$vectors$pvals) #add pvalues to dataframe so you can select species which are significant
#spp.scrs<- cbind(spp.scrs, abrev = abbreviate(spp.scrs$Species, minlength = 6)) #abbreviate species names
sig.spp.scrs_alg <- subset(spp.scrs_alg, pval<=0.05) #subset data to show species significant at 0.05

head(spp.scrs_alg)
#view(spp.scrs)
sig.spp.scrs_alg
# To show biologically extrinsic variables another datasheet needs to be created

env.scores_alg <- as.data.frame(scores(env.fit_alg, display = "vectors")) #extracts relevant scores from envifit
env.scores_alg <- cbind(env.scores_alg, env.variables = rownames(env.scores_alg)) #and then gives them their names

env.scores_alg <- cbind(env.scores_alg, pval = env.fit_alg$vectors$pvals) # add pvalues to dataframe
sig.env.scrs_alg <- subset(env.scores_alg, pval<=0.05) #subset data to show variables significant at 0.05

head(env.scores_alg)
sig.env.scrs_alg


#########################################
# Basic ordination plot (restoration_date and season)
nmds.plot.alg <- ggplot(sample.scrs_alg, aes(x=NMDS1, y=NMDS2), size = 7)+ #sets up the plot
  geom_point(aes(NMDS1, NMDS2, colour = factor(treatments), shape = factor(Phase)), size = 6)+ #adds site points to plot, shape determined season, colour determined by restorartion_date
  #geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,fill=surrounding,group=surrounding),alpha=0.30) +
  #stat_ellipse(aes(fill=surrounding), alpha=.2,type='t',size =1, geom="polygon")+ ## add ellipses
  #geom_text(data=sample.scrs,aes(x=NMDS1,y=NMDS2,label=sample), vjust=0) +  # add the site labels
  coord_fixed()+
  theme_classic()+ 
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
  labs(colour = "Treatments", shape = "Phase")+ # add legend labels for Management and Landuse
  theme(legend.position = "right", legend.text = element_text(size = 14), legend.title = element_text(size = 14), axis.text = element_text(size = 16)) + # add legend at right of plot
  annotate(geom = "label", x = -0.2, y = -0.4, size = 5,
           label = paste("Stress: ", round(spp.nmds_alg$stress, digits = 3))) + # add stress value
  labs(title = "B")

nmds.plot.alg  # displays plot

ggsave("nmds.plot.alg.tiff", units="in", width=7, height=7, dpi = 300, compression = 'lzw')

library(cowplot)
nmds_exstream <- plot_grid(exstream2022_nmdsDM1, nmds.plot.alg, labels=c("A", "B"), 
                           ncol = 2)
nmds_exstream
ggsave("nmds_exstream.tiff", units="in", width=7, height=7, dpi=300, compression = 'lzw')

library(ggpubr)
library(ggpubr)
combined_nmds_plot <- ggarrange(exstream2022_nmdsDM1, nmds.plot.alg, 
                                ncol=1, labels = c("A", "B"))
combined_nmds_plot

ggsave("combined_nmds_plot.tiff", units="in", width=14, height=14, dpi=300, compression = 'lzw')
library(patchwork)
combined_nmds <- exstream2022_nmdsDM1 + nmds.plot.alg
combined_nmds
ggsave("combined_nmds.tiff", units="in", width=14, height=7, dpi=300, compression = 'lzw')

#############################
#install.packages("gridExtra")
library(gridExtra)
library(grid)

grid.arrange(exstream2022_nmdsDM1, nmds.plot.alg, ncol = 2)

grid.arrange(grobTree(textGrob("A", x = unit(0, "npc"), y = unit(1, "npc"), just = c("left", "top")),
                      grobTree(textGrob("B", x = unit(1, "npc"), y = unit(1, "npc"), just = c("left", "top"))),
                      exstream2022_nmdsDM1, nmds.plot.alg, ncol = 2))
library(patchwork)

arranged_plots <- (exstream2022_nmdsDM1/ nmds.plot.alg) & plot_layout(ncol = 2) & plot_spacer(
<<<<<<< HEAD
  #  width = unit(0.1, "npc"),  # Adjust the width of the spacer
  #  height = unit(0.1, "npc"), # Adjust the height of the spacer
  #  color = "red",            # Set the color of the spacer
  #  line_type = "dotted"      # Set the line type of the spacer
=======
#  width = unit(0.1, "npc"),  # Adjust the width of the spacer
#  height = unit(0.1, "npc"), # Adjust the height of the spacer
#  color = "red",            # Set the color of the spacer
#  line_type = "dotted"      # Set the line type of the spacer
>>>>>>> 5997fedc77a4a453d580e63d133156747a2321ea
)
arranged_plots

# Significant species
#tiff("EDM2023_nmdsyear", units="in", width=10, height=10, res=600)
nmds2 <- nmds.plot.alg +
  geom_segment(data = sig.spp.scrs_alg, aes(x = 0, xend=NMDS1, y=0, yend=NMDS2), arrow = arrow(length = unit(0.25, "cm")), colour = "grey10", lwd=0.3) + #add vector arrows of significant species
  ggrepel::geom_text_repel(data = sig.spp.scrs_alg, aes(x=NMDS1, y=NMDS2, label = OTUs), cex = 3, direction = "both", segment.size = 0.25)+ #add labels for species, use ggrepel::geom_text_repel so that labels do not overlap
  labs(title = "")

print(nmds2)
# Compute alpha diversity indices on 18SV9 data
# *******************************************************

# Get help on the diversity() function
#library(vegan)
?diversity

N0 <- rowSums(dna_spe_trans > 0)         # Species richness
N0a <- specnumber(dna_spe_trans)     # Species richness (alternate)
H <- diversity(dna_spe_trans)       # Shannon entropy (base e)
Hb2 <- diversity(dna_spe_trans, base = 2) # Shannon entropy (base 2)
N1 <- exp(H)                   # Shannon diversity (number of abundant species) (base e)
N1b2 <- 2^Hb2                 # Shannon diversity (base 2)
N2 <- diversity(dna_spe_trans, "inv")    # Simpson diversity (number of dominant species)
J <- H/log(N0)                 # Pielou evenness
E10 <- N1/N0                   # Shannon evenness (Hill's ratio)
E20 <- N2/N0                   # Simpson evenness (Hill's ratio)
(div <- data.frame(N0, N0a, H, Hb2, N1, N1b2, N2, E10, E20, J))
# make div dataset rownames as first column
div1 <- tibble::rownames_to_column(div, "Channel")
# make also metadata1 dataset rownames as first column
#meta2 <- tibble::rownames_to_column(meta1, "site_name")
# assigning new column names to some columns in div dataframe
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

# separate the channel column by the "_" into sytsem and Channel
div1<- separate(div1, Channel, into = c("System", "Channel"), sep = "_")
div1
#combine div1 with meta dataset
div2 <- left_join(div1, meta2, by = "Channel")
div2
# Reorder the factor levels
div2$treatments <- factor(div2$treatments, levels = c("NBC","RBC","NSC","NBI",
                                                      "RSC","RBI","NSI","RSI"))
div2
# select all columns except "method"

range(div2$Richness)
range(div2$Shannon)
range(div2$Simpson)
# select only stressor phase samples
div_stress <- div2 %>%
  filter(Phase == "stress")
range(div_stress$Richness)
range(div_stress$Shannon)
range(div_stress$Simpson)
mean(div_stress$Richness)
mean(div_stress$Shannon)
mean(div_stress$Simpson)

# Select only phase samples 
div_rec <- div2 %>%
  filter(Phase == "recovery")
range(div_rec$Richness)
range(div_rec$Shannon)
range(div_rec$Simpson)
mean(div_rec$Richness)
mean(div_rec$Shannon)
mean(div_rec$Simpson)
# remove column 3 in div2
#div2 <- div2[,-3]


# run multiple comparisons for analysis of variance (t-test or ANOVA)
# Multiple comparisons
#update.packages(ask = FALSE)
library(tidyverse)
#install.packages("ggstatsplot")
library(ggstatsplot)
# Comparison between species

# edit from here
x <- "treatments"
cols <- 3:12 # the 4 continuous dependent variables
type <- "parametric" # given the large number of observations, we use the parametric version
paired <- FALSE # FALSE for independent samples, TRUE for paired samples

#stressor phase
# edit until here

# edit at your own risk
plotlist <-
  purrr::pmap(
    .l = list(
      data = list(as_tibble(div_stress)),
      x = x,
      y = as.list(colnames(div_stress)[cols]),
      plot.type = "box", # for boxplot
      type = type, # parametric or nonparametric
      pairwise.comparisons = TRUE, # to run post-hoc tests if more than 2 groups
      pairwise.display = "significant", # show only significant differences
      bf.message = TRUE, # remove message about Bayes Factor
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

1.75*0.001
4.11*0.0001
# recovery phase
# edit until here

# edit at your own risk
plotlist <-
  purrr::pmap(
    .l = list(
      data = list(as_tibble(div_rec)),
      x = x,
      y = as.list(colnames(div_rec)[cols]),
      plot.type = "box", # for boxplot
      type = type, # parametric or nonparametric
      pairwise.comparisons = TRUE, # to run post-hoc tests if more than 2 groups
      pairwise.display = "significant", # show only significant differences
      bf.message = TRUE, # remove message about Bayes Factor
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
## indicator species analysis (ISA) with microscopy data
library(indicspecies)

set.seed(123)
## digital microscopy
# cluster by treatments
diat.ISA <- multipatt(x = diat1,
                      cluster = meta1$treatments,
                      duleg = TRUE)
summary(diat.ISA)
### cluster by phase
diat.ISA2 <- multipatt(x = diat1,
<<<<<<< HEAD
                       cluster = meta1$Phase,
                       duleg = TRUE)
=======
                      cluster = meta1$Phase,
                      duleg = TRUE)
>>>>>>> 5997fedc77a4a453d580e63d133156747a2321ea
summary(diat.ISA2)


## 18S-V9
diat.ISA_18S <- multipatt(x = dna_spe_trans,
<<<<<<< HEAD
                          cluster = meta1$treatments,
                          duleg = TRUE)
summary(diat.ISA_18S)
##Phase
diat.ISA_18SPhase<- multipatt(x = dna_spe_trans,
                              cluster = meta1$Phase,
                              duleg = TRUE)
=======
                      cluster = meta1$treatments,
                      duleg = TRUE)
summary(diat.ISA_18S)
##Phase
diat.ISA_18SPhase<- multipatt(x = dna_spe_trans,
                          cluster = meta1$Phase,
                          duleg = TRUE)
>>>>>>> 5997fedc77a4a453d580e63d133156747a2321ea
summary(diat.ISA_18SPhase)

selected_rows <- c("N42151_1615","N37669_544","N28073_324", "N27270_242", "N30714_137")
# Use the row names as criteria to subset and view specific observations
selected_rows <- dna[dna$Name %in% selected_rows, ]
print(selected_rows)

##############################
## turnover and nestedness: 18S
turn <- read.csv("turn_nest_dna.csv", sep = ",",
                 header = T)
# filter only phases: stress and recovery
turn1 <- turn %>%
  filter(Phase == "Stress" | Phase == "Recovery")
# comparison: # Perform one-way ANOVA
anova_turnover <- aov(Turnover ~ Phase, data = turn1)
# Print the ANOVA table
summary(anova_turnover)
turnover_stress <- turn1 %>% filter(Phase == "Stress")
range(turnover_stress$Turnover)
mean(turnover_stress$Turnover)

turnover_rec <- turn1 %>% filter(Phase == "Recovery")
range(turnover_rec$Turnover)
mean(turnover_rec$Turnover)

# comparison: # Perform one-way ANOVA
anova_Nestedness <- aov(Nestedness ~ Phase, data = turn1)
# Print the ANOVA table
summary(anova_Nestedness)
# comparison: # Perform one-way ANOVA
anova_Sorensen <- aov(Sorensen ~ Phase, data = turn1)
# Print the ANOVA table
summary(anova_Sorensen)

# comparison: # Perform one-way ANOVA against treatment
anova_turnover1 <- aov(Turnover ~ Treatment, data = turn1)
# Print the ANOVA table
summary(anova_turnover1)
# comparison: # Perform one-way ANOVA
anova_Nestedness1 <- aov(Nestedness ~ Treatment, data = turn1)
# Print the ANOVA table
summary(anova_Nestedness1)
# Perform pairwise comparisons using Tukey's HSD
tukey_results <- TukeyHSD(anova_Nestedness1)

# View the results
print(tukey_results)
# comparison: # Perform one-way ANOVA
anova_Sorensen1 <- aov(Sorensen ~ Treatment, data = turn1)
# Print the ANOVA table
summary(anova_Sorensen1)
# Create a bar plot
# Extract the comparisons, p-values, and other relevant information
tukey_df <- data.frame(
  Comparison = rownames(tukey_results$`group`[,1]),
  Difference = tukey_results$`group`[, 2],
  Lower_CI = tukey_results$`group`[, 3],
  Upper_CI = tukey_results$`group`[, 4],
  p_value = p.adjust(tukey_results$`group`[, 5], method = "BH") # Adjusted p-values
)
#In this code, we create a dataframe tukey_df that includes the comparison groups, 
#the mean differences, confidence intervals, and adjusted p-values. You may want to 
#adjust the column names and additional details depending on the specific information 
#you need from the TukeyHSD results.

#This code will allow you to work with the TukeyHSD results in a more dataframe-like 
#format for further analysis and visualization.
significant_results <- summary(tukey_results) %>%
  as.data.frame() %>%
  filter(p adj < 0.05)  # Filter for significant results (adjust alpha as needed)

###############
##############################
## Gamma_div 18S
#Phase
gamma <- read.csv("gamma_div.csv", sep = ",",
<<<<<<< HEAD
                  header = T)
=======
                 header = T)
>>>>>>> 5997fedc77a4a453d580e63d133156747a2321ea

# comparison: # Perform one-way ANOVA
anova_gamma <- aov(gamma_div ~ Phase, data = gamma)
# Print the ANOVA table
summary(anova_gamma)
## Treatments per phase
#stress
# filter only phases: stress and recovery
#stress
library(tidyverse)
gamma_stress <- gamma %>%
  filter(Phase == "stress")
range(gamma_stress$gamma_div)
mean(gamma_stress$gamma_div)
#anova_gamma_str <- aov(gamma_stress ~ Treatment, data = gamma)
#summary(anova_gamma_str)
# recovery
gamma_rec <- gamma %>%
  filter(Phase == "recovery")
range(gamma_rec$gamma_div)
mean(gamma_rec$gamma_div)
#anova_gamma_rec <- aov(gamma_div ~ Treatment, data = gamma)
#summary(anova_gamma_rec)
######################
##############################
## turnover and nestedness: digital microscopy
turn_micro <- read.csv("turn_nest_micro.csv", sep = ",",
<<<<<<< HEAD
                       header = T)
=======
                 header = T)
>>>>>>> 5997fedc77a4a453d580e63d133156747a2321ea
# filter only phases: stress and recovery
turn2 <- turn_micro %>%
  filter(Phase == "Stress" | Phase == "Recovery")
# comparison: # Perform one-way ANOVA
anova_turnover1 <- aov(Turnover ~ Phase, data = turn2)
# Print the ANOVA table
summary(anova_turnover1)
turnover_stress <- turn2 %>% filter(Phase == "Stress")
range(turnover_stress$Turnover)
mean(turnover_stress$Turnover)

turnover_rec <- turn2 %>% filter(Phase == "Recovery")
range(turnover_rec$Turnover)
mean(turnover_rec$Turnover)

# comparison: # Perform one-way ANOVA
anova_Nestedness <- aov(Nestedness ~ Phase, data = turn2)
# Print the ANOVA table
summary(anova_Nestedness)
# comparison: # Perform one-way ANOVA
anova_Sorensen <- aov(Sorensen ~ Phase, data = turn2)
# Print the ANOVA table
summary(anova_Sorensen)

# comparison: # Perform one-way ANOVA against treatment
anova_turnover1 <- aov(Turnover ~ Treatment, data = turn2)
# Print the ANOVA table
summary(anova_turnover1)
# comparison: # Perform one-way ANOVA
anova_Nestedness1 <- aov(Nestedness ~ Treatment, data = turn2)
# Print the ANOVA table
summary(anova_Nestedness1)
# Perform pairwise comparisons using Tukey's HSD
tukey_results <- TukeyHSD(anova_Nestedness)

# View the results
print(tukey_results)
# comparison: # Perform one-way ANOVA
anova_Sorensen1 <- aov(Sorensen ~ Treatment, data = turn2)
# Print the ANOVA table
summary(anova_Sorensen1)
# Create a bar plot
# Extract the comparisons, p-values, and other relevant information
tukey_df <- data.frame(
  Comparison = rownames(tukey_results$`group`[,1]),
  Difference = tukey_results$`group`[, 2],
  Lower_CI = tukey_results$`group`[, 3],
  Upper_CI = tukey_results$`group`[, 4],
  p_value = p.adjust(tukey_results$`group`[, 5], method = "BH") # Adjusted p-values
)
#In this code, we create a dataframe tukey_df that includes the comparison groups, 
#the mean differences, confidence intervals, and adjusted p-values. You may want to 
#adjust the column names and additional details depending on the specific information 
#you need from the TukeyHSD results.

#This code will allow you to work with the TukeyHSD results in a more dataframe-like 
#format for further analysis and visualization.
significant_results <- summary(tukey_results) %>%
  as.data.frame() %>%
  filter(p adj < 0.05)  # Filter for significant results (adjust alpha as needed)

###############
##############################
## Gamma_div micro
#Phase
gamma_micro <- read.csv("gamma_div_micro.csv", sep = ",",
<<<<<<< HEAD
                        header = T)
=======
                  header = T)
>>>>>>> 5997fedc77a4a453d580e63d133156747a2321ea

# comparison: # Perform one-way ANOVA
anova_gamma <- aov(Gamma_div ~ Phase, data = gamma_micro)
# Print the ANOVA table
summary(anova_gamma)
## Treatments per phase
#stress
# filter only phases: stress and recovery
#stress
gam_str_micro <- gamma_micro %>%
  filter(Phase == "stress")
range(gam_str_micro$Gamma_div)
mean(gam_str_micro$Gamma_div)
# recovery
gam_rec_micro <- gamma_micro %>%
  filter(Phase == "recovery")
range(gam_rec_micro$Gamma_div)
mean(gam_rec_micro$Gamma_div)


# Load necessary libraries
library(lme4)

# Assuming diatom_data is your dataset
# Fit a GLMM
model <- glmer(diat5 ~ Temperature + Salinity + Velocity + (1 | treatments), 
               data = meta, family = "poisson")

# Summarize the model
summary(model)

# Generate mock data
set.seed(123)
n <- 100  # Number of observations
diatom_data <- data.frame(
  response_variable = rpois(n, lambda = 10),  # Assuming count data, change accordingly
  temperature = rnorm(n, mean = 25, sd = 5),
  salinity = rnorm(n, mean = 30, sd = 8),
  flow_velocity = rnorm(n, mean = 0.5, sd = 0.2),
  site = rep(1:5, each = n/5)  # Five sites
)

# Fit a GLMM
model <- glmer(response_variable ~ temperature + salinity + flow_velocity + (1 | site), data = diatom_data, family = poisson)

# Summarize the model
summary(model)


######################################################################################
#### plotting scraped area composition
set.seed(123)
scr_read_ab <- scraped_area[as.logical(rowSums(scraped_area[,1:32]!= 0)), ]
view(scr_read_ab)
colSums(scr_read_ab[,1:32])
scr_18S <- scr_read_ab[,1:32]
scr_18S_t <- t(scr_18S)
x <- scr_18S_t
view(x)
x <- as.data.frame(x)
# make Channel column as rownames
x <- x %>% rownames_to_column(var="Sample") # change row names
# Use separate to split the full_column into two columns based on "_"
x1 <- separate(x, Sample, into = c("System", "Sample", "ph"), sep = "_")
x2 <- x1[,-c(1,3)]
# column to rownames
x3 <- x2 %>% column_to_rownames(var="Sample")
#diat<-diat/rowSums(diat)
x<-x3/rowSums(x3)

#x<-x3[,order(colSums(x3),decreasing=TRUE)]
#  x1<-spe5/rowSums(spe5)*100
#diat<-diat[,order(colSums(diat),decreasing=TRUE)]
#  x1<-x1[,order(colSums(x1),decreasing=TRUE)]
colSums(x)
rowSums(x)
#colSums(x1)
#head(diat)
#head(x1)
#x <- t(x) #transpose
#Extract list of top N Taxa
N<-31
taxa_list_18S<-scr_read_ab[,41]
#taxa_list1<-colnames(x1)[1:N]
N<-length(taxa_list_18S)
#N1<-length(taxa_list1)

#Create a custom color scale
library(RColorBrewer)
# Define the number of colors you want
nb.cols <- 35
mycolors <- colorRampPalette(brewer.pal(12, "Paired"))(nb.cols)

# other colours
#library(RColorBrewer)
#install.packages("randomcoloR")
library(randomcoloR)
n <- 35
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
pie(rep(1,n), col=sample(col_vector, n))
palette <- distinctColorPalette(n)
#Colours <- brewer.pal(50,"Set")
#names(Colours) <- levels(diatom_ab$Taxa)
#colScale <- scale_colour_manual(name = "Taxa",values = Colours)

#Generate a new table with everything added to Others
#new_x<-data.frame(x[,colnames(x) %in% taxa_list],Others=rowSums(x[,!colnames(x) %in% taxa_list]))
#You can change the Type=grouping_info[,1] should you desire any other grouping of panels
meta1_rec <- subset(meta1, Phase == "recovery")

df_18S_scr<-NULL
for (i in 1:dim(x)[2]){
  tmp<-data.frame(row.names=NULL,Sample=rownames(x), Treatments=meta1_rec$treatments, Salinity=meta1_rec$Salinity,
                  Temperature=meta1_rec$Temperature, Velocity=meta1_rec$Velocity, Phase=meta1_rec$Phase,
                  Taxa=rep(scr_read_ab$Species[i],dim(x)[1]),Value=x[,i])
  if(i==1){df_18S_scr<-tmp} else {df_18S_scr<-rbind(df_18S_scr,tmp)}
}

df_Class<-NULL
for (i in 1:dim(x)[2]){
  tmp<-data.frame(row.names=NULL,Sample=rownames(meta1), Treatments=meta1$treatments, Salinity=meta1$Salinity,
                  Temperature=meta1$Temperature, Velocity=meta1$Velocity, Phase=meta1$Phase,
                  Class=rep(algae1$Class[i],dim(x)[1]),Value=x[,i])
  if(i==1){df_Class<-tmp} else {df_Class<-rbind(df_Class,tmp)}
}

# Replace all the taxa with lower than 1% occurrence in the sample with Others
df_18S_scr_others <- df_18S_scr %>% mutate(Taxa = replace(Taxa, Value <= 0.01, "Others (<1%)"))

# Based on site experiment phase: plot community composition in the channels
# subset stressor phase
#set.seed(123)

# subset recovery phase scraped surface
df_18S_rec <- subset(df_18S_scr_others, Phase == "recovery")

taxa18S_recovery_scr <- ggplot(df_18S_scr_others, aes(x=Sample,y=Value, fill=Taxa))+geom_bar(stat="identity")+
  facet_grid(. ~ Treatments, drop=TRUE,scale="free",space="free_x")+
  scale_fill_manual(values=palette[1:(N+1)]) + theme_bw()+ylab("Reads abundance") +
  scale_y_continuous(expand = c(0,0))+theme(strip.background = element_rect(fill="gray85"))+
  theme(panel.margin = unit(0.3, "lines")) +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=0.5, size = 9, face = "bold"), 
        strip.text = element_text(size = 14, face = "bold"))+
  theme(legend.key.size = unit(0.7, "cm"), legend.key.width = unit(0.7,"cm"), 
        legend.text = element_text(size = 14, face = "italic"))+
  guides(fill=guide_legend(ncol=1))

taxa18S_recovery_scr
ggsave("taxa18S_recovery_scr.tiff", units="in", width=10, height=8, dpi = 300, compression = 'lzw')

# facet_warp treatments
exstream2022_18S_scr_treat2 <- ggplot(df_18S_scr_others, aes(x=Sample,y=Value, fill=Taxa))+geom_bar(stat="identity")+
  facet_grid(Phase ~ Treatments, drop=TRUE,scale="free",space="free_x")+
  scale_fill_manual(values=palette[1:(N+1)]) + theme_bw()+ylab("Relative abundance of OTU reads") +
  scale_y_continuous(expand = c(0,0))+theme(strip.background = element_rect(fill="gray85"))+
  theme(panel.margin = unit(0.3, "lines")) +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=0.5, size = 5, face = "bold"), 
        strip.text = element_text(size = 9, face = "bold"))+
  theme(legend.key.size = unit(0.4, "cm"), legend.key.width = unit(0.4,"cm"), 
        legend.text = element_text(size = 7))+
  guides(fill=guide_legend(ncol=1))

exstream2022_dna_treat2
# save the plot
ggsave("exstream2022_dna_treat2.tiff", units="in", width=9, height=9, dpi=300, compression = 'lzw')

######################################
library(vegan)

# Simulated data (replace this with your actual data)
set.seed(123)
num_samples <- 50
num_species <- 20

# Creating a data frame with community composition data
data <- data.frame(
  Site = rep(c("Control", "Treatment"), each = num_samples/2),
  matrix(rpois(num_samples * num_species, lambda = 5), ncol = num_species)
)

# Assigning row names to the data frame
rownames(data) <- paste0("Sample", 1:num_samples)

# Performing PERMANOVA
permanova_result <- adonis2(data[,-1] ~ Site, data = data, permutations = 999)

# Displaying the result
print(permanova_result)
####################################################################
# comparison of individual treatment against control
# digital  microscopy
<<<<<<< HEAD
############################
# Priority effects
library(indicspecies)
group <- meta1$Phase
prio_eff <- multipatt(diat5,meta1$Phase)
summary(prio_eff)
=======
>>>>>>> 5997fedc77a4a453d580e63d133156747a2321ea
