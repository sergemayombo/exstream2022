# SFB RESIST
# ExStream 2022
# Analysis of TECAN plate reader data
# Serge Mayombo
# 10.10.2023

# Load necessary libraries
library(tidyverse)
# Import data
tecan <- read.csv("exstream2022_tecan.csv", sep=",", 
                  header=T)
View(tecan)
# read exstream metadata
metadata <- read.csv("metadata_IMP2022.csv")
#metadata1 <- metadata[,-6]
head(metadata)
#metadata_IMP <- metadata1 %>%
# filter(!is.na(F), System == "IMP")
# edit channel treatments : salt in C07 and background in C08
# Assuming df is your dataset
metadata[3, 6] <- "salt"
metadata[3, 8] <- "NSI"
metadata[4, 6] <- "background"
metadata[4, 8] <- "RBI"
head(metadata)
#metadata_IMP <- metadata1 %>%
# filter(!is.na(F), System == "IMP")
metadata$Temperature[metadata$Temperature == "normal"] <- "control"
metadata$treatments[metadata$treatments == "NBN"] <- "NBC"
metadata$treatments[metadata$treatments == "NSN"] <- "NSC"
metadata$treatments[metadata$treatments == "RBN"] <- "RBC"
metadata$treatments[metadata$treatments == "RSN"] <- "RSC"
########################################
### EDM
metadata$treatments[metadata$treatments == "NBC"] <- "C"
metadata$treatments[metadata$treatments == "RBC"] <- "F"
metadata$treatments[metadata$treatments == "NSC"] <- "S"
metadata$treatments[metadata$treatments == "NBI"] <- "T"
metadata$treatments[metadata$treatments == "RSC"] <- "F+S"
metadata$treatments[metadata$treatments == "RBI"] <- "F+T"
metadata$treatments[metadata$treatments == "NSI"] <- "S+T"
metadata$treatments[metadata$treatments == "RSI"] <- "F+S+T"

###############################
metadata$Temperature <- factor(metadata$Temperature)
metadata$Salinity <- factor(metadata$Salinity)
metadata$Velocity <- factor(metadata$Velocity)
metadata$Phase <- factor(metadata$Phase)
metadata$treatments <- factor(metadata$treatments)
# Reorder the treatment factor levels
metadata$treatments <- factor(metadata$treatments, 
                              levels = c("C","F","S","T",
                                         "F+S","F+T","S+T","F+S+T"))
head(metadata)
############
# make channel column as rownames
#meta <- metadata %>% column_to_rownames(var="Channel") # change row names
#str(meta)
###############
##### Correct salt treatments of channels C07 & C08 since they were exchanged
# Define the observation you want to modify (e.g., "Bob")
#selected_observation <- "C07"
#selected_observation1 <- "C08"
# Use subsetting to select all cells associated with the observation
# Then assign new values to those cells
#metadata[metadata$Channel == selected_observation, "Salinity"] <- "salt"
#metadata[metadata$Channel == selected_observation, "treatments"] <- "NSI"
#metadata[metadata$Channel == selected_observation1, "Salinity"] <- "background"
#metadata[metadata$Channel == selected_observation1, "treatments"] <- "RBI"
# View the modified data frame
View(metadata)
#library(tidyverse)
# remove channels C07 & C08 because they were exchange for salt treatment
# but we do not know what happened with velocity and temperature (black boxes)
#metadata1 <- metadata[-c(3,4),]
#change order of factors in meta table
library(tidyverse)
meta1 <- metadata %>%
  mutate(Phase = fct_relevel(Phase,"stress", "recovery"))
############################################################################
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
#meta <- metadata %>% column_to_rownames(var="Channel") # change row names
#str(meta)

#change order of factors in meta table
metadata <- metadata %>%
  mutate(Phase = fct_relevel(Phase,"stress", "recovery"))

# Combine var1 and var2 with an underscore separator
#meta1$combined_var <- factor(paste(meta1$treatments, meta1$Phase, sep = "_"))

############################################################################
# combine the 2 datasets
tecan_full <- left_join(tecan, metadata, by = "Channel")
View(tecan_full)
# Mock data: Assume you have data for different combinations of temperature, flow velocity, and salinity 
# and the fluorescence measurements for chlorophyll in microphytobenthos.
set.seed(123)

# Standardize the Tecan fluorescence data
tecan_full$standardized_fluorescence <- tecan_full$Mean / tecan_full$surf_area

# Check the first few rows
head(tecan_full)
# filter by phase and group by treatment
# stress
tecan_stress <- tecan_full %>% 
  filter(Phase == "stress")
# grouping par treatment
summary_tecan_stress <- tecan_stress %>%
  group_by(treatments) %>% 
  #select("Phase", "Velocity", "Salinity","Temperature", "treatments") %>% 
  summarise(
    Mean_fluor = mean(standardized_fluorescence, na.rm = TRUE),
    SD_fluor = sd(standardized_fluorescence, na.rm = TRUE)
    )
write.csv(summary_tecan_stress, "tecan_stress.csv")
# recovery
tecan_rec <- tecan_full %>% 
  filter(Phase == "recovery")
# grouping par treatment
summary_tecan_rec <- tecan_rec %>%
  group_by(treatments) %>% 
  #select("Phase", "Velocity", "Salinity","Temperature", "treatments") %>% 
  summarise(
    Mean_fluor = mean(standardized_fluorescence, na.rm = TRUE),
    SD_fluor = sd(standardized_fluorescence, na.rm = TRUE)
  )

# Boxplot for fluorescence data by temperature
tecan_plot <- ggplot(tecan_full, aes(x = treatments, y = standardized_fluorescence)) +
  geom_point() +
  geom_boxplot(aes(fill = treatments)) +
  facet_wrap(Phase ~ .) +
  labs(title = "") +
  theme(#axis.text.x = element_blank(),  # remove x-axis text
    #axis.text.y = element_blank(), # remove y-axis text
    #axis.ticks = element_blank(),  # remove axis ticks
    #axis.title.x = element_text(size=18), # remove x-axis labels
    #axis.title.y = element_text(size=18), # remove y-axis labels
    panel.background = element_blank(), 
    #panel.grid.major = element_blank(),  #remove major-grid labels
    #panel.grid.minor = element_blank(),  #remove minor-grid labels
    plot.background = element_blank())

tecan_plot

# Calculate the counts by group
counts <- tecan_full %>%
  group_by(treatments, Phase) %>%
  summarize(n = n())

# Combine group and count into one label
tecan_full$GroupLabel <- paste0(tecan_full$Group, " (n=", counts$n, ")")

# Create the modified plot
tecan_plot_with_counts <- ggplot(tecan_full, aes(x = treatments, 
                                                 y = standardized_fluorescence)) +
  geom_point() +
  geom_boxplot(aes(fill = treatments)) +
  facet_wrap(Phase ~ .) +
  geom_text(data = counts, aes(label = n), vjust = -0.5, size = 3) +  # Add single n value
  labs(title = "") +
  theme_bw()
  
tecan_plot
tecan_plot_with_counts

# Assuming 'counts' dataframe contains counts of observations per group
# Make sure 'counts' dataframe has a column 'treatments' for the groupings

# Assuming 'counts' dataframe contains counts of observations per group
# Make sure 'counts' dataframe has a column 'treatments' for the groupings

ggplot(tecan_full, aes(x = treatments, y = standardized_fluorescence)) +
  geom_point() +
  geom_boxplot(aes(fill = treatments)) +
  facet_wrap(Phase ~ .) +
  stat_summary(fun.data = "mean_se", geom = "text", aes(label = after_stat(..n..)), vjust = -0.5, size = 3) +
  labs(title = "") +
  theme_bw()

ggsave("tecan_plot_EDM20242.tiff", units="in", width=7, height=7, dpi=600, compression = 'lzw')

str(tecan_full)
ggplot(tecan_full, aes(x = Temperature, y = standardized_fluorescence, color = Velocity)) +
  geom_boxplot() +
  facet_grid(Salinity ~ .) +
  theme_minimal()

model1 <- aov(standardized_fluorescence ~ Temperature * Velocity * Salinity, 
             data = tecan_full, subset = Phase == "stress")
summary(model1)
tukey.test <- TukeyHSD(model1)
tukey.test
model2 <- aov(standardized_fluorescence ~ (Temperature + Velocity + Salinity), 
              data = tecan_full, subset = Phase == "stress")
summary(model2)

model2 <- aov(standardized_fluorescence ~ Temperature * Velocity * Salinity, 
              data = tecan_full, subset = Phase == "recovery")
summary(model2)

model3 <- aov(standardized_fluorescence ~ Phase, 
              data = tecan_full)
summary(model3)


# Load the required package
#install.packages("lme4")
library(lme4)

# Model: Fluorescence as a function of Temperature, FlowVelocity, Salinity, and their interactions. 
# Assume random effect for Replicate
model <- lmer(standardized_fluorescence ~ Temperature * Velocity * Salinity + (1|treatments), data = tecan_full)
summary(model)

########################
# comparaing treatments against control
# stress
# remove channels C07 & C08 because they were exchange for salt treatment
# but we do not know what happened with velocity and temperature (black boxes)
tecan_full1 <- tecan_full1[-c(35,36),]
stress <- tecan_full %>% filter(Phase == "stress")
NBC_str <- stress %>% subset (treatments == "NBC")
RBC_str <- stress %>% subset (treatments == "RBC")
NSC_str <- stress %>% subset(treatments == "NSC")
NBI_str <- stress %>% subset(treatments == "NBI")
RSC_str <- stress %>% subset (treatments == "RSC")
RBI_str <- stress %>% subset (treatments == "RBI")
NSI_str <- stress %>% subset(treatments == "NSI")
RSI_str <- stress %>% subset(treatments == "RSI")



# Assuming your_data is your data frame
# Assuming 'value' is the column containing the values

# Perform t-test
test1 <- t.test(RBC_str$Mean, NBC_str$Mean)
test1
test2 <- t.test(NSC_str$Mean, NBC_str$Mean)
test2
test3 <- t.test(NBI_str$Mean, NBC_str$Mean)
test3
test4 <- t.test(RSC_str$Mean, NBC_str$Mean)
test4
test5 <- t.test(RBI_str$Mean, NBC_str$Mean)
test5
test6 <- t.test(NSI_str$Mean, NBC_str$Mean)
test6
test7 <- t.test(RSI_str$Mean, NBC_str$Mean)
test7
