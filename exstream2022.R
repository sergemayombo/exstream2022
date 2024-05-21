# MINI PAM II measurements
# Biomass estimation
# Analysis to test the response of 
# microphytobenthos biomass to salinity, temperature and flow velocity
# ExStream 2022
# Serge Mayombo
# 18.09.2023


set.seed(123)
library(tidyverse)
#read PAM data
data <- read.csv("PAM2022_IMPclean.csv")
head(data)
data <- data[,-c(17,18,19,20,21,22,23,24)]
# Change the column name
colnames(data)[colnames(data) == "F"] <- "F0"
view(data)

#data_IMP <- data %>%
 # filter(!is.na(F), System == "IMP")
#data_IMP <- data_IMP[,-6]

#+write.csv(data_IMP, "PAM_IMP_2022.csv")
# Check for missing values
summary(data)
# read exstream metadata
metadata <- read.csv("treatments_exstream2022.csv")
metadata1 <- metadata[,-7]
head(metadata1)
metadata_IMP <- metadata1 %>%
  filter(!is.na(F), System == "IMP")
########
# EDM 
data$treatments[data$treatments == "NBC"] <- "C"
data$treatments[data$treatments == "RBC"] <- "F"
data$treatments[data$treatments == "NSC"] <- "S"
data$treatments[data$treatments == "NBI"] <- "T"
data$treatments[data$treatments == "RSC"] <- "F+S"
data$treatments[data$treatments == "RBI"] <- "F+T"
data$treatments[data$treatments == "NSI"] <- "S+T"
data$treatments[data$treatments == "RSI"] <- "F+S+T"
# EDM
data$treatments <- factor(data$treatments, 
                              levels = c("C","F","S","T",
                                         "F+S","F+T","S+T","F+S+T"))

##########
# combine PAM dataset and metadat dataset
#data_full <- left_join( data, metadata, join_by(Channel))

# change data type
data_full$Temperature <- as.factor(data_full$Temperature)
data_full$Velocity <- as.factor(data_full$Velocity)
data_full$Salinity <- as.factor(data_full$Salinity)
data_full$duration <- as.factor(data_full$duration)


# If there are missing values, you can decide to filter them out or replace them
# and filter only IMP system
# Example: Removing rows with missing values
data_clean <- na.omit(data_full)

str(data_full)

# or using imputation, for instance:
# data$column_name <- ifelse(is.na(data$column_name), mean(data$column_name, na.rm=TRUE), data$column_name)

# Example: t-test
t.test(F ~ Velocity, data = data_full)

ggplot(data_full, aes(x=duration, y=F)) +
  geom_point(aes(color=Phase)) +
  labs(title="MINI-PAM II Measurements")

# Use linear modelling
model <- lm(F ~ Salinity + Velocity + Temperature, data=data_full)
summary(model)
plot(model)

model_with_interaction <- lm(F ~ Salinity * Temperature + Velocity, data=data_full)
summary(model_with_interaction)
###############################################
### Plotting PAM data

ggplot(data, aes(x=duration, y=F, color=Phase)) +
  #geom_line() +
  geom_point() +
  geom_smooth(method = "lm", alpha = .15, aes(fill = Phase)) +
  labs(title="Evolution of Microphytobenthos Biomass Over Time",
       x="Time (days)",
       y="Biomass (mg)",
       color="Temperature (?C)") +
  theme_minimal()

ggplot(data, aes(x = Day, y = Y.II., color = Phase)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = .15, aes(fill = Phase)) 

ggplot(data, aes(x = duration, y = Y.II., color = Phase)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = .15, aes(fill = Temperature)) 


ggplot(combined_PAMdata, aes(x = Day, y= F, color = treatments)) + 
  geom_point() + facet_wrap(facets = vars(Temperaure)) +
  geom_smooth(method = "lm")
#F0
P1 <- ggplot(data, aes(x=duration, y=F0, color=treatments)) +
  #geom_line() +
  #geom_point() +
  geom_smooth(method="loess", se=FALSE) +
  labs(title="Microphytobenthos Growth Over Time",
       x="Time",
       y= expression(paste("Photosynthetic biomass (F"[0], ")"))) #+
  #theme_minimal()
P1
#Fm
P2 <- ggplot(data, aes(x=duration, y=Fm, color=treatments)) +
  #geom_line() +
  #geom_point() +
  geom_smooth(method="loess", se=FALSE) +
  labs(title="",
       x="Time",
       y= expression(paste("Photosynthetic biomass (F"[0], ")"))) +
  theme(legend.position="none") 
  #theme_minimal()
P2
# Fv/Fm
P3 <- ggplot(data_full, aes(x=duration, y=Y.II., color=treatments)) +
  #geom_line() +
  #geom_point() +
  geom_smooth(method="loess", se=FALSE) +
  labs(title="",
       x="Time (days)",
       y= "Photosynthetic biomass (Fv/Fm)") +
  theme_minimal()
P3
# Fv/Fm
# stressor
# Reorder the factor levels

data$treatments <- factor(data$treatments, 
                          levels = c("NBC","RBC","NSC","NBI",
                                     "RSC","RBI","NSI","RSI"))

ph_stress <- data %>% subset(Phase=="stress")
custom_breaks <- c(1, 5, 10, 15)

library(cowplot) # for background_grid
pl1 <- ggplot(ph_stress, aes(duration, Y.II.)) + 
  geom_point() + facet_grid(. ~ treatments) + stat_smooth(method = "lm", col="red") +
  background_grid(major = 'y', minor = "none") + # add thin horizontal lines
  scale_x_continuous(breaks = custom_breaks) +
  panel_border() + # and a border around each panel
  labs(title="Stressor phase",
       x="",
       y= "Y(II) or Fv/Fm")

pl1

# customize x-axis
#######################
# using combined_PAMdata for plotting

# recovery
ph_recovery <- data %>% subset(Phase=="recovery")

# customize the x-axis
custom_breaks1 <- c(15, 20, 25, 30)

pl2 <- ggplot(ph_recovery, aes(duration, Y.II.)) + 
  geom_point() + facet_grid(. ~ treatments) + stat_smooth(method = "lm", col="blue") +
  background_grid(major = 'y', minor = "none") + # add thin horizontal lines 
  panel_border() + # and a border around each panel
  scale_x_continuous(breaks = custom_breaks1) +
  labs(title="Recovery phase",
       x="",
       y= "Y(II) or Fv/Fm")

pl2
# scraped area
# recovery
ph_recovery_scr <- data %>% subset(Phase=="recovery_scraped")

pl3 <- ggplot(ph_recovery_scr, aes(duration, Y.II.)) + 
  geom_point() + facet_grid(. ~ treatments) + stat_smooth(method = "lm", col="green") +
  background_grid(major = 'y', minor = "none") + # add thin horizontal lines 
  panel_border() + # and a border around each panel
  scale_x_continuous(breaks = custom_breaks1) +
  labs(title="Recovery over the scraped area",
       x="Time (days)",
       y= "Y(II) or Fv/Fm")

pl3
#install.packages("cowplot")
#library(cowplot)
PSII_corrected <- plot_grid(pl1, pl2, pl3, labels=c("A", "B", "C"), ncol = 1, nrow = 3)
PSII_corrected 
ggsave("PSII_EDM2024.tiff", units="in", width=9, height=9, dpi=600, compression = 'lzw')

# # Minimum chlorophyll fluorescence (F0)
# stressor

ph_stress <- data %>% subset(Phase=="stress")
custom_breaks <- c(1, 5, 10, 15)

pf01 <- ggplot(ph_stress, aes(duration, F)) + 
  geom_point() + facet_grid(. ~ treatments) + stat_smooth(method = "lm", col="red") +
  background_grid(major = 'y', minor = "none") + # add thin horizontal lines
  scale_x_continuous(breaks = custom_breaks) +
  panel_border() + # and a border around each panel
  labs(title="Stressor phase",
       x="",
       y= expression(paste("Min. fluoresc. (F"[0], ")")))

pf01
# recovery
ph_recovery <- data %>% subset(Phase=="recovery")

# customize the x-axis
custom_breaks1 <- c(15, 20, 25, 30)

pf02 <- ggplot(ph_recovery, aes(duration, F)) + 
  geom_point() + facet_grid(. ~ treatments) + stat_smooth(method = "lm", col="blue") +
  background_grid(major = 'y', minor = "none") + # add thin horizontal lines 
  panel_border() + # and a border around each panel
  scale_x_continuous(breaks = custom_breaks1) +
  labs(title="Recovery phase",
       x="",
       y= expression(paste("Min. fluoresc. (F"[0], ")")))
pf02
# scraped area
# recovery
ph_recovery_scr <- data %>% subset(Phase=="recovery_scraped")

pf03 <- ggplot(ph_recovery_scr, aes(duration, F)) + 
  geom_point() + facet_grid(. ~ treatments) + stat_smooth(method = "lm", col="green") +
  background_grid(major = 'y', minor = "none") + # add thin horizontal lines 
  panel_border() + # and a border around each panel
  scale_x_continuous(breaks = custom_breaks1) +
  labs(title="Recovery over the scraped area",
       x="Time (days)",
       y= expression(paste("Min. fluoresc. (F"[0], ")")))

pf03
#install.packages("cowplot")
library(cowplot)
f0_EDM2024 <- plot_grid(pf01, pf02, pf03, labels=c("A", "B", "C"), ncol = 1, nrow = 3)

f0_EDM2024
ggsave("f0_EDM2024.tiff", units="in", width=9, height=9, dpi=600, compression = 'lzw')

# Maximum fluorescence yield of dark acclimated biofilm Fm
# stressor phase
ph_stress <- data %>% subset(Phase=="stress")
custom_breaks <- c(1, 5, 10, 15)

pfm1 <- ggplot(ph_stress, aes(duration, Fm)) + 
  geom_point() + facet_grid(. ~ treatments) + stat_smooth(method = "lm", col="red") +
  background_grid(major = 'y', minor = "none") + # add thin horizontal lines
  scale_x_continuous(breaks = custom_breaks) +
  panel_border() + # and a border around each panel
  labs(title="Stressor phase",
       x="",
       y= "Max. fluoresc. (Fm)")
pfm1

# recovery
ph_recovery <- combined_PAMdata %>% subset(Phase=="recovery")

# customize the x-axis
custom_breaks1 <- c(15, 20, 25, 30)

pfm2 <- ggplot(ph_recovery, aes(duration, Fm)) + 
  geom_point() + facet_grid(. ~ treatments) + stat_smooth(method = "lm", col="blue") +
  background_grid(major = 'y', minor = "none") + # add thin horizontal lines 
  panel_border() + # and a border around each panel
  scale_x_continuous(breaks = custom_breaks1) +
  labs(title="Recovery phase",
       x="",
       y= "Max. fluoresc. (Fm)")
pfm2
# scraped area
# recovery
ph_recovery_scr <- combined_PAMdata %>% subset(Phase=="recovery_scraped")

pfm3 <- ggplot(ph_recovery_scr, aes(duration, Fm)) + 
  geom_point() + facet_grid(. ~ treatments) + stat_smooth(method = "lm", col="green") +
  background_grid(major = 'y', minor = "none") + # add thin horizontal lines 
  panel_border() + # and a border around each panel
  scale_x_continuous(breaks = custom_breaks1) +
  labs(title="Recovery over the scraped area",
       x="Time (days)",
       y= "Max. fluoresc. (Fm)")
pfm3

ph_recovery_scr <- combined_PAMdata %>% subset(Phase=="recovery_scraped")

pfm4 <- ggplot(ph_recovery_scr, aes(duration, F)) + 
  geom_point(aes(color="F")) + 
  facet_grid(. ~ treatments) + 
  stat_smooth(method = "lm", col="green") +
  geom_point(aes(x=duration, y=Fm, color="Fm"), size=3) +
  background_grid(major = 'y', minor = "none") + # add thin horizontal lines 
  panel_border() + # and a border around each panel
  scale_x_continuous(breaks = custom_breaks1) +
  labs(title="Recovery over the scraped area",
       x="Time (days)",
       y= "Max. fluoresc. (Fm)")
pfm4

### plot showing both F0 and Fm
# stress phase
ph_stress <- data %>% subset(Phase=="stress")
custom_breaks <- c(1, 5, 10, 15)

p1 <- ggplot(data = ph_stress, aes(x = duration, y = F0)) +
  geom_point(aes(color = "F0")) +
  geom_point(aes(x = duration, y = Fm, color = "Fm")) +
  stat_smooth(aes(x = duration, y = F0, color = "F0"), method = "lm", se = TRUE) +
  stat_smooth(aes(x = duration, y = Fm, color = "Fm"), method = "lm", se = TRUE) +
  facet_grid(. ~ treatments) +
  background_grid(major = 'y', minor = "none") + # add thin horizontal lines 
  panel_border() + # and a border around each panel
  scale_x_continuous(breaks = custom_breaks) +
  labs(x = "Time (days)", y = "F0 and Fm", color = "Legend") +
  scale_color_manual(values = c("F0" = "darkorange", "Fm" = "red")) +
  labs(title="Stressor phase",
       x="",
       y= expression(paste("Min_Max fluoresc. (F"[0], "_Fm)")))
p1
# recovery
ph_recovery <- data %>% subset(Phase=="recovery")

# customize the x-axis
custom_breaks1 <- c(15, 20, 25, 30)

p2 <- ggplot(data = ph_recovery, aes(x = duration, y = F0)) +
  geom_point(aes(color = "F0")) +
  geom_point(aes(x = duration, y = Fm, color = "Fm")) +
  stat_smooth(aes(x = duration, y = F0, color = "F0"), method = "lm", se = TRUE) +
  stat_smooth(aes(x = duration, y = Fm, color = "Fm"), method = "lm", se = TRUE) +
  facet_grid(. ~ treatments) +
  background_grid(major = 'y', minor = "none") + # add thin horizontal lines 
  panel_border() + # and a border around each panel
  scale_x_continuous(breaks = custom_breaks1) +
  labs(x = "Time (days)", y = "F0 and Fm", color = "Legend") +
  scale_color_manual(values = c("F0" = "blue", "Fm" = "green")) +
  labs(title="Recovery phase",
       x="",
       y= expression(paste("Min_Max fluoresc. (F"[0], "_Fm)")))
  
p2
# recovery over scraped surface
ph_recovery_scr <- data %>% subset(Phase=="recovery_scraped")
p3 <- ggplot(data = ph_recovery_scr, aes(x = duration, y = F0)) +
  geom_point(aes(color = "F0")) +
  geom_point(aes(x = duration, y = Fm, color = "Fm")) +
  stat_smooth(aes(x = duration, y = F0, color = "F0"), method = "lm", se = TRUE) +
  stat_smooth(aes(x = duration, y = Fm, color = "Fm"), method = "lm", se = TRUE) +
  facet_grid(. ~ treatments) +
  background_grid(major = 'y', minor = "none") + # add thin horizontal lines 
  panel_border() + # and a border around each panel
  scale_x_continuous(breaks = custom_breaks1) +
  labs(x = "Time (days)", y = "F0 and Fm", color = "Legend") +
  scale_color_manual(values = c("F0" = "blue", "Fm" = "green")) +
  labs(title="Recovery over the scraped area",
       x="Time (days)",
       y= expression(paste("Min_Max fluoresc. (F"[0], "_Fm)")))
p3

#install.packages("cowplot")
library(cowplot)
f0_fm <- plot_grid(p1, p2, p3, labels=c("A", "B", "C"), ncol = 1, nrow = 3)

f0_fm
ggsave("f0_fm.tiff", units="in", width=7, height=7, dpi=300, compression = 'lzw')

library(ggstatsplot)
set.seed(123)

ggbetweenstats(
  data  = ph_stress,
  x     = treatments,
  y     = Y.II.,
  title = "Photosynthetic biomass response to Temperature"
)
