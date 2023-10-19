# MINI PAM II measurements
# Biomass estimation
# Analysis to test the response of 
# microphytobenthos biomass to salinitiy, temperature and flow velocity
# ExStream 2022
# Serge Mayombo
# 18.09.2023

library(tidyverse)

#read PAM data
data <- read.csv("ExStream2022_PAM2.csv")
head(data)
data_IMP <- data %>%
  filter(!is.na(F), System == "IMP")
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
# combine PAM dataset and metadat dataset
data_full <- left_join( data_IMP, metadata_IMP[,3:7], join_by(Channel))

data_full$Temperature <- as.factor(data_full$Temperature)
data_full$Velocity <- as.factor(data_full$Velocity)
data_full$Salinity <- as.factor(data_full$Salinity)

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
# Sample data
# Sample data
data <- data.frame(
  Time_Days = c(1, 2, 3, 4),
  Biomass_mg = c(50, 55, 52, 48),
  Temperature_C = c(25, 26, 27, 27)
)
ggplot(data_full, aes(x=duration, y=F, color=Phase)) +
  #geom_line() +
  geom_point() +
  geom_smooth(method = "lm", alpha = .15, aes(fill = Phase)) +
  labs(title="Evolution of Microphytobenthos Biomass Over Time",
       x="Time (days)",
       y="Biomass (mg)",
       color="Temperature (°C)") +
  theme_minimal()

ggplot(data_clean, aes(x = Day, y = Y.II., color = Phase)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = .15, aes(fill = Phase)) 

ggplot(data_clean, aes(x = duration, y = Y.II., color = Phase)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = .15, aes(fill = Temperature)) 


ggplot(data = data_clean, aes(x = Day, y= F, color = F)) + 
  geom_point() + facet_wrap(facets = vars(Temperaure)) +
  geom_smooth(method = "lm")

ggplot(data_clean, aes(x=duration, y=F, color=Phase)) +
  #geom_line() +
  geom_point() +
  geom_smooth(method="loess", se=TRUE) +
  labs(title="Microphytobenthos Growth Over Time",
       x="Time",
       y= expression(paste("Photosynthetic biomass (F"[0], ")"))) +
  theme_minimal()

ggplot(data_clean, aes(x=duration, y=Fm, color=Phase)) +
  #geom_line() +
  geom_point() +
  geom_smooth(method="loess", se=TRUE) +
  labs(title="Microphytobenthos Growth Over Time",
       x="Time (days)",
       y= "Photosynthetic biomass (Fm)") +
  theme_minimal()

library(ggstatsplot)
set.seed(123)

ggbetweenstats(
  data  = data_full,
  x     = Temperature,
  y     = Fm,
  title = "Photosynthetic biomass response to Temperature"
)
