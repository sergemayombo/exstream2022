# SFB RESIST
# Project A03: Microphytobentos communities
# data analysis for a contribution in STOTEN special issue of RESIST
# 01.09.2023
# Serge Mayombo

# load libraries

library(tidyverse)

# read data
dat <- read.csv("PAM2022_full.csv")

# change factors in the treatments column

dat$Temperature[dat$Temperature == "normal"] <- "control"
dat$treatments[dat$treatments == "NBN"] <- "NBC"
dat$treatments[dat$treatments == "NSN"] <- "NSC"
dat$treatments[dat$treatments == "RBN"] <- "RBC"
dat$treatments[dat$treatments == "RSN"] <- "RSC"
############
# correct the error in treatment assignment
# change salinity treatment in channels C07 and C08 using dplyr 

dat <- dat %>%
  mutate(Salinity = ifelse(Channel == "C07", "salt", Salinity))

dat <- dat %>%
  mutate(treatments = ifelse(Channel == "C07", "NSI", treatments))

dat <- dat %>%
  mutate(Salinity = ifelse(Channel == "C08", "background", Salinity))

dat <- dat %>%
  mutate(treatments = ifelse(Channel == "C08", "RBI", treatments))


##########
# Change of initial treatment codes to make easy for the reader 
dat$treatments[dat$treatments == "NBC"] <- "C"
dat$treatments[dat$treatments == "RBC"] <- "F"
dat$treatments[dat$treatments == "NSC"] <- "S"
dat$treatments[dat$treatments == "NBI"] <- "T"
dat$treatments[dat$treatments == "RSC"] <- "F+S"
dat$treatments[dat$treatments == "RBI"] <- "F+T"
dat$treatments[dat$treatments == "NSI"] <- "S+T"
dat$treatments[dat$treatments == "RSI"] <- "F+S+T"


#########
# change data type of some variables
dat$Temperature <- factor(dat$Temperature)
dat$Salinity <- factor(dat$Salinity)
dat$Velocity <- factor(dat$Velocity)
dat$treatments <- factor(dat$treatments)
#reorder the factors in treatments column
#dat <- dat %>%
 # mutate(treatments = fct_relevel(treatments,"NBC","RBC","NSC","NBI",
  #                                    "RSC","RBI","NSI","RSI"))
#######
# EDM
dat$treatments <- factor(dat$treatments, 
                              levels = c("C","F","S","T",
                                         "F+S","F+T","S+T","F+S+T"))

############
#select only last day measurement of each of the phases
# Use separate to split the Date column into two columns based on "T"
dat1 <- separate(dat, Date, into = c("sampling_day", "sampling_time"), sep = "T")
dna_data2 <- dna_data1[,-1]
lastday_pam <- subset(dat1, sampling_day == "2022-04-05"| sampling_day == "2022-04-19")
lastday_pam1 <- subset(lastday_pam, Phase == "stress"| Phase == "recovery")
# Summary statistics
# grouping par treatment
summary_lastdaypam <- lastday_pam1 %>%
  group_by(treatments) %>% 
  #select("Phase", "Velocity", "Salinity","Temperature", "treatments") %>% 
  summarise(
    Mean_f0 = mean(F, na.rm = TRUE),
    Mean_Y.II = mean(Y.II., na.rm = TRUE)
  )

# Correct the salt treatment of channels C07 & C08 since they were exchanged
#selected_observation <- "C07"
#selected_observation1 <- "C08"
# Use subsetting to select all cells associated with the observation
# Then assign new values to those cells
#dat[dat$Channel == selected_observation, "Salinity"] <- "salt"
#dat[dat$Channel == selected_observation, "treatments"] <- "NSI"
#dat[dat$Channel == selected_observation1, "Salinity"] <- "background"
#dat[dat$Channel == selected_observation1, "treatments"] <- "RBI"
# View the modified data frame
print(dat)
#dat$duration <- factor(dat$duration)
# filter scraper area during recovery phase
dat_pam <- dat %>% filter(!Phase == "recovery_scraped")
recovery <- dat %>% filter(Phase == "recovery_scraped")
# set  throshold value for the maximum quantum yield of photosystem II (YII)
pam_clean <- dat %>% filter(Y.II. >= 0.08)

# Combine datasets
combined_PAMdata <- rbind(pam_clean, recovery)
PAMdata <- combined_PAMdata %>% 
  filter(Phase == "stress"|Phase == "recovery")
# save as .csv
#write.csv(combined_PAMdata, "PAM2022_IMPclean.csv")
# Group by 'Site_name' and calculate mean and standard deviation
summary_statsPAM <- PAMdata %>%
  group_by(Channel) %>% 
  #select("Phase", "Velocity", "Salinity","Temperature", "treatments") %>% 
  summarise(
    Mean_F = mean(F, na.rm = TRUE),
    Mean_Fm = mean(Fm, na.rm = TRUE),
    Mean_Y.II. = mean(Y.II., na.rm = TRUE),
  )
# filter by phase and group by treatment
# stress
PAM_stress <- combined_PAMdata %>% 
  filter(Phase == "stress")
# grouping par treatment
summary_PAMstress <- PAM_stress %>%
  group_by(treatments) %>% 
  #select("Phase", "Velocity", "Salinity","Temperature", "treatments") %>% 
  summarise(
    Mean_F = mean(F, na.rm = TRUE),
    SD_F = sd(F, na.rm = TRUE),
    Mean_Fm = mean(Fm, na.rm = TRUE),
    SD_Fm = sd(Fm, na.rm = TRUE),
    Mean_Y.II. = mean(Y.II., na.rm = TRUE),
    SD_PSII = sd(Y.II., na.rm = TRUE)
  )
write.csv(summary_PAMstress, "PAM_stress.csv")
# recovery
PAM_rec <- combined_PAMdata %>% 
  filter(Phase == "recovery")
# grouping par treatment
summary_PAMrec <- PAM_rec %>%
  group_by(treatments) %>% 
  #select("Phase", "Velocity", "Salinity","Temperature", "treatments") %>% 
  summarise(
    Mean_F = mean(F, na.rm = TRUE),
    SD_F = sd(F, na.rm = TRUE),
    Mean_Fm = mean(Fm, na.rm = TRUE),
    SD_Fm = sd(Fm, na.rm = TRUE),
    Mean_Y.II. = mean(Y.II., na.rm = TRUE),
    SD_PSII = sd(Y.II., na.rm = TRUE)
  )
write.csv(summary_PAMrec, "PAM_recovery.csv")
# combine summary_statsPAM and tecan_full
chloro <- left_join(summary_statsPAM, tecan_full)
library(cowplot)
#library("devtools")
#devtools::install_github("smin95/smplot2")

library(smplot2)
ggplot(data = chloro, mapping = aes(x = standardized_fluorescence, y = Mean_F)) +
  geom_point(shape = 21, fill = '#0f993d', color = 'white', size = 3) + 
  geom_line()
  

# 1st check the order the channels
#summary_statsPAM%>% rownames
#spp.hell.alg %>% rownames
#treats %>% rownames
#spp.hell.alg.scr %>% rownames
# order the channels
#meta1 <- meta1[order(row.names(meta1)),]
#dna_data4 <- dna_data4[order(row.names(dna_data4)),]

# ploz correlation between PAM chlorophyll and extracted tecan chlorophyll


#ggplot(dat, aes(x = Day, y = F, color = treatments)) + geom_point() + geom_smooth()

#ggplot(dat, aes(x = Day, y = Y.II., group = Phase, color = treatments)) + geom_point() + geom_smooth(method = "lm")

# change in yield during stressor phase:
m1.y <- lm(Y.II. ~ Day, data = dat, subset = Phase == "stress")
summary(m1.y)
coefficients(m1.y)
# significant, but very minor increase 

# trend in F0 during stressor phase:
m1.f <- lm(F ~Day, data = dat, subset = Phase == "stress")
summary(m1.f)
# F0 decreases during stressor phase

1.896 * 0.00001
# split by treatments:
m2.f <- lm(F ~ Day * (Velocity + Salinity + Temperature), data = dat, subset = Phase == "stress")
summary(m2.f)
# significant decrease in Y.2 over stressor phase
# no difference either in averages or trends between main treatments
m2.f.a <- lm(F ~ Day + Velocity + Salinity + Temperature, data = dat, subset = Phase == "stress")
summary(m2.f.a)
# ANCOVA with common slopes shows strong negative effect of increased T
AIC(m2.f, m2.f.a)
# common slopes the better model

#ggplot(data = filter(dat, Phase == "stress"), aes(x = jitter(Day), y = F, shape = Velocity, size = Temperature, color = Salinity)) + geom_point()

# yield:
m2.y <- lm(Y.II. ~ Day * (Velocity + Salinity + Temperature), data = dat, subset = Phase == "stress")
summary(m2.y)
# higher T or lower velocity --> slight increase in yield
m2.y.a <- lm(Y.II. ~ Day + Velocity + Salinity + Temperature, data = dat, subset = Phase == "stress")
summary(m2.y.a)
# with common slopes model, reduced velocity seems to have the strongest (negative) effect
# Y(II) stress
y2.stress <- dat %>% subset(Phase =="stress")
m2.y.b <- lm(Y.II. ~ Salinity + Day * (Velocity + Temperature), data = dat, subset = Phase == "stress")
summary(m2.y.b)
# full dataset
m2.y.b <- lm(Y.II. ~ Salinity + Day * (Velocity + Temperature), data = y2.stress)
summary(m2.y.b)
# Get the standardized coefficients
# Load the effectsize package
library(effectsize)
standardize_parameters(m2.y.b)

# slight, but significant positive trend with time
# reduced velocity slight negative, increased T slight positive effect on slope
AIC(m2.y, m2.y.a, m2.y.b)
# model b looks best by AIC

#ggplot(data = filter(dat, Phase == "stress"), aes(x = jitter(Day), y = Y.II., F, shape = Velocity, color = Temperature)) + geom_point() +
 # geom_smooth(method = "lm")


##### during recovery phase:
# split by treatments:
m2.f.r <- lm(F ~ Day + (Velocity + Salinity + Temperature), data = dat, subset = Phase == "recovery")
summary(m2.f.r)

m2.f.r1 <- lm(F ~ Day * (Velocity + Salinity + Temperature), data = dat, subset = Phase == "recovery")
summary(m2.f.r1)
AIC(m2.f.r, m2.f.r1)
# increase in F0 during recovery phase; lower levels at increased T; lower increase under reduced velocity
#ggplot(data = filter(dat, Phase == "recovery"), aes(x = jitter(Day), y = Y.II., shape = Velocity, size = Temperature, color = Salinity)) + geom_point()

# yield:
m2.y.r <- lm(Y.II. ~ Day * (Velocity + Salinity + Temperature), data = dat, subset = Phase == "recovery")
summary(m2.y.r)
# slight positive trend, no differences
#ggplot(data = filter(dat, Phase == "recovery"), aes(x = jitter(Day), y = Y.II., F, shape = Velocity, size = Temperature, color = Salinity)) + geom_point()

##### recovery phase, clean surface:
# split by treatments:
m2.f.s <- lm(F ~ Day * (Velocity + Salinity + Temperature), data = dat, subset = Phase == "recovery_scraped")
summary(m2.f.s)

# strong positive trend, no differences among treatments
#ggplot(data = filter(dat, Phase == "recovery_scraped"), aes(x = jitter(Day), y = Y.II., shape = Velocity, size = Temperature, color = Salinity)) + geom_point()

# yield:
m2.y.s <- lm(Y.II. ~ Day * (Velocity + Salinity + Temperature), data = dat, subset = Phase == "recovery_scraped")
summary(m2.y.s)
# slight positive trend, slightly stronger under salt; no other differences
#ggplot(data = filter(dat, Phase == "recovery_scraped"), aes(x = jitter(Day), y = Y.II., F, shape = Velocity, size = Temperature, color = Salinity)) + geom_point()

####
#### recovery phase: is there a difference between colonized and free surfaces?
##### recovery phase, clean surface:
# split by treatments:
m3.f <- lm(F ~ Phase + Day * (Velocity + Salinity + Temperature), data = dat, subset = (Phase == "recovery_scraped" | Phase == "recovery"))
summary(m3.f)
# slight positive trend, scraped starts much lower, low velocity slightly faster decrease than rest
m3.f.a <- lm(F ~ Phase * Day * (Velocity + Salinity + Temperature), data = dat, subset = (Phase == "recovery_scraped" | Phase == "recovery"))
summary(m3.f.a)
# scraped starts almost by zero (makes sense); no overall trend for colonized surfaces, positive trend for scraped, with little heterogeneity
AIC(m3.f, m3.f.a)
# the second one better by AIC

# yield:
m3.y <- lm(Y.II. ~ Phase + Day * (Velocity + Salinity + Temperature) , data = dat, subset = (Phase == "recovery_scraped" | Phase == "recovery"))
summary(m3.y)
# slight positive trend, slightly stronger under salt; no other differences
m3.y.a <- lm(Y.II. ~ Phase * Day * (Velocity + Salinity + Temperature) , data = dat, subset = (Phase == "recovery_scraped" | Phase == "recovery"))
summary(m3.y.a)
# slight positive trend overall; no heterogeneity
AIC(m3.y, m3.y.a)
# the simpler mode here slightly better by AIC, but basically both say the same

#ggplot(data = filter(dat, Phase == "recovery_scraped"), aes(x = jitter(Day), y = Y.II., F, shape = Velocity, size = Temperature, color = Salinity)) + geom_point()

##### Looking at final days of stressor / recovery phase only:
lastday.stress <- max(dat$Day[dat$Phase == "stress"])
lastday.recovery <- max(dat$Day[dat$Phase == "recovery"])

dat.lastdays <- dat[dat$Day == lastday.stress | dat$Day == lastday.recovery,]
dat.lastdays$Phase <- factor(dat.lastdays$Phase, levels = c("stress", "recovery", "recovery_scraped"))
view(dat.lastdays)
# F0:
ml.f <- lm(F ~ Phase, dat = dat.lastdays)
summary(ml.f)
# final day F0 (~ photosynth. biomass) increases during recovery, but increase on scraped surface much stronger

# Y:
ml.y <- lm(Y.II. ~ Phase, dat = dat.lastdays)
summary(ml.y)
# final day yield (~ photosynth. health) ok even at end of stressor phase; but increases during recovery, 
# increase on scraped surface much stronger

## split by treatments:
# F0 at end of stressor phase:
ml.f.s <- lm(F ~ Salinity + Temperature + Velocity, dat = dat.lastdays, subset = Phase == "stress")
summary(ml.f.s)
# T increase has a negative effect

ml.f.s.i <- lm(F ~ Salinity * Temperature * Velocity, dat = dat.lastdays, subset = Phase == "stress")
summary(ml.f.s.i)
# no nothing with interactions

# F0 at end of recovery phase:
ml.f.r <- lm(F ~ Salinity + Temperature + Velocity, dat = dat.lastdays, subset = Phase == "recovery")
summary(ml.f.r)
# T increase has a slight negative effect

# F0 at end of recovery phase for scraped surfaces:
ml.f.rs <- lm(F ~ Salinity + Temperature + Velocity, dat = dat.lastdays, subset = Phase == "recovery_scraped")
summary(ml.f.rs)
# no differences (makes sense)

# F0 at end of recovery phase for scraped vs. colonized surfaces:
ml.f.rsc <- lm(F ~ Phase * (Salinity + Temperature + Velocity), dat = dat.lastdays, subset = Phase != "stress")
summary(ml.f.rsc)
# apart from seen T effect, no significant heterogeneity

# so we could look at effect of scraping on its own:
ml.f.scr.o <- lm(F ~ Phase, dat = dat.lastdays, subset = Phase != "stress")
ml.f.scr.o <- lm(F ~ treatments, dat = dat.lastdays, subset = Phase != "stress")
summary(ml.f.scr.o)
# overall, the (also above ssen) slight positive effect of scraping --> freshly recolonized surfaces appear to have a slightly "stronger"
# (more photosynthetic biomass + health) biofilm

# Y at end of stressor phase:
ml.y.s <- lm(Y.II. ~ Salinity + Temperature + Velocity, dat = dat.lastdays, subset = Phase == "stress")
summary(ml.y.s)
# low velocity  has a slight  negative effect; more or less no heterogeneity at all

# comparison of last day fluorescence levels
anova_lastday <- aov(F ~ Phase, data = dat.lastdays)
# Print the ANOVA table
summary(anova_lastday)
# posthoc test
install.packages("agricolae")
library(agricolae)
# Perform post hoc test (Tukey's HSD)
tukey_result <- TukeyHSD(anova_lastday)
tukey_result
# comparison of last day fluorescence levels
anova_lastday <- aov(Y.II. ~ Phase, data = dat.lastdays)
# Print the ANOVA table
summary(anova_lastday)
# posthoc test
#install.packages("agricolae")
library(agricolae)
# Perform post hoc test (Tukey's HSD)
tukey_result <- TukeyHSD(anova_lastday)
tukey_result

# plot correlation between Fo and tecan standardized_fluorescence
library("ggpubr")
# discard near 0 values
# set  threshold value for the maximum quantum yield of photosystem II (YII)
lastday_pam2 <- lastday_pam1 %>% filter(Y.II. >= 0.09)

lastday_pam2 <- lastday_pam2[,11:15]
# combine the new created lastday_pam2 dataset with tecan_full
fluoresc <- left_join(tecan_full, lastday_pam2)
str(fluoresc)
# Log transform specific columns (e.g., columns B and C)
fluoresc_log <- fluoresc %>% 
  mutate(
    log_standardized_fluorescence = log(standardized_fluorescence),
    log_Minimum_F0 = log(F)
  )
# stressor phase
fluoresc_stress <- subset(fluoresc, Phase == "stress")


tecan_pam_stress <- ggscatter(fluoresc_stress, x = "standardized_fluorescence", y = "F", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "standardized_fluorescence", ylab = "Minimum fluorescence (F0)")
tecan_pam_stress
# recovery phase
fluoresc_rec <- subset(fluoresc, Phase == "recovery")

tecan_pam_rec <- ggscatter(fluoresc_rec, x = "standardized_fluorescence", y = "F", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "standardized_fluorescence", ylab = "Minimum_fluorescence (F0)")                                                              
## plot with statistical results
library(ggstatsplot)

tecan_pam_rec <- ggscatterstats(
  data = fluoresc_rec,
  x = log_standardized_fluorescence,
  y = log_Minimum_F0,
  bf.message = FALSE,
  marginal = FALSE # remove histograms
)
library(ggpubr)
tecan_pam <- ggarrange(tecan_pam_stress, tecan_pam_rec, 
                                      ncol=2, labels = c("A", "B"))
tecan_pam

ggsave("tecan_pam.tiff", units="in", width=9, height=5, dpi=600, compression = 'lzw')
