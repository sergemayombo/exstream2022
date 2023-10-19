library(ggplot2)
library(dplyr)
dat <- read.csv("PAM2022_full.csv")
# filter scraper area duringrecovery phase
dat_pam <- dat %>% filter(!Phase == "recovery_scraped")
pam_clean <- dat %>% filter(F >= 200)

dat$Temperature[dat$Temperature == "normal"] <- "control"
dat$treatments[dat$treatments == "NBN"] <- "NBC"
dat$treatments[dat$treatments == "NSN"] <- "NSC"
dat$treatments[dat$treatments == "RBN"] <- "RBC"
dat$treatments[dat$treatments == "RSN"] <- "RSC"
dat$Temperature <- factor(dat$Temperature)
dat$Salinity <- factor(dat$Salinity)
dat$Velocity <- factor(dat$Velocity)

dat_pam <- dat %>% filter(F >= 100)

ggplot(dat, aes(x = Day, y = F, color = treatments)) + geom_point() + geom_smooth()

ggplot(dat, aes(x = Day, y = Y.II., group = Phase, color = treatments)) + geom_point() + geom_smooth(method = "lm")

# change in yield during stressor phase:
m1.y <- lm(Y.II. ~ Day, data = dat, subset = Phase == "stress")
summary(m1.y)
# significant, but very minor increase 

# trend in F0 during stressor phase:
m1.f <- lm(F ~ Day, data = dat, subset = Phase == "stress")
summary(m1.f)
# F0 decreases during stressor phase

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

ggplot(data = filter(dat, Phase == "stress"), aes(x = jitter(Day), y = F, shape = Velocity, size = Temperature, color = Salinity)) + geom_point()

# yield:
m2.y <- lm(Y.II. ~ Day * (Velocity + Salinity + Temperature), data = dat, subset = Phase == "stress")
summary(m2.y)
# higher T or lower velocity --> slight increase in yield
m2.y.a <- lm(Y.II. ~ Day + Velocity + Salinity + Temperature, data = dat, subset = Phase == "stress")
summary(m2.y.a)
# with common slopes model, reduced velocity seems to have the strongest (negative) effect
m2.y.b <- lm(Y.II. ~ Salinity + Day * (Velocity + Temperature), data = dat, subset = Phase == "stress")
summary(m2.y.b)
# slight, but significant positive trend with time
# reduced velocity slight negative, increased T slight positive effect on slope
AIC(m2.y, m2.y.a, m2.y.b)
# model b looks best by AIC

ggplot(data = filter(dat, Phase == "stress"), aes(x = jitter(Day), y = Y.II., F, shape = Velocity, color = Temperature)) + geom_point() +
  geom_smooth(method = "lm")


##### during recovery phase:
# split by treatments:
m2.f.r <- lm(F ~ Day * (Velocity + Salinity + Temperature), data = dat, subset = Phase == "recovery")
summary(m2.f.r)
# increase in F0 during recovery phase; lower levels at increased T; lower increase under reduced velocity
ggplot(data = filter(dat, Phase == "recovery"), aes(x = jitter(Day), y = Y.II., shape = Velocity, size = Temperature, color = Salinity)) + geom_point()

# yield:
m2.y.r <- lm(Y.II. ~ Day * (Velocity + Salinity + Temperature), data = dat, subset = Phase == "recovery")
summary(m2.y.r)
# slight positive trend, no differences
ggplot(data = filter(dat, Phase == "recovery"), aes(x = jitter(Day), y = Y.II., F, shape = Velocity, size = Temperature, color = Salinity)) + geom_point()

##### recovery phase, clean surface:
# split by treatments:
m2.f.s <- lm(F ~ Day * (Velocity + Salinity + Temperature), data = dat, subset = Phase == "recovery_scraped")
summary(m2.f.s)
# strong positive trend, no differences among treatments
ggplot(data = filter(dat, Phase == "recovery_scraped"), aes(x = jitter(Day), y = Y.II., shape = Velocity, size = Temperature, color = Salinity)) + geom_point()

# yield:
m2.y.s <- lm(Y.II. ~ Day * (Velocity + Salinity + Temperature), data = dat, subset = Phase == "recovery_scraped")
summary(m2.y.s)
# slight positive trend, slightly stronger under salt; no other differences
ggplot(data = filter(dat, Phase == "recovery_scraped"), aes(x = jitter(Day), y = Y.II., F, shape = Velocity, size = Temperature, color = Salinity)) + geom_point()

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

ggplot(data = filter(dat, Phase == "recovery_scraped"), aes(x = jitter(Day), y = Y.II., F, shape = Velocity, size = Temperature, color = Salinity)) + geom_point()

##### Looking at final days of stressor / recovery phase only:
lastday.stress <- max(dat$Day[dat$Phase == "stress"])
lastday.recovery <- max(dat$Day[dat$Phase == "recovery"])

dat.lastdays <- dat[dat$Day == lastday.stress | dat$Day == lastday.recovery,]
dat.lastdays$Phase <- factor(dat.lastdays$Phase, levels = c("stress", "recovery", "recovery_scraped"))

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
summary(ml.f.scr.o)
# overall, the (also above ssen) slight positive effect of scraping --> freshly recolonized surfaces appear to have a slightly "stronger"
# (more photosynthetic biomass + health) biofilm

# Y at end of stressor phase:
ml.y.s <- lm(Y.II. ~ Salinity + Temperature + Velocity, dat = dat.lastdays, subset = Phase == "stress")
summary(ml.y.s)
# low velocity  has a slight  negative effect; more or less no heterogeneity at all
