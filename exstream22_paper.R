library(ggplot2)
library(dplyr)

# Sample data
set.seed(42)

df <- data.frame(
  Metabolite = rep(paste0("M", 1:20), each = 3),
  Disease = rep(c("Disease1", "Disease2", "Disease3"), 20),
  Healthy_MES = rnorm(60, mean = 10, sd = 1),
  Diseased_MES = rnorm(60, mean = 9, sd = 1.5),
  Kruskal_p_value = runif(60, 0, 0.05)
)

# Compute MES difference and filter metabolites based on given criteria
df <- df %>%
  mutate(MES_difference = Healthy_MES - Diseased_MES,
         Significance = Kruskal_p_value < (0.05 / n_distinct(Disease))) %>%
  filter(MES_difference > 0, Significance) %>%
  group_by(Disease) %>%
  top_n(5, wt = MES_difference)

# Plot
ggplot(df, aes(x = Metabolite, y = MES_difference, fill = Disease)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Top 5 Metabolites with Highest MES Differences",
    y = "MES Difference (Healthy - Diseased)",
    x = "Metabolite"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())
