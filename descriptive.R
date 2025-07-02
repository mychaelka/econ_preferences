rm(list=ls())

library(ggplot2)
library(tidyverse)

load("../DATA/PCT_data.RData")

summary_donations <- PTC_data %>%
  group_by(CO2_Treatment, High_school) %>%
  summarise(
    mean_donation = mean(CO2_Contribution, na.rm = TRUE),
    sd = sd(CO2_Contribution, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci = qt(0.975, df = n - 1) * se,  # 95% CI
    .groups = "drop"
  ) %>% 
  mutate(treatment_label = factor(CO2_Treatment, labels = c("Control", "Treatment"))) %>% 
  mutate(school_type = factor(High_school, labels = c("University", "High School")))


summary_donations %>% ggplot(aes(x=school_type, y=mean_donation, fill=treatment_label)) +
  geom_bar(stat='identity', position = position_dodge(0.6), width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = mean_donation - ci, ymax = mean_donation + ci), width = 0.2, position = position_dodge(0.6)) +
  scale_fill_manual(values = c("white", "gray50")) +
  labs(x = "School Type", y = "Mean Donation", fill = "Condition") +
  theme_minimal(base_size = 14)
