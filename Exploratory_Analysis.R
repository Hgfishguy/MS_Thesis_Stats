library(tidyverse) # swagalicous
library(dplyr) # moveing verbs
library(doBy) # lowkey idk
library(readxl) # read excel documents
library(lubridate) # messing with dates
library(nortest) # needed for Lilliefors Test
library(rstatix) # for statistical tests and summary tools
library(DescTools) # ’Dunnets test for multiple comparisons of means
library(emmeans) # for estimated marginal means and Bonferroni correction

Biomass_data = read_excel('/Users/suzanneguy/R_Projects/MS_Thesis_Data_Analysis/MS_Thesis_Stats/Data/BMA_Human_Impacts_Master_Datasheet.xlsx', sheet = 'Biomass')
NH4_data = read_excel('/Users/suzanneguy/R_Projects/MS_Thesis_Data_Analysis/MS_Thesis_Stats/Data/BMA_Human_Impacts_Master_Datasheet.xlsx', sheet = 'NH4')
NO3_data = read_excel('/Users/suzanneguy/R_Projects/MS_Thesis_Data_Analysis/MS_Thesis_Stats/Data/BMA_Human_Impacts_Master_Datasheet.xlsx', sheet = 'NO3')
PO4_data = read_excel('/Users/suzanneguy/R_Projects/MS_Thesis_Data_Analysis/MS_Thesis_Stats/Data/BMA_Human_Impacts_Master_Datasheet.xlsx', sheet = 'PO4')
Sediment_data = read_excel('/Users/suzanneguy/R_Projects/MS_Thesis_Data_Analysis/MS_Thesis_Stats/Data/BMA_Human_Impacts_Master_Datasheet.xlsx', sheet = 'Sediment')
# reading in sheets by sheet

Biomass_filtered = Biomass_data %>% filter(Site != "B3" & Site != "GI") %>%
  mutate(Month = month(Date, label = TRUE, abbr = FALSE)) %>% filter(Month != "October" & Month != "December")
NH4_filtered = NH4_data %>% filter(Site != "B3" & Site != "GI") %>%
  mutate(Month = month(Date, label = TRUE, abbr = FALSE)) %>% filter(Month != "October" & Month != "December") %>%
  mutate(Notes = ifelse(is.na(Notes), "porewater", Notes)) %>%
  filter(Notes != "Ambient")
NO3_filtered = NO3_data %>% filter(Site != "B3" & Site != "GI") %>%
  mutate(Month = month(Date, label = TRUE, abbr = FALSE)) %>% filter(Month != "October" & Month != "December") %>%
  mutate(Notes = ifelse(is.na(Notes), "porewater", Notes)) %>%
  filter(Notes != "Ambient")
PO4_filtered = PO4_data %>% filter(Site != "B3" & Site != "GI") %>%
  mutate(Month = month(Date, label = TRUE, abbr = FALSE)) %>% filter(Month != "October" & Month != "December") %>%
  mutate(Notes = ifelse(is.na(Notes), "porewater", Notes)) %>%
  filter(Notes != "Ambient")
Sediment_filtered = Sediment_data %>% filter(Site != "B3" & Site != "GI") %>%
  mutate(Month = month(Date, label = TRUE, abbr = FALSE)) %>% filter(Month != "October" & Month != "December")
# removing B3 and GI sites from dataset, as well as adding a month column (then removing oct/dec)
# For nutrient samples, NAs in Notes column are then replaced by porewater in order to filter out ambient samples
# the filter function cannot filter something out from NAs 


NH4_boxplot = ggplot(data = NH4_filtered) +
  geom_boxplot(aes(y = Adjusted_Concentration_uM, x = Estuary, fill = Estuary)) +
  scale_fill_manual(values = c("chartreuse3", "darkturquoise"), guide = "none") +
  # geom_jitter(aes(y = Adjusted_Concentration_uM, x = Estuary), width = 0.2) +
  facet_wrap(~Month, nrow = 1) + 
  ylab("NH4 Concentration (μM)") +
  theme_bw()
NH4_boxplot

NO3_boxplot = ggplot(data = NO3_filtered) +
  geom_boxplot(aes(y = Adjusted_Concentration_uM, x = Estuary, fill = Estuary)) +
  scale_fill_manual(values = c("chartreuse3", "darkturquoise"), guide = "none") +
  # geom_jitter(aes(y = Adjusted_Concentration_uM, x = Estuary), width = 0.2) +
  facet_wrap(~Month, nrow = 1) + 
  ylab("NO3 Concentration (μM)") +
  theme_bw()
NO3_boxplot

PO4_boxplot = ggplot(data = PO4_filtered) +
  geom_boxplot(aes(y = Adjusted_Concentration_uM, x = Estuary, fill = Estuary)) +
  scale_fill_manual(values = c("chartreuse3", "darkturquoise"), guide = "none") +
  # geom_jitter(aes(y = Adjusted_Concentration_uM, x = Estuary), width = 0.2) +
  facet_wrap(~Month, nrow = 1) + 
  ylab("PO4 Concentration (μM)") +
  theme_bw()
PO4_boxplot

Biomass_boxplot = ggplot(data = Biomass_filtered) +
  geom_boxplot(aes(y = `Chla (ug/g)`, x = Estuary, fill = Estuary)) +
  scale_fill_manual(values = c("chartreuse3", "darkturquoise"), guide = "none") +
  # geom_jitter(aes(y = `Chla (ug/g)`, x = Estuary), width = 0.2) +
  facet_wrap(~Month, nrow = 1) + 
  ylab("Chla Concentration (μg/g)") + 
  ylim(0,120) +
  theme_bw()
Biomass_boxplot
# plotting data! Notice that the jitter data points are masked by a comment
# they were lowkey making the graphs hard to read lol

# biomass RCB
Biomass_filtered %>%
  group_by(Month) %>%
  group_modify(~ tidy(lillie.test(.x$`Chla (ug/g)`)))
# data VERY abnormal (p << 0.05)
Biomass_filtered %>% levene_test(`Chla (ug/g)` ~ Month*Estuary, center = "mean")
# variances unequal! (p << 0.05)

RB_model = Biomass_filtered %>% anova_test(`Chla (ug/g)` ~ Month + Estuary, effect.size = "pes")
get_anova_table(RB_model) %>% p_format(digits = 3)
# both Month and Estuary seem to have an effect on Biomass (p < 0.0001)

Biomass_filtered %>% group_by(Estuary) %>%
  get_summary_stats(`Chla (ug/g)`, type = "mean_sd")
# MI mean: 13.6, sd: 12.5
# NI mean: 15.8, sd: 15.2

Biomass_aov = aov(`Chla (ug/g)` ~ Month + Estuary, data = Biomass_filtered)
summary(Biomass_aov)
# base r aov to use in further analyses (same result as rstatix RCB anova)

TukeyHSD(Biomass_aov, "Estuary")
# 4.085999 difference, p = 0.0001027

emmeans(Biomass_aov, pairwise ~ Estuary)

Biomass_filtered %>%
  emmeans_test(`Chla (ug/g)` ~ Estuary, p.adjust.method = "bonferroni", model = Biomass_aov)
# emmeans difference with bonferroni correction
# 4.04 statistic, p < 0.00001 ****

