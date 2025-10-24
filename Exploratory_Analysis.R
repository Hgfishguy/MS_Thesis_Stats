library(tidyverse) # swagalicous
library(dplyr) # moving verbs
library(doBy) # lowkey idk
library(readxl) # read excel documents
library(lubridate) # messing with dates
library(nortest) # needed for Lilliefors Test
library(rstatix) # for statistical tests and summary tools
library(DescTools) # ’Dunnets test for multiple comparisons of means
library(emmeans) # for estimated marginal means and Bonferroni correction
library(lmtest) # For Durbin-Watson statistic
library(olsrr) # For Model selection based on R squared
library(car) # for Partial Regression plots


### READING AND FILTERING


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

# Creating a new parameter, DIN, by adding NO3 and NH4 concentrations together
NH4_united = NH4_filtered %>% unite(col = "Marker", c(Date, Estuary, Site, Notes, Replicate), sep = "", remove = FALSE)
NO3_united = NO3_filtered %>% unite(col = "Marker", c(Date, Estuary, Site, Notes, Replicate), sep = "", remove = FALSE)
# First, an index column ("Marker") must be created in order to properly join datasets

DIN_combined = NH4_united %>%
  inner_join(NO3_united, by = "Marker") %>%
  mutate(DIN_uM = Adjusted_Concentration_uM.x + Adjusted_Concentration_uM.y, Estuary = Estuary.x, Month = Month.x, Site = Site.x) 
# Dataframes joined using "Marker" index and concentrations added together

# Sediment data changed in order to be plottet (facors reordered as well)
Sediment_longer = Sediment_filtered %>%
  pivot_longer(cols = c(`>500um (%)`, `>63um (%)`, `<63um (%)`), names_to = "Grain Size", values_to = "Percent") %>%
  mutate(`Grain Size` = fct_relevel(`Grain Size`, ">500um (%)", ">63um (%)", "<63um (%)")) %>%
  na.omit()


### VISUALIZING


Sediment_boxplot = ggplot(data = Sediment_longer) +
  geom_boxplot(aes(y = Percent, x = Estuary, fill = `Grain Size`)) +
  scale_fill_manual(values = c("gray", "beige", "chocolate4")) +
  facet_wrap(~Month, nrow = 1) + 
  ylab("Grain Size (%)") +
  xlab("Estuary") +
  ylim(0,100) +
  theme_bw()
Sediment_boxplot

DIN_boxplot = ggplot(data = DIN_combined) +
  geom_boxplot(aes(y = DIN_uM, x = Estuary, fill = Estuary)) +
  scale_fill_manual(values = c("chartreuse3", "darkturquoise"), guide = "none") +
  # geom_jitter(aes(y = Adjusted_Concentration_uM, x = Estuary), width = 0.2) +
  facet_wrap(~Month, nrow = 1) + 
  ylab("DIN Concentration (μM)") +
  xlab("Estuary") +
  ylim(0,5000) +
  theme_bw()
DIN_boxplot

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


### STATISTICAL ANALYSIS


# biomass RCB
Biomass_filtered %>%
  group_by(Month) %>%
  group_modify(~ tidy(lillie.test(.x$`Chla (ug/g)`)))
# data VERY abnormal (p << 0.05)
Biomass_filtered %>% levene_test(`Chla (ug/g)` ~ Month*Estuary, center = "mean")
# variances unequal! (p << 0.05)

Biomass_RB_model = Biomass_filtered %>% anova_test(`Chla (ug/g)` ~ Month + Estuary, effect.size = "pes")
get_anova_table(Biomass_RB_model) %>% p_format(digits = 3)
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


# DIN RCB
DIN_combined %>%
  group_by(Month) %>%
  group_modify(~ tidy(lillie.test(.x$DIN_uM)))
# haha wow that is abnormal!

DIN_combined %>% levene_test(DIN_uM ~ Month*Estuary, center = "mean")
# variances unequal! (p << 0.05)

DIN_RB_model = DIN_combined %>% anova_test(DIN_uM ~ Month + Estuary, effect.size = "pes")
get_anova_table(DIN_RB_model) %>% p_format(digits = 3)
# Only month seemed to have a significant affect on DIN

DIN_combined %>% group_by(Estuary) %>%
  get_summary_stats(DIN_uM, type = "mean_sd")
# MI mean: 1186, sd: 1090
# NI mean: 1201, sd: 1415

# PO4 RCB
PO4_filtered %>%
  group_by(Month) %>%
  group_modify(~ tidy(lillie.test(.x$Adjusted_Concentration_uM)))
# haha wow that is still abnormal!

PO4_filtered %>% levene_test(Adjusted_Concentration_uM ~ Month*Estuary, center = "mean")
# variances still unequal! (p <<<<< 0.05)

PO4_RB_model = PO4_filtered %>% anova_test(Adjusted_Concentration_uM ~ Month + Estuary, effect.size = "pes")
get_anova_table(PO4_RB_model) %>% p_format(digits = 3)
# Both Estuary and Month seemed to have a significant effect on PO4 (p << 0.05)

PO4_filtered %>% group_by(Estuary) %>%
  get_summary_stats(Adjusted_Concentration_uM, type = "mean_sd")
# MI mean: 42.3, sd: 63.7
# NI mean: 8.95, sd: 21.9

# Sediment RCBs

# >500 um
Sediment_filtered %>%
  group_by(Month) %>%
  group_modify(~ tidy(lillie.test(.x$`>500um (%)`)))
# abnormal 

# >63 um
Sediment_filtered %>%
  group_by(Month) %>%
  group_modify(~ tidy(lillie.test(.x$`>63um (%)`)))
# slightly abnormal? Depends on month

# >63 um
Sediment_filtered %>%
  group_by(Month) %>%
  group_modify(~ tidy(lillie.test(.x$`<63um (%)`)))
# also slightly abnormal, different months

Sediment_filtered %>% levene_test(`>500um (%)` ~ Month*Estuary, center = "mean")
# variances unequal (p < 0.05)
Sediment_filtered %>% levene_test(`>63um (%)` ~ Month*Estuary, center = "mean")
# variances unequal (p < 0.05)
Sediment_filtered %>% levene_test(`<63um (%)` ~ Month*Estuary, center = "mean")
# variances unequal (p < 0.05)

Sand_RB_model = Sediment_filtered %>% anova_test(`>500um (%)` ~ Month + Estuary, effect.size = "pes")
get_anova_table(Sand_RB_model) %>% p_format(digits = 3)
# Both Estuary and Month seemed to have a significant effect on sand composition (p << 0.05)
Silt_RB_model = Sediment_filtered %>% anova_test(`>63um (%)` ~ Month + Estuary, effect.size = "pes")
get_anova_table(Silt_RB_model) %>% p_format(digits = 3)
# Both Estuary and Month seemed to have a significant effect on silt composition (p << 0.05)
Clay_RB_model = Sediment_filtered %>% anova_test(`<63um (%)` ~ Month + Estuary, effect.size = "pes")
get_anova_table(Clay_RB_model) %>% p_format(digits = 3)
# Both Estuary and Month seemed to have a significant effect on clay composition (p << 0.05)

Sediment_filtered %>% group_by(Estuary) %>%
  get_summary_stats(`>500um (%)`, type = "mean_sd")
# MI mean: 27.8, sd: 18.3 
# NI mean: 2.52, sd: 4.72
Sediment_filtered %>% group_by(Estuary) %>%
  get_summary_stats(`>63um (%)`, type = "mean_sd")
# MI mean: 63.4, sd: 24.4
# NI mean: 71.6, sd: 18.3
Sediment_filtered %>% group_by(Estuary) %>%
  get_summary_stats(`<63um (%)`, type = "mean_sd")
# MI mean: 9.49, sd: 15.0
# NI mean: 26.5, sd: 16.9


### Linear Modeling BMA Responses

DIN_avg = DIN_combined %>%
  na.omit() %>%
  group_by(Month, Site) %>%
  summarize(mean_DIN_uM = mean(DIN_uM), sd_DIN_uM = sd(DIN_uM))
DIN_avg

Biomass_avg = Biomass_filtered %>%
  na.omit() %>%
  group_by(Month, Site) %>%
  summarize(mean_Chla_ug = mean(`Chla (ug/g)`), sd_Chla_ug = sd(`Chla (ug/g)`)) %>%
Biomass_avg

PO4_avg = PO4_filtered %>%
  na.omit() %>%
  group_by(Month, Site) %>%
  summarize(mean_PO4_uM = mean(Adjusted_Concentration_uM), sd_DIN_um = sd(Adjusted_Concentration_uM))
PO4_avg

Sed_avg = Sediment_filtered %>%
  na.omit() %>%
  group_by(Month, Site) %>%
  summarize(mean_500um = mean(`>500um (%)`), sd_500um = sd(`>500um (%)`), 
            mean_63um = mean(`>63um (%)`), sd_63um = sd(`>63um (%)`),
            mean_less63um = mean(`<63um (%)`), sd_less63um = sd(`<63um (%)`))
Sed_avg

LM_data = Biomass_avg %>% 
  left_join(DIN_avg, by = c("Month", "Site")) %>%
  left_join(PO4_avg, by = c("Month", "Site")) %>%
  left_join(Sed_avg, by = c("Month", "Site")) %>%
  na.omit() %>%
  ungroup() %>%
  print()
# combining all data by averaging by site/month in order to get "paired' observations

# Step 1: Run all possible models
full_model = lm(mean_Chla_ug ~ mean_DIN_uM + mean_PO4_uM + mean_500um + mean_63um + mean_less63um, data = LM_data)
all_models <- ols_step_all_possible(full_model)
all_models

all_models_df <- all_models$result

best_model_info <- all_models_df %>%
  arrange(desc(adjr)) %>%
  slice(1)
best_model_info

selected_predictors <- best_model_info$predictors[[1]]

selected_predictors_split <- unlist(strsplit(selected_predictors, " "))

predictor_formula <- paste(selected_predictors_split, collapse = " + ")

formula_best <- as.formula(paste("mean_Chla_ug ~", predictor_formula)) 

final_model_best <- lm(formula_best, data = LM_data) 

stepwise_summary <- summary(final_model_best)

parameter_estimates <- coef(final_model_best)

stepwise_summary
# PO4 most significant contributor, with small (<63) grain size minorly contributing

dw_stat <- dwtest(final_model_best)
cat("Durbin-Watson Statistic:", dw_stat$statistic, "\n")
# DW stat: 2.12
ols_coll_diag(final_model_best)

resids <- residuals(final_model_best)
# Basic descriptive statistics
summary(resids)
# mean = 0.000

# Normality plot of residuals
plot(final_model_best, which = 2)
plot(final_model_best, 1) # residual vs fitted plot.
lillie.test(final_model_best$residuals)
# abnormal residuals (p < 0.05)

avPlots(final_model_best)

plot(LM_data$mean_Chla_ug, final_model_best$fitted.values)

adj_r2 <- summary(final_model_best)$adj.r.squared

LM_plot = ggplot(LM_data, aes(x = mean_Chla_ug, y = final_model_best$fitted.values)) + 
geom_point(color = "blue", size = 2) + # scatter points
  geom_smooth(method = "lm", se = TRUE, color = "red") + # linear fit line with 95% CI
  annotate("text",
           x = max(LM_data$mean_Chla_ug) * 0.9,
           y = max(final_model_best$fitted.values)*0.1, 
           label = paste0("r\u00B2 = ", round(adj_r2, 3)), # "\u00B2" is the notation r^2
           size = 4, # size the r-squared value is reported as
           color = "black") +
  labs(
    x = "Mean Chla (ug/g)",
    y = "Unstandardized Predicted Values",
    title = "Actual vs Predicted Values for Chla"
  ) +
  theme_bw()
LM_plot
