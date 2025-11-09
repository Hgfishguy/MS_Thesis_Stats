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
install.packages('vegan',
                 repos = c('https://vegandevs.r-universe.dev','https://cloud.r-project.org'))
library(vegan) # PERMANOVA analysis and ANOSIM
library(ggplot2)
install.packages("ggsignif")
library(ggsignif) # ggplot significance
install.packages("writexl")
library(writexl)

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
  filter(Notes != "Ambient") %>%
  mutate(Adjusted_Concentration_uM = replace(Adjusted_Concentration_uM, Adjusted_Concentration_uM == 0, 0.1))
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
  mutate(DIN_uM = Adjusted_Concentration_uM.x + Adjusted_Concentration_uM.y, Estuary = Estuary.x, Month = Month.x, Site = Site.x) %>%
  mutate(DIN_uM = replace(DIN_uM, DIN_uM == 0, 0.1))
# Dataframes joined using "Marker" index and concentrations added together
# Points below detection (equal to zero) reassigned as 0.1 for stat purposes

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
ggsave(Sediment_boxplot, filename = "Figures/Sediment_boxplot.pdf", device = "pdf", height = 5, width = 8) 

DIN_boxplot = ggplot(data = DIN_combined, aes(y = DIN_uM, x = Estuary, fill = Estuary)) +
  geom_boxplot() +
  scale_fill_manual(values = c("chartreuse3", "darkturquoise"), guide = "none") +
  # geom_jitter(aes(y = Adjusted_Concentration_uM, x = Estuary), width = 0.2) +
  # geom_signif(
   # comparisons = list(c("MI", "NI")), # Specify the groups to compare
  #  map_signif_level = TRUE, # Display significance stars (e.g., *, **, ***)
   # test = "t.test", # Or "t.test" for t-test
   # vjust = 0.5, # Adjust vertical position of the significance bar
   # tip_length = 0.01 # Adjust length of the tips of the significance bar
  # ) +
  facet_wrap(~Month, nrow = 1) + 
  ylab("DIN Concentration (μM)") +
  xlab("Estuary") +
  ylim(0,5000) +
  theme_bw()
DIN_boxplot
ggsave(DIN_boxplot, filename = "Figures/DIN_boxplot.pdf", device = "pdf", height = 5, width = 5) 


NH4_boxplot = ggplot(data = NH4_filtered, aes(y = Adjusted_Concentration_uM, x = Estuary, fill = Estuary)) +
  geom_boxplot() +
  geom_signif(
    comparisons = list(c("MI", "NI")), # Specify the groups to compare
    map_signif_level = TRUE, # Display significance stars (e.g., *, **, ***)
    test = "t.test", # Or "t.test" for t-test
    vjust = 0.5, # Adjust vertical position of the significance bar
    tip_length = 0.01 # Adjust length of the tips of the significance bar
  ) +
  scale_fill_manual(values = c("chartreuse3", "darkturquoise"), guide = "none") +
  # geom_jitter(aes(y = Adjusted_Concentration_uM, x = Estuary), width = 0.2) +
  facet_wrap(~Month, nrow = 1) + 
  ylab("NH4 Concentration (μM)") +
  ylim(0,5000) +
  theme_bw()
NH4_boxplot
ggsave(NH4_boxplot, filename = "Figures/NH4_boxplot.pdf", device = "pdf", height = 5, width = 5) 


NO3_boxplot = ggplot(data = NO3_filtered) +
  geom_boxplot(aes(y = Adjusted_Concentration_uM, x = Estuary, fill = Estuary)) +
  scale_fill_manual(values = c("chartreuse3", "darkturquoise"), guide = "none") +
  # geom_jitter(aes(y = Adjusted_Concentration_uM, x = Estuary), width = 0.2) +
  facet_wrap(~Month, nrow = 1) + 
  ylab("NO3 Concentration (μM)") +
  ylim(0,2000) +
  theme_bw()
NO3_boxplot
ggsave(NO3_boxplot, filename = "Figures/NO3_boxplot.pdf", device = "pdf", height = 5, width = 5) 


PO4_boxplot = ggplot(data = PO4_filtered, aes(y = Adjusted_Concentration_uM, x = Estuary, fill = Estuary)) +
  geom_boxplot() +
  scale_fill_manual(values = c("chartreuse3", "darkturquoise"), guide = "none") +
  geom_signif(
    comparisons = list(c("MI", "NI")), # Specify the groups to compare
    map_signif_level = TRUE, # Display significance stars (e.g., *, **, ***)
    test = "t.test", # Or "t.test" for t-test
    vjust = 0.5, # Adjust vertical position of the significance bar
    tip_length = 0.01 # Adjust length of the tips of the significance bar
  ) +
  # geom_jitter(aes(y = Adjusted_Concentration_uM, x = Estuary), width = 0.2) +
  facet_wrap(~Month, nrow = 1) + 
  ylab("PO4 Concentration (μM)") +
  theme_bw()
PO4_boxplot
ggsave(PO4_boxplot, filename = "Figures/PO4_boxplot.pdf", device = "pdf", height = 5, width = 5) 


Biomass_boxplot = ggplot(data = Biomass_filtered) +
  geom_boxplot(aes(y = `Chla (ug/g)`, x = Estuary, fill = Estuary)) +
  scale_fill_manual(values = c("chartreuse3", "darkturquoise"), guide = "none") +
  # geom_jitter(aes(y = `Chla (ug/g)`, x = Estuary), width = 0.2) +
  facet_wrap(~Month, nrow = 1) + 
  ylab("Chla Concentration (μg/g)") + 
  ylim(0,120) +
  theme_bw()
Biomass_boxplot
ggsave(Biomass_boxplot, filename = "Figures/Biomass_boxplot.pdf", device = "pdf", height = 5, width = 5) 

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


### Linear Regresion of BMA Responses


DIN_avg = DIN_combined %>%
  na.omit() %>%
  group_by(Month, Site, Estuary) %>%
  summarize(mean_DIN_uM = mean(DIN_uM), sd_DIN_uM = sd(DIN_uM))
DIN_avg

Biomass_avg = Biomass_filtered %>%
  na.omit() %>%
  group_by(Month, Site, Estuary) %>%
  summarize(mean_Chla_ug = mean(`Chla (ug/g)`), sd_Chla_ug = sd(`Chla (ug/g)`))
Biomass_avg

PO4_avg = PO4_filtered %>%
  na.omit() %>%
  group_by(Month, Site, Estuary) %>%
  summarize(mean_PO4_uM = mean(Adjusted_Concentration_uM), sd_DIN_um = sd(Adjusted_Concentration_uM))
PO4_avg

Sed_avg = Sediment_filtered %>%
  na.omit() %>%
  group_by(Month, Site, Estuary) %>%
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
  mutate(Estuary = Estuary.x) %>%
  select(-Estuary.x) %>%
  select(-Estuary.x.x) %>%
  select(-Estuary.y) %>%
  select(-Estuary.y.y) %>%
  print()

# LM_data = LM_data[-16, ]
# removing outlier data point (July SD)

# combining all data by averaging by site/month in order to get "paired' observations

# Step 1: Run all possible models

LM_data_filtered = LM_data %>%
  filter(mean_Chla_ug <= 40)
# filtering out high chla values

full_model = lm(mean_Chla_ug ~ mean_DIN_uM + mean_PO4_uM + mean_500um + mean_63um + mean_less63um, data = LM_data_filtered)
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

final_model_best <- lm(formula_best, data = LM_data_filtered) 

stepwise_summary <- summary(final_model_best)

parameter_estimates <- coef(final_model_best)

stepwise_summary
# PO4 most significant contributor, with small (<63) grain size minorly contributing

dw_stat <- dwtest(final_model_best)
cat("Durbin-Watson Statistic:", dw_stat$statistic, "\n")
# DW stat: 1.917664
ols_coll_diag(final_model_best)

resids <- residuals(final_model_best)
# Basic descriptive statistics
summary(resids)
# mean = 0.000

# Normality plot of residuals
plot(final_model_best, which = 2)
plot(final_model_best, 1) # residual vs fitted plot.
lillie.test(final_model_best$residuals)
# relatively normal residuals (p = 0.2971)

avPlots(final_model_best)

plot(LM_data_filtered$mean_Chla_ug, final_model_best$fitted.values)

adj_r2 <- summary(final_model_best)$adj.r.squared

LM_plot = ggplot(LM_data_filtered, aes(x = mean_Chla_ug, y = final_model_best$fitted.values)) + 
geom_point(color = "blue", size = 2) + # scatter points
  geom_smooth(method = "lm", se = TRUE, color = "red") + # linear fit line with 95% CI
  annotate("text",
           x = max(LM_data_filtered$mean_Chla_ug) * 0.9,
           y = max(final_model_best$fitted.values)*0.1, 
           label = paste0("r\u00B2 = ", round(adj_r2, 3)), # "\u00B2" is the notation r^2
           size = 4, # size the r-squared value is reported as
           color = "black") +
  labs(
    x = "Mean Chla (ug/g)",
    y = "Unstandardized Predicted Values",
    # title = "Actual vs Predicted Values for Chla"
  ) +
  theme_bw()
LM_plot
ggsave(LM_plot, filename = "Figures/LM_plot.pdf", device = "pdf", height = 5, width = 5) 

# Best predictors: PO4, smallest sed, DIN
# adjusted r^2 of 0.349
# with July SD point removed r^2 jumps to 0.66 !!!


### PERMANOVA for complete site comparison


# PERMANOVA_data = read_excel('/Users/suzanneguy/R_Projects/MS_Thesis_Data_Analysis/MS_Thesis_Stats/Data/BMA_Human_Impacts_Master_Datasheet.xlsx', sheet = 'PERMANOVA') %>%
  # filter(Site != "B3" & Site != "GI") %>%
 #  mutate(Month = month(Date, label = TRUE, abbr = FALSE)) %>% filter(Month != "October" & Month != "December")
# DOESN'T WORK,  KEEPING FOR NOW

perm_data = LM_data %>%
  select(-sd_Chla_ug, -sd_DIN_uM, -sd_DIN_um, -sd_500um, -sd_63um, -sd_less63um)

PERMANOVA_data = perm_data

head(PERMANOVA_data)

PERMANOVA_data$Month<-as.factor(PERMANOVA_data$Month) #Factor 1 (blocking)
PERMANOVA_data$Estuary<-as.factor(PERMANOVA_data$Estuary) #Factor 2
permanova_data <- PERMANOVA_data %>%
  select(-Estuary, -Month, -Site)
perm_dist<-vegdist(permanova_data, method = "bray")

NMDS_model <- metaMDS(permanova_data, trace = FALSE)
plot(NMDS_model, display = "sites")
scores(NMDS_model)

NMDS_model$stress
# stress = 0.141996

with(PERMANOVA_data, ordiellipse(NMDS_model, Estuary, kind = "sd", label = TRUE))
ellipse_data_treatment <- with(PERMANOVA_data, ordiellipse(NMDS_model, Estuary, kind = "sd"))

summary(ellipse_data_treatment)

dispersion<-betadisper(perm_dist, PERMANOVA_data$Estuary)

dispersion

anova(dispersion)

plot(dispersion, hull=FALSE, ellipse=TRUE)

full_model_result<-adonis2(perm_dist~ Estuary + Month, data = PERMANOVA_data, permutations =999, by = NULL) #currently set to check full model all together

full_model_result

perma_result <-adonis2(perm_dist~ Estuary + Month, data = PERMANOVA_data, permutations = 999, by= "terms") #Add interaction terms - factor1*factor2

perma_result
# Estuary weakly significant (p = 0.057)
# Month significant (p = 0.001)


### ANOSIMS IDK


Biomass_anosim_data = Biomass_filtered %>% na.omit()
Biomass_anosim = anosim(x = Biomass_anosim_data$`Chla (ug/g)`, grouping = Biomass_anosim_data$Estuary, permutations = 999, distance = "bray", strata = Biomass_anosim_data$Month)
summary(Biomass_anosim)
plot(Biomass_anosim)
# R = 0.02923, p = 0.001
# groups similar

DIN_anosim_data = DIN_combined %>% na.omit()
DIN_anosim = anosim(x = DIN_anosim_data$DIN_uM, grouping = DIN_anosim_data$Estuary, permutations = 999, distance = "bray", strata = DIN_anosim_data$Month)
summary(DIN_anosim)
plot(DIN_anosim)
# R = 0.008638, p = 0.872
# groups similar

PO4_anosim_data = PO4_filtered %>% na.omit() 
PO4_anosim = anosim(x = PO4_anosim_data$Adjusted_Concentration_uM, grouping = PO4_anosim_data$Estuary, permutations = 999, distance = "bray", strata = PO4_anosim_data$Month)
summary(PO4_anosim)
plot(PO4_anosim)
# R = 0.05296, p = 0.001
# groups similar

Sediment_anosim_data = Sediment_filtered %>% na.omit() %>%
  mutate(`>500um (%)` = replace(`>500um (%)`, `>500um (%)` == 0, 0.00001), 
         `>63um (%)` = replace(`>63um (%)`, `>63um (%)` == 0, 0.00001),
         `<63um (%)` = replace(`<63um (%)`, `<63um (%)` == 0, 0.00001))

Sand_anosim = anosim(x = Sediment_anosim_data$`>500um (%)`, grouping = Sediment_anosim_data$Estuary, permutations = 999, distance = "bray", strata = Sediment_anosim_data$Month)
summary(Sand_anosim)
plot(Sand_anosim)
# R = 0.4681, p = 0.001
# groups different
Silt_anosim = anosim(x = Sediment_anosim_data$`>63um (%)`, grouping = Sediment_anosim_data$Estuary, permutations = 999, distance = "bray", strata = Sediment_anosim_data$Month)
summary(Silt_anosim)
plot(Silt_anosim)
# R = 0.02256, p = 0.009
# groups similar
Clay_anosim = anosim(x = Sediment_anosim_data$`<63um (%)`, grouping = Sediment_anosim_data$Estuary, permutations = 999, distance = "bray", strata = Sediment_anosim_data$Month)
summary(Clay_anosim)
plot(Clay_anosim)
# R = 0.2759, p = 0.001
# groups almost different


### Friedman tests

# FM_data = LM_data %>% na.omit() %>%
  # group_by(Month, Estuary) %>%
  # summarize(mean_500um = mean(mean_500um), sd_500um = sd(sd_500um), 
            # mean_63um = mean(mean_63um), sd_63um = sd(sd_63um),
            # mean_less63um = mean(mean_less63um), sd_less63um = sd(sd_less63um),
            # mean_DIN_uM = mean(mean_DIN_uM), sd_DIN_uM = sd(sd_DIN_uM),
            # mean_Chla_ug = mean(mean_Chla_ug), sd_Chla_ug = sd(sd_Chla_ug),
           #  mean_PO4_uM = mean(mean_PO4_uM), sd_DIN_um = sd(sd_DIN_um)) %>%
  # ungroup()

FM_data = LM_data %>% na.omit() %>%
  group_by(Month, Estuary) %>%
  summarize(mean_500um = mean(mean_500um), mean_63um = mean(mean_63um),
            mean_less63um = mean(mean_less63um),
            mean_DIN_uM = mean(mean_DIN_uM),
            mean_Chla_ug = mean(mean_Chla_ug),
            mean_PO4_uM = mean(mean_PO4_uM)) %>%
  ungroup()

head(FM_data) # month and estuary aren't factors!
FM_data$Month<-as.factor(FM_data$Month) #Factor 1 (blocking)
FM_data$Estuary<-as.factor(FM_data$Estuary) #Factor 2
# didn't work, converting to df
FM_df = as.data.frame(FM_data)
head(FM_df)
FM_df$Month = factor(FM_df$Month) # factorization!
FM_df$Estuary = factor(FM_df$Estuary) # factorization!

FM_df %>% friedman_test(mean_500um ~ Estuary|Month)
# >500um composition different (p < 0.05)

FM_df %>% friedman_test(mean_63um ~ Estuary|Month)
# >63um composition not different (p = 0.655)

FM_df %>% friedman_test(mean_less63um ~ Estuary|Month)
# <63 composition not different (p = 0.180)

FM_df %>% friedman_test(mean_DIN_uM ~ Estuary|Month)
# DIN levels not different (p = 0.180)

FM_df %>% friedman_test(mean_PO4_uM ~ Estuary|Month)
# PO4 levels not different (p = 0.180)

FM_df %>% friedman_test(mean_Chla_ug ~ Estuary|Month)
# Chla levels not different (p = 0.655)

# JUNE AND AUGUST DATA MISSING FROM LM_data # FIX!!!!! # FIXED
# LM data generation seems to have removed boat sites during generation, double check naming conventions


### Exporting data to excel for chatgpt analysis


write_xlsx(Biomass_filtered, path = "Data/biomass_filtered.xlsx")
write_xlsx(PERMANOVA_data, path = "Data/PERMANOVA_data.xlsx")
write_xlsx(DIN_combined, path = "Data/DIN_combined.xlsx")


### prepping for random forest analysis in chatgpt


Biomass_forest = read_excel('/Users/suzanneguy/R_Projects/MS_Thesis_Data_Analysis/MS_Thesis_Stats/Data/BMA_Human_Impacts_Master_COPY.xlsx', sheet = 'Biomass')
DIN_forest = read_excel('/Users/suzanneguy/R_Projects/MS_Thesis_Data_Analysis/MS_Thesis_Stats/Data/BMA_Human_Impacts_Master_COPY.xlsx', sheet = 'DIN')
PO4_forest = read_excel('/Users/suzanneguy/R_Projects/MS_Thesis_Data_Analysis/MS_Thesis_Stats/Data/BMA_Human_Impacts_Master_COPY.xlsx', sheet = 'PO4')
Sediment_forest = read_excel('/Users/suzanneguy/R_Projects/MS_Thesis_Data_Analysis/MS_Thesis_Stats/Data/BMA_Human_Impacts_Master_COPY.xlsx', sheet = 'Sediment')

forest_joined = Biomass_forest %>%
  full_join(DIN_forest, by = c("Date", "Site", "Replicate")) %>%
  full_join(PO4_forest, by = c("Date", "Site", "Replicate")) %>%
  full_join(Sediment_forest, by = c("Date", "Site", "Replicate"))

forest_data = forest_joined %>%
  select(-Estuary.y, -Estuary.x.x, -Estuary.y.y, -Month)

write_xlsx(forest_data, path = "Data/forest_data.xlsx")


### CERF PLOTS


CERF_Sediment_boxplot = ggplot(data = Sediment_longer) +
  geom_boxplot(aes(y = Percent, x = Estuary, fill = `Grain Size`)) +
  scale_fill_manual(values = c("gray", "beige", "chocolate4")) +
  # facet_wrap(~Month, nrow = 1) + 
  ylab("Grain Size (%)") +
  xlab("Estuary") +
  ylim(0,100) +
  theme_bw()
CERF_Sediment_boxplot
ggsave(CERF_Sediment_boxplot, filename = "Figures/CERF_Sediment_boxplot.pdf", device = "pdf", height = 5, width = 8) 

DIN_limit = DIN_combined %>%
  filter(DIN_uM <= 5000)
CERF_DIN_boxplot = ggplot(data = DIN_limit, aes(y = DIN_uM, x = Estuary, fill = Estuary)) +
  geom_boxplot() +
  scale_fill_manual(values = c("chartreuse3", "darkturquoise"), guide = "none") +
  # geom_jitter(aes(y = Adjusted_Concentration_uM, x = Estuary), width = 0.2) +
  geom_signif(
    comparisons = list(c("MI", "NI")), # Specify the groups to compare
    map_signif_level = TRUE, # Display significance stars (e.g., *, **, ***)
    test = "t.test", # Or "t.test" for t-test
    vjust = 0.5, # Adjust vertical position of the significance bar
    tip_length = 0.01 # Adjust length of the tips of the significance bar
  ) +
  ylab("DIN Concentration (uM)") +
  xlab("Estuary") +
  # ylim(0,5000) +
  theme_bw()
CERF_DIN_boxplot
ggsave(CERF_DIN_boxplot, filename = "Figures/CERF_DIN_boxplot.pdf", device = "pdf", height = 5, width = 5) 

PO4_limit = PO4_filtered %>%
  filter(Adjusted_Concentration_uM <= 100)
CERF_PO4_boxplot = ggplot(data = PO4_limit, aes(y = Adjusted_Concentration_uM, x = Estuary, fill = Estuary)) +
  geom_boxplot() +
  scale_fill_manual(values = c("chartreuse3", "darkturquoise"), guide = "none") +
  geom_signif(
  comparisons = list(c("MI", "NI")), # Specify the groups to compare
  map_signif_level = TRUE, # Display significance stars (e.g., *, **, ***)
  test = "t.test", # Or "t.test" for t-test
  vjust = 0.5, # Adjust vertical position of the significance bar
  tip_length = 0.01 # Adjust length of the tips of the significance bar
  ) +
  # geom_jitter(aes(y = Adjusted_Concentration_uM, x = Estuary), width = 0.2) +
  # facet_wrap(~Month, nrow = 1) + 
  ylab("PO4 Concentration (uM)") +
 # ylim(0,100) +
  theme_bw()
CERF_PO4_boxplot
ggsave(CERF_PO4_boxplot, filename = "Figures/CERF_PO4_boxplot.pdf", device = "pdf", height = 5, width = 5) 

Biomass_limit = Biomass_filtered %>%
  filter(`Chla (ug/g)` <= 50)
CERF_Biomass_boxplot = ggplot(data = Biomass_limit) +
  geom_boxplot(aes(y = `Chla (ug/g)`, x = Estuary, fill = Estuary)) +
  scale_fill_manual(values = c("chartreuse3", "darkturquoise"), guide = "none") +
  # geom_jitter(aes(y = `Chla (ug/g)`, x = Estuary), width = 0.2) +
  # facet_wrap(~Month, nrow = 1) + 
  ylab("Chla Concentration (ug/g)") + 
  # ylim(0,50) +
  theme_bw()
CERF_Biomass_boxplot
ggsave(CERF_Biomass_boxplot, filename = "Figures/CERF_Biomass_boxplot.pdf", device = "pdf", height = 5, width = 5) 

