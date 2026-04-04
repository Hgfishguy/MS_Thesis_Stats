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
library(vegan) # PERMANOVA analysis and ANOSIM
library(ggplot2)
library(ggsignif) # ggplot significance (REMEMBER TO PUT AES IN GGPLOT() COMMAND OR ELSE IT WON'T WORK)
library(writexl)

### Pulling Data
Grain_data = read_excel('/Users/suzanneguy/R_Projects/MS_Thesis_Data_Analysis/MS_Thesis_Stats/Data/BMA_Human_Impacts_Master_Datasheet_UPDATED_Copy.xlsx', sheet = 'Sediment')
Porosity_data = read_excel('/Users/suzanneguy/R_Projects/MS_Thesis_Data_Analysis/MS_Thesis_Stats/Data/BMA_Human_Impacts_Master_Datasheet_UPDATED_Copy.xlsx', sheet = 'Porosity')

### Combining Porosity and Sediment data

Grain_filtered = Grain_data %>%
  mutate(Month = month(Date, label = TRUE, abbr = FALSE)) %>%
  select(-Replicate) %>%
  na.omit()

Grain_avg = Grain_filtered %>%
  group_by(Month, Site, Elevation) %>%
  summarize(mean_500um = mean(`>500um (%)`), 
            mean_63um = mean(`>63um (%)`),
            mean_less63um = mean(`<63um (%)`)) %>%
  ungroup()

Porosity_filtered = Porosity_data %>%
  mutate(Month = month(Date, label = TRUE, abbr = FALSE)) %>%
  filter(Porosity < 1)
  na.omit()

Porosity_avg = Porosity_filtered %>%
  group_by(Month, Site, Elevation) %>%
  summarize(mean_porosity = mean(Porosity)) %>%
  ungroup()

Sediment_complete_avg = Porosity_avg %>%
  left_join(Grain_avg, by = c("Month", "Site", "Elevation")) %>%
  na.omit()

### Modeling 
LM_data_filtered = Sediment_complete_avg

full_model = lm(mean_porosity ~ mean_500um + mean_63um + mean_less63um, data = LM_data_filtered)
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

formula_best <- as.formula(paste("mean_porosity ~", predictor_formula)) 

final_model_best <- lm(formula_best, data = LM_data_filtered) 
coef(final_model_best) # equation: y =  6.90026920 - 0.06090042(mean_500um) - 0.06145127(mean_63um) - 0.06744043(mean_less_63um)
bm_formula = coef(final_model_best)
bm_formula[1]
bm_equation = paste0("y = ", round(bm_formula[2], 3), "(% sand) ", round(bm_formula[3], 3), "(% silt) ", round(bm_formula[4], 3), "(% clay) ", "+", round(bm_formula[1], 3))
bm_equation

stepwise_summary <- summary(final_model_best)

parameter_estimates <- coef(final_model_best)

stepwise_summary

dw_stat <- dwtest(final_model_best)
cat("Durbin-Watson Statistic:", dw_stat$statistic, "\n")
# DW stat: 0.7833089
ols_coll_diag(final_model_best)

resids <- residuals(final_model_best)
# Basic descriptive statistics
summary(resids)
# mean = 

# Normality plot of residuals
plot(final_model_best, which = 2)
plot(final_model_best, 1) # residual vs fitted plot.
lillie.test(final_model_best$residuals)
# relatively normal residuals (p = )

avPlots(final_model_best)

plot(LM_data_filtered$mean_porosity, final_model_best$fitted.values)

adj_r2 <- summary(final_model_best)$adj.r.squared

LM_plot = ggplot(LM_data_filtered, aes(x = mean_porosity, y = final_model_best$fitted.values)) + 
  geom_point(color = "blue", size = 2) + # scatter points
  geom_smooth(method = "lm", se = TRUE, color = "red") + # linear fit line with 95% CI
  annotate("text",
           x = max(LM_data_filtered$mean_porosity) * 0.9,
           y = max(final_model_best$fitted.values)*0.1, 
           label = paste0("r\u00B2 = ", round(adj_r2, 3), "   n = ", length(LM_data_filtered$Month)), # "\u00B2" is the notation r^2
           size = 4, # size the r-squared value is reported as
           color = "black") + 
  labs(
    x = expression("Porosity (ml water/ml sed)"),
    y = "Unstandardized Predicted Values",
    title = bm_equation
  ) +
  theme_bw()
LM_plot

  
  