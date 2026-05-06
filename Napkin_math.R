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
library(ggsignif) # ggplot significance (REMEMBER TO PUT AES IN GGPLOT() COMMAND OR ELSE IT WON'T WORK)
install.packages("writexl")
library(writexl)

### Opening file

Porosity_data = read_excel('/Users/suzanneguy/R_Projects/MS_Thesis_Data_Analysis/MS_Thesis_Stats/Data/BMA_Human_Impacts_Master_Datasheet.xlsx', sheet = 'Porosity')
Sediment_data = read_excel('/Users/suzanneguy/R_Projects/MS_Thesis_Data_Analysis/MS_Thesis_Stats/Data/BMA_Human_Impacts_Master_Datasheet.xlsx', sheet = 'Sediment')

### Analysis

Sediment_filtered = Sediment_data %>% filter(Site == c("B3", "GI")) %>%
  mutate(Month = month(Date, label = TRUE, abbr = FALSE))
Porosity_filtered = Porosity_data %>% filter(Site == c("B3" , "GI")) %>%
  mutate(Month = month(Date, label = TRUE, abbr = FALSE), Replicate = substr(Replicate, 1, 1))

Sediment_filtered %>% group_by(Site, Replicate) %>%
  get_summary_stats(`>500um (%)`, type = "mean_sd") %>%
  mutate(cv = (sd/mean)*100)
# MI mean: 27.8, sd: 18.3, cv: 65.6%
# NI mean: 2.52, sd: 4.72, cv: 187%
Sediment_filtered %>% group_by(Site, Replicate) %>%
  get_summary_stats(`>63um (%)`, type = "mean_sd") %>%
  mutate(cv = (sd/mean)*100)
# MI mean: 63.4, sd: 24.4, cv: 38.4%
# NI mean: 71.6, sd: 18.3, cv 25.5%
Sediment_filtered %>% group_by(Site, Replicate) %>%
  get_summary_stats(`<63um (%)`, type = "mean_sd")%>%
  mutate(cv = (sd/mean)*100)
# MI mean: 9.49, sd: 15.0, cv: 158%
# NI mean: 26.5, sd: 16.9, cv: 63.9%

Porosity_filtered %>% group_by(Site, Replicate) %>%
  get_summary_stats(Porosity, type = "mean_sd") %>%
  mutate(cv = (sd/mean)*100)








