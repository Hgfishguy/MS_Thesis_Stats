library(tidyverse)
library(dplyr)
library(doBy)
library(readxl)
library(lubridate)

Biomass_data = read_excel('/Users/suzanneguy/R_Projects/MS_Thesis_Data_Analysis/MS_Thesis_Stats/Data/BMA_Human_Impacts_Master_Datasheet.xlsx', sheet = 'Biomass')
NH4_data = read_excel('/Users/suzanneguy/R_Projects/MS_Thesis_Data_Analysis/MS_Thesis_Stats/Data/BMA_Human_Impacts_Master_Datasheet.xlsx', sheet = 'NH4')
NO3_data = read_excel('/Users/suzanneguy/R_Projects/MS_Thesis_Data_Analysis/MS_Thesis_Stats/Data/BMA_Human_Impacts_Master_Datasheet.xlsx', sheet = 'NO3')
PO4_data = read_excel('/Users/suzanneguy/R_Projects/MS_Thesis_Data_Analysis/MS_Thesis_Stats/Data/BMA_Human_Impacts_Master_Datasheet.xlsx', sheet = 'PO4')
Sediment_data = read_excel('/Users/suzanneguy/R_Projects/MS_Thesis_Data_Analysis/MS_Thesis_Stats/Data/BMA_Human_Impacts_Master_Datasheet.xlsx', sheet = 'Sediment')

Biomass_filtered = Biomass_data %>% filter(Site != c("B3", "GI"))
NH4_filtered = NH4_data %>% filter(Site != c("B3", "GI"))
NO3_filtered = NO3_data %>% filter(Site != c("B3", "GI"))
PO4_filtered = PO4_data %>% filter(Site != c("B3", "GI"))
Sediment_filtered = Sediment_data %>% filter(Site != c("B3", "GI"))

NH4_Month = NH4_filtered %>%
  mutate(Month = month(Date))

NH4_boxplot = ggplot(data = NH4_Month) +
  geom_boxplot(aes(y = Adjusted_Concentration_uM, x = Estuary, fill = Estuary), fill = "aquamarine2") +
  geom_jitter(aes(y = Adjusted_Concentration_uM, x = Estuary), width = 0.2) +
  facet_wrap(~Month, nrow = 1) + 
  ylab("NH4 Concentration (μM)") +
  theme_bw()
NH4_boxplot


