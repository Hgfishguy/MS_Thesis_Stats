# Exploratory data analysis for sediment data

library(tidyverse)
library(dplyr)
library(doBy)
library(readxl)

porosity_data = read_excel('/Users/suzanneguy/R_Projects/MS_Thesis_Data_Analysis/MS_Thesis_Stats/Data/Porosity.BMFL.xlsx')
porosity_clean = na.omit(porosity_data)
# data is read and cleaned of NAs

porosity_parsed = porosity_clean %>%
  mutate(Elevation = substr(Replicate, 1, 1))
# New column created from first letter of another column


porosity_avg = porosity_parsed %>% 
  group_by(Date, Location, Elevation) %>%
  summarize(mean_porosity = mean(Porosity), sd_porosity = sd(Porosity))
porosity_avg

porosity_B3 = filter(porosity_parsed, Location == "B3")
porosity_GI = filter(porosity_parsed, Location == "GI")
porosity_A = filter(porosity_parsed, Location == "A")
porosity_B = filter(porosity_parsed, Location == "B")
porosity_D = filter(porosity_parsed, Location == "D")
porosity_E = filter(porosity_parsed, Location == "E")


B3_boxplot = ggplot(data = porosity_B3) +
  geom_boxplot(aes(y = Porosity, x = Elevation, fill = Elevation), fill = "aquamarine2") +
  geom_jitter(aes(y = Porosity, x = Elevation), width = 0.2) +
  ylab("Porosity (ml water per ml sediment") +
  facet_wrap(~Date, nrow = 1) + 
  theme_bw()
B3_boxplot
