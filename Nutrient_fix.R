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
install.packages("ggtext")
library(ggtext) # for custom graph font formatting 


# Reading in files
NH4_data = read_excel('Data\\BMFL_Nutrients_Master_FIXED.xlsx',
                      sheet = 'NH4 Data')
NO3_data = read_excel('Data\\BMFL_Nutrients_Master_FIXED.xlsx',
                      sheet = 'NO3 Data')
PO4_data = read_excel('Data\\BMFL_Nutrients_Master_FIXED.xlsx',
                      sheet = 'PO4 Data')

# Creating a marker for joining
NH4_marker = NH4_data %>% unite(col = "Marker", c("Sample Date", Site, Replicate, Notes), sep = "", remove = FALSE)
NO3_marker = NO3_data %>% unite(col = "Marker", c("Sample Date", Site, Replicate, Notes), sep = "", remove = FALSE)
PO4_marker = PO4_data %>% unite(col = "Marker", c("Sample Date", Site, Replicate, Notes), sep = "", remove = FALSE)

# Joining Data frames
Nutrients_combined = NH4_marker %>%
  inner_join(NO3_marker, by = "Marker") %>%
  inner_join(PO4_marker, by = "Marker") %>%
  select('Sample Date', Site, Replicate, `Adjusted NH4 Concentration (μM)`,
         `Adjusted Concentration (μM NO3)`, `Adjusted Concentration (μM PO4)`)

# exporting new DF
write_xlsx(Nutrients_combined, path = "Data/Nurient_Summary.xlsx")
