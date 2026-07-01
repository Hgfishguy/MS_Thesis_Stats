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
NH4_data = read_excel('Data\BMFL_Nutrients_Master_FIXED.xlsx',
                      sheet = 'NH4 Data')

