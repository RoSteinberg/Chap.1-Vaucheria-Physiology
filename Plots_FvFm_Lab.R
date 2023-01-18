# Vaucheria FvFM_Lab2022 
# Ronny Steinberg
# 17.01.2023

# Load librarys ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ggpubr)
library(car)

FvFm_Lab <- 
  read.delim2('C:/Users/rosteinb/Chap.1-Vaucheria-Physiology/data/FvFm_LabEx.csv', header=TRUE, sep=",", dec=".") %>%
  filter(Day == '7') %>%
  group_by(Temp) %>%
  summarise(FvFm_mean = mean(FvFm, na.rm = TRUE), 
            FvFm_sd = sd(FvFm, na.rm = TRUE),
            .groups = 'drop')  

read.delim2('C:/Users/rosteinb/Chap.1-Vaucheria-Physiology/data/FvFm_LabEx.csv', header=TRUE, sep=",", dec=".") %>%
  filter(Day == '7') %>%
  group_by(Temp) %>%
  summarise(FvFm_mean = mean(FvFm, na.rm = TRUE), 
            FvFm_sd = sd(FvFm, na.rm = TRUE),
            .groups = 'drop') %>%
  ggplot(aes(x = Temp, y = FvFm_mean, fill = Temp)) + 
  geom_bar(stat='identity', color = 'black') +
  geom_errorbar(aes(ymin = FvFm_mean-0, ymax = FvFm_mean + FvFm_sd), width = 0.2, position = position_dodge(width = 1), color = 'black') +
  labs(x = " ", y = "Fv/Fm") +
  theme_pubclean() +
  scale_fill_distiller(palette = 'Spectral')

