# Vaucheria FvFM 2022 
# Ronny Steinberg
# 17.01.2023

# Load librarys ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ggpubr)
library(car)

FvFm <- 
  read.delim2('C:/Users/rosteinb/Chap.1-Vaucheria-Physiology/data/FvFm_field.csv', header=TRUE, sep=",", dec=".") %>%
  group_by(Month,Site,PAR) %>%
  summarise(temp_mean = mean(Temp, na.rm = TRUE), 
            temp_sd = sd(Temp, na.rm = TRUE),
            ETR_mean = mean(ETR, na.rm = TRUE), 
            ETR_sd = sd(ETR, na.rm = TRUE),
            FvFm_mean = mean(FvFm, na.rm = TRUE), 
            FvFm_sd = sd(FvFm, na.rm = TRUE),
            .groups = 'drop')  

read.delim2('C:/Users/rosteinb/Chap.1-Vaucheria-Physiology/data/FvFm_field.csv', header=TRUE, sep=",", dec=".") %>%
  group_by(Month,Site,PAR) %>%
  summarise(temp_mean = mean(Temp, na.rm = TRUE), 
            temp_sd = sd(Temp, na.rm = TRUE),
            ETR_mean = mean(ETR, na.rm = TRUE), 
            ETR_sd = sd(ETR, na.rm = TRUE),
            FvFm_mean = mean(FvFm, na.rm = TRUE), 
            FvFm_sd = sd(FvFm, na.rm = TRUE),
            .groups = 'drop') %>% 
  group_by(Site,Month) %>%
  ggplot(aes(x = PAR, y = ETR_mean, color = Site, shape = Month)) + 
  geom_point() +
  labs(x = "PAR", y = "ETR") +
  theme_pubclean() +
  scale_color_brewer(palette = 'Paired')

read.delim2('C:/Users/rosteinb/Chap.1-Vaucheria-Physiology/data/FvFm_field.csv', header=TRUE, sep=",", dec=".") %>%
  group_by(Month,Site,PAR) %>%
  summarise(temp_mean = mean(Temp, na.rm = TRUE), 
            temp_sd = sd(Temp, na.rm = TRUE),
            ETR_mean = mean(ETR, na.rm = TRUE), 
            ETR_sd = sd(ETR, na.rm = TRUE),
            FvFm_mean = mean(FvFm, na.rm = TRUE), 
            FvFm_sd = sd(FvFm, na.rm = TRUE),
            .groups = 'drop') %>% 
  group_by(Site,Month) %>%
  ggplot(aes(x = Site, y = FvFm_mean, fill = Month)) + 
  geom_bar(stat='identity',position = position_dodge(width = 1), color = 'black') +
  geom_errorbar(aes(ymin = FvFm_mean-0, ymax = FvFm_mean + FvFm_sd), width = 0.2, position = position_dodge(width = 1), color = 'black') +
  labs(x = " ", y = "Fv/Fm") +
  theme_pubclean() +
  scale_fill_brewer(palette = 'Paired')

