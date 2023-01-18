# Vaucheria Antiox 2022 
# Ronny Steinberg
# 87.01.2023

# Load librarys ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ggpubr)
library(car)

antiox <- 
  read.delim2('C:/Users/rosteinb/Chap.1-Vaucheria-Physiology/data/Antiox.csv', header=TRUE, sep=",", dec=".") %>%
  filter(Site != 'Standard') %>%
  group_by(Month, Site) %>%
  summarise(Conc_mean = mean(Conc, na.rm = TRUE), 
            Conc_sd = sd(Conc, na.rm = TRUE),
            .groups = 'drop')  

read.delim2('C:/Users/rosteinb/Chap.1-Vaucheria-Physiology/data/Antiox.csv', header=TRUE, sep=",", dec=".") %>%
  filter(Site != 'Standard') %>%
  group_by(Month, Site) %>%
  ggplot(aes(x = Month, y = Conc,color = as.factor(Site)))+
  geom_boxplot()+
  labs(x = '', y = 'TE')+
  theme_pubclean()+
  scale_color_brewer(palette = 'Set1')

read.delim2('C:/Users/rosteinb/Chap.1-Vaucheria-Physiology/data/Antiox.csv', header=TRUE, sep=",", dec=".") %>%
  filter(Site != 'Standard') %>%
  group_by(Month, Site) %>%
  ggplot(aes(x = Site, y = Conc,color = as.factor(Month)))+
  geom_boxplot()+
  labs(x = '', y = 'TE')+
  theme_pubclean()+
  scale_color_brewer(palette = 'Set1')
