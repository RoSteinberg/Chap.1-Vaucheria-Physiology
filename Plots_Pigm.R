# Vaucheria Pigments 2022 
# Ronny Steinberg
# 16.01.2023

# Load librarys ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ggpubr)
library(car)

pigments <- 
  read.delim2('C:/Users/rosteinb/Chap.1-Vaucheria-Physiology/data/Pigments_2.csv', header=TRUE, sep=",", dec=".") %>%
  group_by(Site = as.factor(Site), Month = as.factor(Month), Pigment = as.factor(Pigment)) %>% 
  # Then calculate the mean for each pigment and site
  summarise(mean_conc = mean(Sample.Conc..µg., na.rm = TRUE), 
            # and the SD
            sd_conc = sd(Sample.Conc..µg., na.rm = TRUE),
            .groups = 'drop')

pigment_plot <- 
  pigments %>% 
  ggplot(aes(x = Month, y = mean_conc, group = Site, alpha = Site, fill = Pigment)) + 
  # Create dots
  geom_bar(stat='identity', position = position_dodge2(width = 1)) +
  geom_errorbar(aes(ymin = mean_conc-0, ymax = mean_conc + sd_conc), width = 0.9, position = position_dodge2(width = 1), color = 'black') +
  scale_x_discrete(limits = c('July', 'September', 'November'),
                   labels = c('July', 'September', 'November'),
                   ) +
  scale_alpha_manual(values = c('Kampen'=0.7,'List'= 1),
                     guide = 'none')+
  scale_fill_brewer(palette = 'Set1',
                    limits = c('Chlorophyll a', 'Chlorophyll c2', 'ß-Carotin', 'Diadinoxanthin', 
                               'Diatoxanthin', 'Violaxanthin', 'Antheraxanthin', 'Zeaxanthin'),
                    labels = c('Chlorophyll a', 'Chlorophyll c2', 'ß-Carotin', 'Diadinoxanthin', 
                               'Diatoxanthin', 'Violaxanthin', 'Antheraxanthin', 'Zeaxanthin')
                    )+
  labs(x = " ", y = "Concentration (µg g)") +
  theme_pubclean() 
pigment_plot


read.delim2('C:/Users/rosteinb/Chap.1-Vaucheria-Physiology/data/Pigments.csv', header=TRUE, sep=",", dec=".") %>%
  group_by(as.factor(Site), as.factor(Pigment)) %>% 
  ggplot(aes(x = Pigment, y = Sample.Conc..µg.,color = as.factor(Site)))+
  geom_boxplot()+
  labs(x = '', y = 'Concentration (µg g)')+
  theme_pubclean()+
  scale_color_brewer(palette = 'Paired')
 