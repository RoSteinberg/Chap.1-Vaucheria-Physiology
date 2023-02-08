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
  group_by(Site, Month, Pigment) %>% 
  # Then calculate the mean for each pigment and site
  summarise(mean_conc = mean(Sample.Conc..µg., na.rm = TRUE), 
            # and the SD
            sd_conc = sd(Sample.Conc..µg., na.rm = TRUE),
            .groups = 'drop')

pigment_plot <- 
  pigments %>% 
  group_by(as.factor(Site), as.factor(Month),as.factor(Pigment)) %>% 
  ggplot(aes(x = Month, y = mean_conc, group = Site, color = Site, fill = as.factor(Pigment))) + 
  # Create dots
  geom_bar(stat='identity', position = position_dodge2(width = 1)) +
  geom_errorbar(aes(ymin = mean_conc-0, ymax = mean_conc + sd_conc), width = 0.9, position = position_dodge2(width = 1), color = 'black') +
  scale_x_discrete(limits = c('July', 'September', 'November'),
                   labels = c('July', 'September', 'November'),
                   ) +
  scale_color_manual(values = c('Kampen'='red','List'= 'blue'))+
  scale_fill_brewer(palette = 'Set1')+
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
 