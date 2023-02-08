# Load librarys ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ggpubr)
library(car)

pigments <- 
  read.delim2('C:/Users/rosteinb/Chap.1-Vaucheria-Physiology/data/Pigments_4.csv', header=TRUE, sep=",", dec=".") %>%
  group_by(Site, Month, Perc) %>% 
  # Then calculate the mean for each pigment and site
  summarise(mean_sum = mean(Sum, na.rm = TRUE), 
            # and the SD
            mean_per = mean(Perc_val, na.rm = TRUE), 
            # and the SD
            sd_per = sd(Perc_val, na.rm = TRUE),
            .groups = 'drop')

pigment_plot <- 
  pigments %>% 
  subset(Perc == 'chla/total') %>%
  group_by(as.factor(Site), as.factor(Month), as.factor(Perc)) %>% 
  ggplot(aes(x = Month, y = mean_per, group = Site, color = Site)) + 
  # Create dots
  geom_point() +
  scale_x_discrete(limits = c('July', 'September', 'November'),
                   labels = c('July', 'September', 'November'),
  ) +
  labs(x = " ", y = "chla/total %") +
  theme_pubclean() 
pigment_plot

pigment_plot <- 
  pigments %>% 
  subset(Perc == 'vdd/total') %>%
  group_by(as.factor(Site), as.factor(Month), as.factor(Perc)) %>% 
  ggplot(aes(x = Month, y = mean_per, group = Site, color = Site)) + 
  # Create dots
  geom_point() +
  scale_x_discrete(limits = c('July', 'September', 'November'),
                   labels = c('July', 'September', 'November'),
  ) +
  labs(x = " ", y = "vdd/total %") +
  theme_pubclean() 
pigment_plot

pigment_plot <- 
  pigments %>% 
  subset(Perc == 'chl/vdd') %>%
  group_by(as.factor(Site), as.factor(Month), as.factor(Perc)) %>% 
  ggplot(aes(x = Month, y = mean_per, group = Site, color = Site)) + 
  # Create dots
  geom_point() +
  scale_x_discrete(limits = c('July', 'September', 'November'),
                   labels = c('July', 'September', 'November'),
  ) +
  labs(x = " ", y = "chl/vdd") +
  theme_pubclean() 
pigment_plot


read.delim2('C:/Users/rosteinb/Chap.1-Vaucheria-Physiology/data/Pigments.csv', header=TRUE, sep=",", dec=".") %>%
  group_by(as.factor(Site), as.factor(Pigment)) %>% 
  ggplot(aes(x = Pigment, y = Sample.Conc..µg.,color = as.factor(Site)))+
  geom_boxplot()+
  labs(x = '', y = 'Concentration (µg g)')+
  theme_pubclean()+
  scale_color_brewer(palette = 'Paired')
