# Vaucheria Pigments 2022 
# Ronny Steinberg
# 16.01.2023

# Load librarys ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ggpubr)
library(car)

pigments <- 
  read.delim2('C:/Users/rosteinb/Chap.1-Vaucheria-Physiology/data/Pigments.csv', header=TRUE, sep=",", dec=".") >%>
  group_by(Site, Pigment) %>% 
  # Then calculate the mean for each year and month
  summarise(mean_day = mean(Temperature...C, na.rm = TRUE), 
            # and the SD
            sd_day = sd(Temperature...C, na.rm = TRUE),
            .groups = 'drop')