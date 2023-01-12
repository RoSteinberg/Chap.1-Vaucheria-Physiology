# Sylt Temperature 2022 Statistics
# Ronny Steinberg
# 09.01.2023

# Load librarys ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ggpubr)
library(car)

Temp_Kampen <- 
  read.delim2('C:/Users/rosteinb/Chap.1-Vaucheria-Physiology/data/Temp_Kampen_2022.csv', header=TRUE, sep=",", dec=".") %>% 
  # Then create a month abbreviation column
  mutate(Month = month(Date, label = T)) %>%
  mutate(Day = day(Date)) %>%
  mutate(Year = year(Date)) %>%
  # Then group by day and months
  group_by(Year, Month, Day) %>% 
  # Then calculate the mean for each year and month
  summarise(mean_day = mean(Temperature...C, na.rm = TRUE), 
            # and the SD
            sd_day = sd(Temperature...C, na.rm = TRUE),
            .groups = 'drop') %>%
  add_column(site = 'Kampen')

Temp_List <- 
  read.delim2('C:/Users/rosteinb/Chap.1-Vaucheria-Physiology/data/Temp_List_2022.csv', header=TRUE, sep=",", dec=".") %>% 
  # Then create a month abbreviation column
  mutate(Month = month(Date, label = T)) %>%
  mutate(Day = day(Date)) %>%
  mutate(Year = year(Date)) %>%
  # Then group by day and months
  group_by(Year, Month, Day) %>% 
  # Then calculate the mean for each year and month
  summarise(mean_day = mean(Temperature...C, na.rm = TRUE), 
            # and the SD
            sd_day = sd(Temperature...C, na.rm = TRUE),
            .groups = 'drop') %>%
  add_column(site = 'List')

Temp_Sylt <- full_join(Temp_Kampen, Temp_List) #%>% 

Temp_Sylt %>%  
  ggplot(aes(x = Month, y = mean_day, color = as.factor(site)),group = as.factor(site),color = as.factor(site)) + 
  # Create dots
  geom_boxplot() +
  # Change labels
  labs(x = " ", y = "Temperature °C") +
  theme_pubclean()+
  scale_color_brewer(palette = 'Paired')


model <- lm(mean_day ~ is.factor(site), data=Temp_Sylt) # linear regression
shapiro.test(residuals(model)) # Test auf Normalverteilung
hist(residuals(model)) #histogram
leveneTest(mean_day ~ site, data=Temp_Sylt) # Test auf Varianzhomogenitaet
aov.model <- aov(model) # ANOVA
print(summary(aov.model))                                    
posthoc <- TukeyHSD(aov.model) # Posthoc test
print(posthoc)                                               
p_sec <- data.frame(posthoc[["Year:Month"]])