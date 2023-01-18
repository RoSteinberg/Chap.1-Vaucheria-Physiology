# Vaucheria Antiox Statistics 2022 
# Ronny Steinberg
# 18.01.2023

# Load librarys ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ggpubr)
library(car)

antiox <- 
  read.delim2('C:/Users/rosteinb/Chap.1-Vaucheria-Physiology/data/Antiox.csv', header=TRUE, sep=",", dec=".") %>%
  filter(Site != 'Standard') %>%
  group_by(Month, Site)

model <- lm(Conc ~ Site*Month, data=antiox) # linear regression
shapiro.test(residuals(model)) # Test auf Normalverteilung
hist(residuals(model)) #histogram
leveneTest(Conc ~ Site*Month, data=antiox) # Test auf Varianzhomogenitaet
aov.model <- aov(model) # ANOVA
print(summary(aov.model))                                    
posthoc <- TukeyHSD(aov.model) # Posthoc test
print(posthoc)                                               

model <- lm(Conc ~ Site, data=antiox) # linear regression
shapiro.test(residuals(model)) # Test auf Normalverteilung
hist(residuals(model)) #histogram
leveneTest(Conc ~ Site, data=antiox) # Test auf Varianzhomogenitaet
aov.model <- aov(model) # ANOVA
print(summary(aov.model))                                    
posthoc <- TukeyHSD(aov.model) # Posthoc test
print(posthoc)                                               
