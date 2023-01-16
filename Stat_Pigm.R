# Vaucheria Pigments 2022 Statistic
# Ronny Steinberg
# 16.01.2023

# Load librarys ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ggpubr)
library(car)

pigment <- 
read.delim2('C:/Users/rosteinb/Chap.1-Vaucheria-Physiology/data/Pigments.csv', header=TRUE, sep=",", dec=".") %>%
  group_by(site = as.factor(Site), pigment = as.factor(Pigment))

model <- lm(Sample.Conc..µg. ~ site*pigment, data=pigment) # linear regression
shapiro.test(residuals(model)) # Test auf Normalverteilung
hist(residuals(model)) #histogram
leveneTest(Sample.Conc..µg. ~ site*pigment, data=pigment) # Test auf Varianzhomogenitaet
aov.model <- aov(model) # ANOVA
print(summary(aov.model))                                    
posthoc <- TukeyHSD(aov.model) # Posthoc test
print(posthoc)                                               
p_sec <- data.frame(posthoc[["Year:Month"]])

kruskal.test(mean_day ~ Month, data=Temp_Sylt)
