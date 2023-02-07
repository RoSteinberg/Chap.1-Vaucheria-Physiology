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
##############################################

pigment <- 
  read.delim2('C:/Users/rosteinb/Chap.1-Vaucheria-Physiology/data/Pigments_2.csv', header=TRUE, sep=",", dec=".") %>%
  group_by(Site, Pigment)


hist(pigment$Sample.Conc..µg.)
hist(exp(pigment$Sample.Conc..µg.))

log(pigment$Sample.Conc..µg.)
hist(log(pigment$Sample.Conc..µg.))
summary(glm((Sample.Conc..µg.) ~ Site, data=pigment))


hist(pigment$Sample.Conc..µg.)
hist(log(pigment$Sample.Conc..µg.))
model <- glm(log(Sample.Conc..µg.) ~ Site*Month, data=pigment)
print(model)
shapiro.test(residuals(model))
hist(residuals(model)) #histogram
leveneTest(log(Sample.Conc..µg.) ~ Site*Pigment, data=pigment)
kruskal.test(log(Sample.Conc..µg.) ~ Month, data=pigment)

model <- glm(log(Sample.Conc..µg.) ~ site, data=pigment) # linear regression
summary(model)
shapiro.test(residuals(model)) # Test auf Normalverteilung
hist(residuals(model)) #histogram
leveneTest(log(Sample.Conc..µg.) ~ Site*Pigment, data=pigment) # Test auf Varianzhomogenitaet
aov.model <- aov(model) # ANOVA
print(summary(aov.model))                                    
posthoc <- TukeyHSD(aov.model) # Posthoc test
print(posthoc)                                               
p_sec <- data.frame(posthoc[["Year:Month"]])

kruskal.test(log(Sample.Conc..µg.) ~ Pigment, data=pigment)

