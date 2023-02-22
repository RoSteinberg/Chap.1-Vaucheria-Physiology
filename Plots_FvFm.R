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

# ETR ---------------------------------------------------------------------

read.delim2(
  'C:/Users/rosteinb/Chap.1-Vaucheria-Physiology/data/FvFm_field.csv',
  header = TRUE,
  sep = ",",
  dec = "."
) %>%
  group_by(Month, Site, PAR) %>%
  summarise(
    temp_mean = mean(Temp, na.rm = TRUE),
    temp_sd = sd(Temp, na.rm = TRUE),
    ETR_mean = mean(ETR, na.rm = TRUE),
    ETR_sd = sd(ETR, na.rm = TRUE),
    FvFm_mean = mean(FvFm, na.rm = TRUE),
    FvFm_sd = sd(FvFm, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  group_by(Site, Month) %>%
  ggplot(aes(
    x = PAR,
    y = ETR_mean,
    color = Site,
    shape = Month
  )) +
  geom_point(size = 3) +
  scale_shape_manual(
    name = 'Month',
    values = c(16, 17),
    labels = c('July',
               'September')
  ) +
  scale_color_manual(
    name = '',
    values = c('#31a354', '#dd1c77'),
    labels = c('', ''),
    breaks = NULL
  ) +
  labs(x = '',
       y = 'ETR',
       caption = 'Figure 3: Electron transport rate of Vaucheria mats at the sampling sites Kampen and List for \n                Photosynthetic Active Radiation (PAR)') +
  theme_pubclean() +
  theme(
    legend.position = 'bottom',
    axis.text = element_text(size = 12),
    axis.title = element_text(
      family = 'sans',
      size = 16,
      face = 'bold'
    ),
    legend.title = element_text('Sampling Site'),
    axis.text.x = element_text(
      family = 'sans',
      size = 16,
      face = 'bold'
    ),
    plot.caption = element_text(
      family = 'sans',
      size = 16,
      hjust = 0
    )
  ) +
  annotate("text",
           label = "Kampen",
           x = 2000,
           y = 190) +
  annotate("text",
           label = "List",
           x = 2000,
           y = 380)

ggsave(
  filename = "figures/ETR.png",
  width = 12,
  height = 9,
  dpi = 600
)

# qN ----------------------------------------------------------------------

read.delim2(
  'C:/Users/rosteinb/Chap.1-Vaucheria-Physiology/data/FvFm_field.csv',
  header = TRUE,
  sep = ",",
  dec = "."
) %>%
  group_by(Month, Site, PAR) %>%
  summarise(
    temp_mean = mean(Temp, na.rm = TRUE),
    temp_sd = sd(Temp, na.rm = TRUE),
    ETR_mean = mean(ETR, na.rm = TRUE),
    ETR_sd = sd(ETR, na.rm = TRUE),
    FvFm_mean = mean(FvFm, na.rm = TRUE),
    FvFm_sd = sd(FvFm, na.rm = TRUE),
    qN_mean = mean(qN, na.rm = TRUE),
    qN_sd = sd(qN, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  group_by(Site, Month) %>%
  ggplot(aes(
    x = PAR,
    y = qN_mean,
    color = Site,
    shape = Month
  )) +
  geom_point(size = 3) +
  labs(x = "PAR", y = "qN") +
  theme_pubclean() +
  scale_color_brewer(palette = 'Set1')


# FvFm --------------------------------------------------------------------

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
  scale_fill_brewer(palette = 'Set1')

