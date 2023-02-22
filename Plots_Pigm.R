# Vaucheria Pigments 2022 
# Ronny Steinberg
# 16.01.2023

# Load librarys ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ggpubr)
library(car)


# Pigments_Overview_Plot --------------------------------------------------

pig_plot <-
  read.delim2(
    'C:/Users/rosteinb/Chap.1-Vaucheria-Physiology/data/Pigments_2.csv',
    header = TRUE,
    sep = ",",
    dec = "."
  ) %>%
  group_by(
    Site = as.factor(Site),
    Month = as.factor(Month),
    Pigment = as.factor(Pigment)
  ) %>%
  filter(
    Pigment %in% c(
      '1 Chlorophyll a',
      '2 Chlorophyll c2',
      '3 ß-Carotin',
      '4 Diadinoxanthin',
      '5 Diatoxanthin'
    )
  ) %>%
  summarise(
    mean_conc = mean(Sample.Conc..µg., na.rm = TRUE),
    sd_conc = sd(Sample.Conc..µg., na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  ggplot(aes(
    x = Month,
    y = mean_conc,
    alpha = Site,
    fill = Pigment
  )) +
  geom_bar(
    stat = 'identity',
    position = position_dodge(width = .9),
    color = 'black',
    width = .65
  ) +
  geom_errorbar(
    aes(ymin = mean_conc - 0, ymax = mean_conc + sd_conc),
    position = position_dodge(width = .9),
    color = 'black',
    width = .65
  ) +
  geom_segment(aes(
    x = 1,
    y = -25,
    xend = 1,
    yend = 500
  ),
  size = .8,
  linetype = 'longdash') +
  geom_segment(aes(
    x = 2,
    y = -25,
    xend = 2,
    yend = 500
  ),
  size = .8,
  linetype = 'longdash') +
  geom_segment(aes(
    x = 3,
    y = -25,
    xend = 3,
    yend = 500
  ),
  size = .8,
  linetype = 'longdash') +
  scale_fill_manual(
    values = c('#31a354', '#a1d99b', '#d95f0e', '#dd1c77', '#c994c7'),
    labels = c(
      'Chlorophyll a',
      'Chlorophyll c2',
      'ß-Carotin',
      'Diadinoxanthin',
      'Diatoxanthin'
    )
  ) +
  scale_x_discrete(
    limits = c('July', 'September', 'November'),
    labels = c('July', 'September', 'November'),
  ) +
  scale_alpha_manual(values = c('Kampen' = 1, 'List' = 0.8),
                     guide = 'none') +
  labs(x = " ",
       y = expression(paste(
         'Concentration ',
         sep = '  ',
         '(',
         'µg',
         sep = ' ',
         g ^ {
           -1
         },
         ')'
       )),
       caption = 'Figure 1: Pigment concentration of Vaucheria mats for July, September and November for the sampling sites \n               Kampen and List') +
  theme_pubclean() +
  theme(
    legend.position = 'bottom',
    axis.text = element_text(size = 12),
    axis.title = element_text(
      family = 'sans',
      size = 16,
      face = 'bold'
    ),
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
           x = 0.775,
           y = -15) +
  annotate("text",
           label = "List",
           x = 1.225,
           y = -15) +
  annotate("text",
           label = "Kampen",
           x = 1.775,
           y = -15) +
  annotate("text",
           label = "List",
           x = 2.225,
           y = -15) +
  annotate("text",
           label = "Kampen",
           x = 2.775,
           y = -15) +
  annotate("text",
           label = "List",
           x = 3.225,
           y = -15)

ggsave(
  filename = "figures/Pigment_overview.png",
  width = 12,
  height = 9,
  dpi = 600
)


# Ratio_Accessory_Xanthophyll --------------------------------------------

pig_ratio <-
  read.delim2(
    'C:/Users/rosteinb/Chap.1-Vaucheria-Physiology/data/Pigments_2.csv',
    header = TRUE,
    sep = ",",
    dec = "."
  ) %>%
   filter(
    Pigment %in% c(
      '1 Chlorophyll a',
      '2 Chlorophyll c2',
      '3 ß-Carotin',
      '4 Diadinoxanthin',
      '5 Diatoxanthin'
    )
  ) %>%
 group_by(
    Site,
    Month,
    Pig_group,
    Rep
  ) %>% 
  mutate(Sum_Pig = sum(Sample.Conc..µg.)) %>% 
  summarise(sum = sum(Sample.Conc..µg.),
            .groups = 'drop') %>%
  group_by(
    Site,
    Month,
    Pig_group,
  ) %>% mutate(mean = mean(sum),
                  .groups = 'drop')
#%>%
  group_by(
    Site,
    Month,
  ) %>% summarise(ratio = mean(sum),
                  .groups = 'drop')

