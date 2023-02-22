# Sylt Temperature 2022 Plots
# Ronny Steinberg
# 09.01.2023

# Load librarys ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ggpubr)

# Temperature Difference ------------------------------------------------------

read.delim2('C:/Users/rosteinb/Chap.1-Vaucheria-Physiology/data/Temp_Kampen_2022.csv', header=TRUE, sep=",", dec=".") %>%
  ggplot(aes(x = Date, y = Temperature...C)) + 
  # Create dots
  geom_point()+
  labs(x = " ", y = "Temperature °C") +
  theme_pubclean()

#Temp_Kampen <- 
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
  unite(Year, Month, Day, col = 'Day', sep = '-') %>%
  # Begin ggplot
  ggplot(aes(x = Day, y = mean_day)) + 
  # Create dots
  geom_point() +
  # Change labels
  labs(x = " ", y = "Temperature °C") +
  theme_pubclean()

#Temp_Kampen <- 
  read.delim2('C:/Users/rosteinb/Chap.1-Vaucheria-Physiology/data/Temp_Kampen_2022.csv', header=TRUE, sep=",", dec=".") %>% 
  # Then create a month abbreviation column
  mutate(Month = month(Date, label = T)) %>%
  mutate(Day = day(Date)) %>%
  mutate(Year = year(Date)) %>%
  # Then group by day and months
  group_by(Day, Month, Year) %>% 
  # Then calculate the mean for each year and month
  summarise(mean_day = mean(Temperature...C, na.rm = TRUE), 
            # and the SD
            sd_day = sd(Temperature...C, na.rm = TRUE),
            .groups = 'drop') %>%
  # Begin ggplot
  ggplot(aes(x = Month, y = mean_day)) + 
  # Create dots
  geom_boxplot() +
  # Change labels
  labs(x = " ", y = "Temperature °C") +
  theme_pubclean()

#Temp_Kampen <- 
  read.delim2('C:/Users/rosteinb/Chap.1-Vaucheria-Physiology/data/Temp_Kampen_2022.csv', header=TRUE, sep=",", dec=".") %>% 
    # Then create a month abbreviation column
    mutate(Month = month(Date, label = T)) %>%
    # Then group by day and months
    group_by(Month) %>% 
    # Then calculate the mean for each year and month
    summarise(mean_month = mean(Temperature...C, na.rm = TRUE), 
              # and the SD
              sd_month = sd(Temperature...C, na.rm = TRUE),
              .groups = 'drop') %>%
    # Begin ggplot
    ggplot(aes(x = Month, y = mean_month)) + 
    # Create dots
    geom_point() +
    geom_errorbar(aes(ymin = mean_month - sd_month, ymax = mean_month + sd_month )) +
    # Change labels
    labs(x = " ", y = "Temperature (°C)") +
    theme_pubclean()
  
#Temp_Kampen <- 
  read.delim2('C:/Users/rosteinb/Chap.1-Vaucheria-Physiology/data/Temp_List_2022.csv', header=TRUE, sep=",", dec=".") %>% 
    # Then create a month abbreviation column
    mutate(Month = month(Date, label = T)) %>%
    mutate(Day = day(Date)) %>%
    mutate(Year = year(Date)) %>%
    # Then group by day and months
    group_by(Day, Month, Year) %>% 
    # Then calculate the mean for each year and month
    summarise(mean_day = mean(Temperature...C, na.rm = TRUE), 
              # and the SD
              sd_day = sd(Temperature...C, na.rm = TRUE),
              .groups = 'drop') %>%
    # Begin ggplot
    ggplot(aes(x = Month, y = mean_day)) + 
    # Create dots
    geom_boxplot() +
    # Change labels
    labs(x = " ", y = "Temperature °C") +
    theme_pubclean()

# Temperature_Comparison_List/Kampen ---------------------------------------

  Temp_Kampen <-
    read.delim2(
      'C:/Users/rosteinb/Chap.1-Vaucheria-Physiology/data/Temp_Kampen_2022.csv',
      header = TRUE,
      sep = ",",
      dec = "."
    ) %>%
    # Then create a month abbreviation column
    mutate(Month = month(Date, label = T)) %>%
    mutate(Day = day(Date)) %>%
    mutate(Year = year(Date)) %>%
    # Then group by day and months
    group_by(Year, Month, Day) %>%
    # Then calculate the mean for each year and month
    summarise(
      mean_day = mean(Temperature...C, na.rm = TRUE),
      # and the SD
      sd_day = sd(Temperature...C, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    add_column(site = 'Kampen')
  
  Temp_List <-
    read.delim2(
      'C:/Users/rosteinb/Chap.1-Vaucheria-Physiology/data/Temp_List_2022.csv',
      header = TRUE,
      sep = ",",
      dec = "."
    ) %>%
    # Then create a month abbreviation column
    mutate(Month = month(Date, label = T)) %>%
    mutate(Day = day(Date)) %>%
    mutate(Year = year(Date)) %>%
    group_by(Year, Month, Day) %>%
    summarise(
      mean_day = mean(Temperature...C, na.rm = TRUE),
      sd_day = sd(Temperature...C, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    add_column(site = 'List')
  
  Temp_Sylt <- full_join(Temp_Kampen, Temp_List)
  
  Temp_Sylt %>%
    ggplot(aes(x = Month, y = mean_day, fill = site),
           group = site) +
    geom_boxplot() +
    scale_fill_manual(
      name = 'Sampling Sites',
      values = c('#31a354', '#dd1c77'),
      labels = c('Kampen',
                 'List')
    ) +
    labs(x = '',
         y = 'Temperature (°C)',
         caption = 'Figure 2: Temperature in 2022 from June to November in the Wadden Sea at the sampling sites Kampen\n                and List. The data for June starts at 15th and the data for November ends at 15th') +
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
    )
  
  ggsave(
    filename = "figures/Temperature.png",
    width = 12,
    height = 9,
    dpi = 600
  )
  


