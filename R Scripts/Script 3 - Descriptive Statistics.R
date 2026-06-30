# Project: Analysis of an individual conductivity meter 
# Script 3: Descriptive Statistics of Salinity and Conductivity
# Authors: Grant McKown (james.mckown@unh.edu)

# Last Updated: June 2026

#Project Description:


# Script Description: 



#Chapter 1: Package Library ------------------------------------

# Data Organization and Formatting
library(tidyverse)
library(DescTools)


# Chapter 2: Import salinity and conductivity datasets -----------------------

# Step 1: User Input of Site, Year

Site <- "Philbrook Pond"; Year = "2026"

# Step 2: Import datasets

salinity <- read.csv(paste("Formatted Datasets\\ ", Site, " - ", Year, " - Salinty psu Formatted.csv")) %>%
  select(-X) %>%
  mutate(Date.Time = as.POSIXct(Date.Time, 
                                format = '%Y-%m-%d %H:%M:%S')) %>%
  select(Date.Time, Tidal_Cycle, Garland_Brook:West_Platform) %>%
  filter(!is.na(Date.Time))

glimpse(salinity)

conductivity <- read.csv(paste("Formatted Datasets\\ ", Site, " - ", Year, " - Conductivity mS-cm Formatted.csv")) %>%
  select(-X) %>%
  mutate(Date.Time = as.POSIXct(Date.Time, 
                                format = '%Y-%m-%d %H:%M:%S')) %>%
  filter(!is.na(Date.Time))

glimpse(conductivity)

# Step 3: Identify full lunar tidal cycles
# In order to compare apples to apples for salinity and specific conductivity
# measurements, each tidal cycle should have at least 20 days of measurements.

# This designation can be changed for each user based on the last line in the code
# below. For a full lunar cycle, change the number of days threshold to 30 (~29.5)

lunar_length <- salinity %>%
  mutate(Days = day(Date.Time)) %>%
  group_by(Tidal_Cycle) %>%
  reframe(Num_Days = length(unique(Days))) %>%
  ungroup() %>%
  filter(Num_Days >= 20)
          

# Chapter 3: Salinity Stats -------------------------

# Salinity statistics are first calculated for each tidal cycle and then calculated
# for the entire monitoring dataset

# Step 1 - Calculate statistics for each day

salinity_day_stats <- salinity %>%
  # Remove lunar tidal cycles that do not meet threshold
  filter(Tidal_Cycle %in% lunar_length$Tidal_Cycle) %>%
  # Assign the Month and Day
  mutate(Month = month(Date.Time),
         Day = day(Date.Time)) %>%
  # Transform the dataset from wide to long 
  pivot_longer(Garland_Brook:West_Platform,
               names_to = "Loggers",
               values_to = "Salinity_psu") %>%
  # Calculate daily mean statistics, absolute minimum and maximum
  group_by(Loggers, Month, Day, Tidal_Cycle) %>%
  summarise(
    daily_mean_psu = round(mean(Salinity_psu, na.rm = TRUE),
                           1),
    daily_se_psu = round((sd(Salinity_psu, na.rm = TRUE) / sqrt(n())),
                         1),
    max_psu = max(Salinity_psu, na.rm = TRUE),
    min_psu = min(Salinity_psu, na.rm = TRUE)) %>%
  ungroup()

glimpse(salinity_day_stats)

# Step 2 - Calculate statistics for each lunar tidal cycle

salinity_lunar_stats <- salinity_day_stats %>%
  # Calculate daily mean statistics, absolute minimum and maximum
  group_by(Loggers, Tidal_Cycle) %>%
  summarise(
    lunar_mean_psu = round(mean(daily_mean_psu, na.rm = TRUE),
                           1),
    lunar_se_psu = round((sd(daily_mean_psu, na.rm = TRUE) / sqrt(n())),
                         1),
    lunar_max_psu = max(max_psu, na.rm = TRUE),
    lunar_min_psu = min(min_psu, na.rm = TRUE)) %>%
  ungroup()

glimpse(salinity_lunar_stats)


# Step 3 - Calculate statistics for entire study

salinity_study_stats <- salinity_lunar_stats %>%
  group_by(Loggers) %>%
  summarise(
    study_mean_psu = round(mean(lunar_mean_psu, na.rm = TRUE),
                           1),
    study_se_psu = round((sd(lunar_mean_psu, na.rm = TRUE) / sqrt(n())),
                         1),
    
    study_max_psu = max(lunar_max_psu, na.rm = TRUE),
    study_max_se_psu = round((sd(lunar_max_psu, na.rm = TRUE) / sqrt(n())),
                         1),
    
    study_min_psu = min(lunar_min_psu, na.rm = TRUE),
    study_min_se_psu = round((sd(lunar_min_psu, na.rm = TRUE) / sqrt(n())),
                         1),) %>%
  ungroup()

glimpse(salinity_study_stats)


# Step 4: Export datasets for further analysis and visualization

# Daily salinity statistics
write.csv(salinity_day_stats,
          paste("Formatted Datasets\\ ", Site, " - ", Year, "Salinity Daily Stats.csv"))

# Tidal cycle salinity statistics
write.csv(salinity_lunar_stats,
          paste("Formatted Datasets\\ ", Site, " - ", Year, "Salinity Lunar Stats.csv"))

# Study salinity statistics
write.csv(salinity_study_stats,
          paste("Formatted Datasets\\ ", Site, " - ", Year, "Salinity Study Stats.csv"))




# Chapter 4: Specific Conductivity Stats -------------------------

# Conductivity statistics are first calculated for each tidal cycle and then calculated
# for the entire monitoring dataset

# Step 1 - Calculate statistics for each day

conductivity_day_stats <- conductivity %>%
  # Remove lunar tidal cycles that do not meet threshold
  filter(Tidal_Cycle %in% lunar_length$Tidal_Cycle) %>%
  # Assign the Month and Day
  mutate(Month = month(Date.Time),
         Day = day(Date.Time)) %>%
  # Transform the dataset from wide to long 
  pivot_longer(Garland_Brook:West_Platform,
               names_to = "Loggers",
               values_to = "Conductivity_mS") %>%
  # Calculate daily mean statistics, absolute minimum and maximum
  group_by(Loggers, Month, Day, Tidal_Cycle) %>%
  summarise(
    daily_mean_mS = round(mean(Conductivity_mS, na.rm = TRUE),
                           1),
    daily_se_mS = round((sd(Conductivity_mS, na.rm = TRUE) / sqrt(n())),
                        1),
    max_mS = max(Conductivity_mS, na.rm = TRUE),
    min_mS = min(Conductivity_mS, na.rm = TRUE)) %>%
  ungroup()

glimpse(conductivity_day_stats)

# Step 2 - Calculate statistics for each lunar tidal cycle

conductivity_lunar_stats <- conductivity_day_stats %>%
  # Calculate daily mean statistics, absolute minimum and maximum
  group_by(Loggers, Tidal_Cycle) %>%
  summarise(
    lunar_mean_mS = round(mean(daily_mean_mS, na.rm = TRUE),
                           1),
    lunar_se_mS = round((sd(daily_mean_mS, na.rm = TRUE) / sqrt(n())),
                         1),
    lunar_max_mS = max(max_mS, na.rm = TRUE),
    lunar_min_mS = min(min_mS, na.rm = TRUE)) %>%
  ungroup()

glimpse(conductivity_lunar_stats)


# Step 3 - Calculate statistics for entire study

conductivity_study_stats <- conductivity_lunar_stats %>%
  group_by(Loggers) %>%
  summarise(
    study_mean_mS = round(mean(lunar_mean_mS, na.rm = TRUE),
                           1),
    study_se_mS = round((sd(lunar_mean_mS, na.rm = TRUE) / sqrt(n())),
                         1),
    
    study_max_mS = max(lunar_max_mS, na.rm = TRUE),
    study_max_se_mS = round((sd(lunar_max_mS, na.rm = TRUE) / sqrt(n())),
                             1),
    
    study_min_mS = min(lunar_min_mS, na.rm = TRUE),
    study_min_se_mS = round((sd(lunar_min_mS, na.rm = TRUE) / sqrt(n())),
                             1),) %>%
  ungroup()

glimpse(conductivity_study_stats)


# Step 4: Export datasets for further analysis and visualization

# Daily conductivity statistics
write.csv(conductivity_day_stats,
          paste("Formatted Datasets\\ ", Site, " - ", Year, "Conductivity Daily Stats.csv"))

# Tidal cycle conductivity statistics
write.csv(conductivity_lunar_stats,
          paste("Formatted Datasets\\ ", Site, " - ", Year, "Conductivity Lunar Stats.csv"))

# Study conductivity statistics
write.csv(conductivity_study_stats,
          paste("Formatted Datasets\\ ", Site, " - ", Year, "Conductivity Study Stats.csv"))





