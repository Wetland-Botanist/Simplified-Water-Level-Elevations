# Project: Analysis of an individual conductivity meter 
# Script 1: Formatting and Organization of Salinity and Conductivity Datasets
# Authors: Grant McKown (james.mckown@unh.edu)

# Last Updated: June 2026

#Project Description:


# Script Description: 



#Chapter 1: Package Library ------------------------------------

# Data Organization and Formatting
library(tidyverse)
library(DescTools)


#Chapter 2: Import Salinity and Conductivity Datasets -----------------

# Step 1: User input site Name

# Year is used for file name conventions, export, and import throughout the
# R project

Site <- "Philbrook Pond"

Creek_Name <- "Garland_Brook"

# Step 2: Import and format salinity and conductivity datasets

salinity <- read.csv("Input Data\\Philbrook Pond - Salinity psu - First Deployment - 2026.csv") %>%
  # Remove miscellaneous rows that does not have a proper date
  filter(!is.na(Date.Time),
         Date.Time != "") %>%
  # Format the Date.Time column
  mutate(Date.Time = as.POSIXct(Date.Time, format = '%m/%d/%Y %H:%M'))

glimpse(salinity)

conductivity <- read.csv("Input Data\\Philbrook Pond - Cond mS - First Deployment - 2026.csv") %>%
  filter(!is.na(Date.Time),
         Date.Time != "") %>%
  mutate(Date.Time = as.POSIXct(Date.Time, format = '%m/%d/%Y %H:%M'))

glimpse(conductivity)

# Step 3: Extract Year from salinity dataset

# Year is used for file name conventions, export, and imporpt throughout the
# R project

Year <- max(Year(salinity$Date.Time))

Year

#Chapter 3: Remove "out of water" times from datasets -------------------

# If the creek water level recorder was out of water, the salinity and conductivity
# measurements would not be appropriate to include in the analysis

# Step 1: Import the Creek Water Elevation Dataset
creek_wlr <- read.csv("Input Data\\Philbrook Pond - First Deployment 2026 - June 2026.csv") %>%
  select(Date.Time, Creek_Name) %>%
  mutate(Date.Time = as.POSIXct(Date.Time, format = '%m/%d/%Y %H:%M')) %>%
  rename(Creek = Creek_Name)

# Step 2: Reduce the water elevation dataset to times out of the water

# The calculation is made that anytime the water level elevation is roughly 1 cm
# of the minimum recorded water elevation, it is most likely the sensor was out 
# of the water. 

# Step 1: Remove "out of water" readings from creek water elevation dataset
# "Out of water" readings were justified as within 12.5 cm of the minimum water elevation
# to be on the conservative side, so 2/3 of the PVC casing is underwater

creek_water <- creek_wlr %>%
  filter(Date.Time %in% salinity$Date.Time) %>%
  filter(Creek >= min(Creek) + 0.05) %>%
  rename(Creek_WLR = Creek)

# Step 2: Remove "out of water" readings from the salinity and conductivity datasets

# The "out of water" water elevation dataset is merged for QAQC review

creek_salinity_water <- salinity %>%
  select(Date.Time, Creek_Name) %>%
  rename(Creek_Salinity = Creek_Name) %>%
  filter(Date.Time %in% creek_water$Date.Time) %>%
  full_join(creek_water, by = "Date.Time")

glimpse(creek_salinity_water)

creek_conductivity_water <- conductivity %>%
  select(Date.Time, Creek_Name) %>%
  rename(Creek_Cond = Creek_Name) %>%
  filter(Date.Time %in% creek_water$Date.Time) %>%
  full_join(creek_water, by = "Date.Time")

glimpse(creek_conductivity_water)

# Step 3: Merge the out of water datasets back with the original datasets

salinity_creek <- salinity %>%
  select(-Creek_Name) %>%
  full_join(creek_salinity_water, by = "Date.Time") %>%
  rename(!!Creek_Name := Creek_Salinity) %>%
  select(-Creek_WLR)

glimpse(salinity_creek)

conductivity_creek <- conductivity %>%
  select(-Creek_Name) %>%
  full_join(creek_conductivity_water, by = "Date.Time") %>%
  rename(!!Creek_Name := Creek_Cond) %>%
  select(-Creek_WLR)

glimpse(conductivity_creek)

rm(creek_salinity_water, creek_conductivity_water, creek_water, creek_wlr)



# Chapter 4: Temperature QAQC -----------------------------

#One major QAQC check is to determine if something went haywire with the probe, which
# can easily be seen if the temperature readings went sideways. If so, the probe may 
# have been impacted, malfunctioned out of water, or other issues. 

# Step 1: Import the Temperature Dataset

temp <- read.csv("Input Data\\Philbrook Pond - Temp F - First Deployment - 2026.csv") %>%
  mutate(Date.Time = as.POSIXct(Date.Time, format = '%m/%d/%Y %H:%M'))

glimpse(temp)

# Step 2: Identify all of the time readings for each probe that seem haywire
  # For now, haywire refers to temperatures being negative as probes are only deployed
  # outside of the winter
temp <- temp %>%
  # Gather all of the columns except for Date.Time
  gather(2:ncol(.),
         key = Logger,
         value = Temperature_F) %>%
  mutate(
    Data_Check = ifelse(Temperature_F <= 0, "Flag", "Good"))

# Step 3: Remove erroneous salinity measurements

salinity_qaqc <- salinity_creek %>%
  pivot_longer(2:ncol(.),
         names_to = "Logger",
         values_to = "Salinity") %>%
  full_join(temp,
            by = c("Logger", "Date.Time")) %>%
  mutate(
    Salinity = ifelse(Data_Check == "Flag",
                      NA, Salinity)) %>%
  select(-Temperature_F, -Data_Check) %>%
  pivot_wider(
         names_from = "Logger",
         values_from = "Salinity")

glimpse(salinity_qaqc)


# Step 4: Remove erroneous conductivity measurements

conductivity_qaqc <- conductivity_creek %>%
  pivot_longer(2:ncol(.),
               names_to = "Logger",
               values_to = "Salinity") %>%
  full_join(temp,
            by = c("Logger", "Date.Time")) %>%
  mutate(
    Salinity = ifelse(Data_Check == "Flag",
                      NA, Salinity)) %>%
  select(-Temperature_F, -Data_Check) %>%
  pivot_wider(
    names_from = "Logger",
    values_from = "Salinity")

glimpse(conductivity_qaqc)

rm(temp, salinity_creek, conductivity_creek)

# Chapter 5: Lunar Tidal Cycles --------------------------

# Lunar cycles are based on a 29.5 day (708 hours). The start of the lunar cycle
# in the dataset is at the first measurement. 

# Step 1: Identify the date and time every 29.5 days (breaks for each lunar tidal cycle)
cycle_breaks <- seq(from = min(salinity_qaqc$Date.Time), 
                         to = max(salinity_qaqc$Date.Time),
                         by = "708 hour")


# Step 2: Assign a grouping variable for each lunar tidal cycle

# The findInterval function does exactly that!
salinity_final <- salinity_qaqc %>%
  mutate(Tidal_Cycle = findInterval(Date.Time, cycle_breaks))

glimpse(salinity_final)

# The findInterval function does exactly that!
conductivity_final <- conductivity_qaqc %>%
  mutate(Tidal_Cycle = findInterval(Date.Time, cycle_breaks))

glimpse(conductivity_final)


# Chapter 5: Export formatted Salinity and Conductivity Datasets

write.csv(salinity_final,
          paste("Formatted Datasets\\ ", Site, " - ", Year, " - Salinty psu Formatted.csv"))

write.csv(conductivity_final,
          paste("Formatted Datasets\\ ", Site, " - ", Year, " - Conductivity mS-cm Formatted.csv"))

          