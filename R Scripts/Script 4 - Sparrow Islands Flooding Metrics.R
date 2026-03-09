# Project: Analysis of an individual water level recorder for salt marsh monitoring
# Script 4: Flooding Metrics of Sparrow Islands
# Authors: Jenny Gibson (jennifer.gibson@unhs.edu), Grant McKown (james.mckown@unh.edu)

# Last Updated: March 9th, 2026

# Script Description:
# The flooding conditions, described by flooding frequency, flooding duration,
# and the maximum time not inundated, of constructed sparrow islands are 
# calculated for the entire monitoring time frame. The monitoring time frame,
# rather than the 30 day window, is used to capture multiple spring tide
# events for the maximum time not inundated metric. 

# Sparrow islands are piles of excavated peat from restoration activities 
# designed to provide refuge to nesting salt marsh sparrows above the Mean Higher 
# High Water elevation of the local creek

# script can also be used to evaluate flooding conditions at specific elevations
# of the salt marsh such as monitoring plots, restoration sites, etc. 

#Chapter 1: Package Library ----------------------------------------------------

#Tidyr and dplyr are great data organization packages
library(tidyr)
library(dplyr)
library(pillar)
library(purrr)
library(ggplot2)

# Data formatting and organization
library(lubridate)
library(DescTools)

# Hydrology Analaysis
library(VulnToolkit)


#Chapter 2: Import Datasets ------------------

# Task 1: User required data and names

Site <- "Kents Island" ; Year <- "2025"

Creek_Logger_Name = "Creek"


#Task 1: Import Sparrow Island Elevation dataset

#Sparrow island elevations dataset includes individual island, island elevation, island group, and 
# respective creek water level recorder

sparrow <- read.csv("Input Data\\Kents Island Sparrow Elevations 2025.csv")

glimpse(sparrow)


# Task 2: Import the Original Water Elevation dataset

# Please note the water elevation profile used in this script is the 
# Raw Water Elevations (input data in Script 1), not the formatted and 
# reduced water elevation profiles (~ 30 day length). 

# We want to know the maximum number of days throughout the monitoring timeframe. 
# Reduction to just the 30 days does not provide an accurate representation of
# maximum consecutive dry days, especially if spring tide flooding is in the 
# middle of the dataset. 

wlr <- read.csv("Input Data\\Kents Island 2025 Compiled WLRs R.csv") %>%
  mutate(Date.Time = as.POSIXct(Date.Time, format = '%m/%d/%Y %H:%M')) %>%
  rename(Creek_Logger = contains("Creek"))

glimpse(wlr)


# Chapter 3: High & Low Tide List -------------------

# High and low tides for the entire creek hydrology dataset is created, similar to
# the protocol followed in Script 2 (for the 30-day dataset)

# Task 1: Generate list of low and high tides for entire monitoring time frame

tides <- HL(level = wlr$Creek_Logger, 
            time = wlr$Date.Time, period = 12.5) %>%
  rename(Date.Time = time) %>%
  mutate(Creek_Logger = Creek_Logger_Name)

glimpse(tides)

# Task 2: Quality Control

#First check if there are consecutive high or low tides in the dataset
tides_qaqc <- tides %>%
  # Calculate previous tidal stage using lag() function
  mutate(previous_tide = lag(tide, 1)) %>%
  # Remove subsequent low tide from the dataset
  filter(!(tide == "L" & previous_tide == "L")) %>%
  # Remove subsequent high tide from the dataset
  filter(!(tide == "H" & previous_tide == "H")) %>%
  # Remove duplicates (not sure why this happened...)
  distinct(Date.Time, .keep_all = TRUE) %>%
  dplyr::select(-previous_tide)

glimpse(tides_qaqc)




# Chapter 4: Flooding Duration (%)----------------------

#First, we need to create a mega dataset where each sparrow island has a 
# respective water elevation profile by merging the water elevation profile 
# dataset (reduced to creek water logger) and the sparrow island
# elevation datasets

wlr_format <- wlr %>%
  #Select only the creek water level recorders
  dplyr::select(Date.Time, Creek_Logger) %>%
  gather(key = Creek_Logger, value = elev, 
         colnames(select(., Creek_Logger))) %>%
  mutate(Creek_Logger = "Creek") %>%
  #Combines the sparrow island dataset with the formatted creek WLR dataset, so now
  #each sparrow island is associated with a full water level elevation dataset in the monitoring period
  merge(dplyr::select(sparrow, Island, Creek_Logger, Elevation), 
        by = "Creek_Logger") %>%
  dplyr::select(Island, Elevation, Date.Time, Creek_Logger, elev) %>%
  rename(sparrow_elev = "Elevation") %>%
  arrange(Island, Date.Time)

#Second, we can now easily calculate flooding duration of each sparrow island 
# using summarise(), fld.dur functions

sparrow_flood <- wlr_format %>%
  group_by(Island) %>%
  summarise(
         island_flood = fld.dur(z = sparrow_elev[1],
                                    level = elev) * 100,
         island_elev = sparrow_elev[1]) %>%
  ungroup() %>%
  mutate(island_flood = as.numeric(island_flood),
           island_flood = round(island_flood, 1))

glimpse(sparrow_flood)


#Chapter 5: Flooding Frequency (%) --------------------

#We can do this quickly with a similar approach of the flooding duration through
# first merging the creek logger tidal hydrology (low - high tides) and sparrow island elevations
# datasets. Second, we can then use summarise and fld.frq functions to calculate high tide
# flooding frequency 

sparrow_freq <- tides %>%
  filter(tide == "H") %>%
  merge(dplyr::select(sparrow, Island, Elevation, Creek_Logger), 
        by = "Creek_Logger") %>%
  group_by(Island) %>%
  summarise(island_freq = fld.frq(z = Elevation[1], 
                                  ht = tides$level),
            island_elevation = Elevation[1]) %>%
  ungroup() %>%
  mutate(island_freq = round(island_freq, 2) * 100)

glimpse(sparrow_freq)


# Chapter 6: Maximum Dry Period (days) --------------------------

# One of the main metrics of the sparrow construction is remaining dry 
# (or not flooding) for at least several weeks to allow for sparrow fledglings 
# to hatch and retreat from flooding

#Calculate the max dry period with the diff.date() function, 
# which calculates the difference in two time periods and can be used as a 
# substitute for a for loop when processing through an entire column.

# It calculates the maximum difference between two chronological time periods 
# when the island was flooded.

sparrow_dry <- wlr_format %>%
  group_by(Island) %>%
  #Sets the first and last water elevation to 2.5 NAVD88 m to artificially bound the analysis
  # The analysis will start counting days at the first time and end the counting at the last time
  mutate(elev = ifelse(row_number() == 1, 2.5, 
                       ifelse(row_number() == nrow(.), 2.5, elev))) %>%
  #Keeps only the times that the island was flooded
  filter(elev > sparrow_elev) %>%
  #Sometimes there might be an NA inserted into the dataset, remove them
  filter(!is.na(Date.Time)) %>%
  #Calculates maximum time difference through the entire dataset
  summarise(island_dry_days = max(diff.Date(Date.Time))/ddays(1),
            island_elevation = sparrow_elev[1]) %>%
  mutate(island_dry_days = as.numeric(island_dry_days),
         island_dry_days = round(island_dry_days, 2)) %>%
  ungroup()

glimpse(sparrow_dry)


#Chapter 7: Island Flooding Metrics Table -------------

# Task 1: Compile all of the statistics and sparrow island information into one table

sparrow_stats <- sparrow %>%
  #Series of merging tables to the original sparrow island table,
  # For ease of reading, I broke up the code for each hydrology metric
  merge(select(sparrow_flood, Island, island_flood), by = "Island") %>%
  merge(select(sparrow_freq, Island, island_freq), by = "Island") %>%
  merge(select(sparrow_dry, Island, island_dry_days), by = "Island") %>%
  rename(Flooding_Duration_Percent = island_flood,
         HT_Frequency_Percent = island_freq,
         Max_Dry_Period_days = island_dry_days) %>%
  dplyr::select(Island, Island_Group, Creek_Logger, Elevation,
         Flooding_Duration_Percent, HT_Frequency_Percent, Max_Dry_Period_days)

glimpse(sparrow_stats)

#Task 2: Export the sparrow island statistics table to Excel

write.csv(sparrow_stats,
          paste("Output Stats\\", Site, Year, "Sparrow Island Hydrology Stats.csv",
                collapse = ""))


# Chapter 8: Island Group Descriptive Statistics ------------------

# For each flooding metric, the descriptive statistics (mean +/- standard error) 
# are calculated for each sparrow island group or restoration site

# Task 1: Calculate descriptive statistics 
island_stats <- sparrow_stats %>%
  group_by(Island_Group) %>%
  summarise(across(Flooding_Duration_Percent:Max_Dry_Period_days,
            list(
             mean = ~ mean(., na.rm = TRUE),
             se = ~ sd(., na.rm = TRUE) / sqrt(n())
            ))) %>%
  ungroup() %>%
  mutate(across(Flooding_Duration_Percent_mean:Max_Dry_Period_days_se,
                ~round(., 1))) %>%
  # Format the descriptive statistics for easier reporting
  mutate(
    Flooding_Duration = paste(Flooding_Duration_Percent_mean, Flooding_Duration_Percent_se,
                              sep = " +/- "),
    Flood_Frequency = paste(HT_Frequency_Percent_mean, HT_Frequency_Percent_se,
                            sep = " +/- "),
    Max_Days_Dry = paste(Max_Dry_Period_days_mean, Max_Dry_Period_days_se,
                         sep = " +/- ")) %>%
  dplyr::select(Island_Group, Flooding_Duration, Flood_Frequency, Max_Days_Dry)

glimpse(island_stats)

# Task 2: Export the Island Group table 

write.csv(island_stats,
          paste("Output Stats\\", Site, Year, "Sparrow Island Group Hydrology Stats.csv",
                collapse = ""))









