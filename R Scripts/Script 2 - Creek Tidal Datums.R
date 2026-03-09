# Project: Analysis of an individual water level recorder for salt marsh monitoring
# Script 2: Tidal Datums of Creek Water Level Recorders
# Authors: Jenny Gibson (jennifer.gibson@unhs.edu), Grant McKown (james.mckown@unh.edu)

# Last Updated: March 9th, 2026


# Script Description: 
# Tidal datums are calculated for a single water level recorder in a creek, 
# ditch, or hydrologic pathway. The mean and standard error for MLLW, MLW,
# MSL, MHW, MHHW, and maximum water elevation are calculated. MLLW and MLW are
# the elevation of the instrument in the hydrologic pathway and should not be
# used to report official tidal datums. Additionally, MSL is also a function of 
# the location of the instrument elevation. Tidal datums are compiled into a
# single table and exported. 


#Chapter 1: Package Library ------------------------------

# Data organization and formatting
library(tidyr)
library(dplyr)
library(pillar)
library(lubridate)
library(DescTools)

# Hydrology analysis
library(VulnToolkit)


#Chapter 2: Import Hydrology Dataset ----------------------

#Step 1 - User input name of Site, Year, and creek logger name (column name)

# User input data allows the program to easily retrieve the hydrology time series
# dataset from the Formatted Datasets folder without the user having to change
# the file path. The user input data will be the same in Script 3 as well

Creek_Logger <- "Creek" ; Site <- "Kents Island"; Year <- "2025"



# #Step 2 - Import and format the time series hydrology dataset

# The hydrology dataset is the subsetted dataset that reduced the monitoring period
# to one lundar cycle (30 days) in Script 1 

wlr_format <- read.csv(paste("Formatted Datasets\\", Site, Year, "WLR Formatted Dataset.csv", 
                                       collapse = "")) %>%
  # Ensure the Date.Time column is in the proper format
  mutate(Date.Time = as.POSIXct(Date.Time, format = '%Y-%m-%d %H:%M:%S')) %>%
  # Remove extra column that is created when csv files are created
  dplyr::select(-X) %>%
  # Rename the creek logger column to 'WLR' for coding purposes
  rename(WLR = Creek_Logger) %>%
  # QAQC and remove any rows that do not have a Date.Time stamp
  filter(is.na(Date.Time) == FALSE)

glimpse(wlr_format)


#Chapter 3: Identify High and Low Tides -------------------------

# To calculate tidal datums properly, the VulnToolkit identifies all minimums and 
# maximums of the water elevation within a given time period. For this analysis,
# the time period will be 12.5 hours to locate the diurnal low and high tides

# Additionally, HL function also identifies and assigns the highest and lowest
# tides per day (HH or LL, compared to H or L in the tides column)

# Step 1 - Calculate low and high tides with HL function in VulnToolkit

wlr_tides <-HL(level = wlr_format$WLR, 
               time = wlr_format$Date.Time, period = 12.5) %>%
  rename(Date.Time = time)

glimpse(wlr_tides)

# Step 2 - Quality Control

#First check if there are consecutive high or low tides in the dataset
wlr_tides_qaqc <- wlr_tides %>%
  # Calculate previous tidal stage using lag() function
  mutate(previous_tide = lag(tide, 1)) %>%
  # Remove subsequent low tide from the dataset
  filter(!(tide == "L" & previous_tide == "L")) %>%
  # Remove subsequent high tide from the dataset
  filter(!(tide == "H" & previous_tide == "H")) %>%
  # Remove potential duplicates 
  distinct(Date.Time, .keep_all = TRUE) %>%
  dplyr::select(-previous_tide)

glimpse(wlr_tides_qaqc)

# Export the low and high tide dataset for use in Script 3

write.csv(wlr_tides_qaqc,
          paste("Formatted Datasets\\", Site, Year, "Tidal Hydrology Dataset.csv", 
                collapse = ""))


#Chapter 4: Tidal Datums ---------------------------------

#Step 1 - Mean high water and mean low water (MHW & MLW)

# MHW and MLW are calculated simply as the average elevation of the identified
# high and low tides

wlr_tides_stats <- wlr_tides_qaqc %>%
  group_by(tide) %>%
  summarise(
    avg_elev = mean(level),
    se_elev = sd(level)/sqrt(n()),
    count = n()) %>%
  mutate(across(avg_elev:se_elev, ~round(., 3)))

glimpse(wlr_tides_stats)

# Step 2 - Mean higher high water (MHHW)

# MHHW is calculated based on the identified HH tides from HL function

wlr_tides_stats2 <- wlr_tides_qaqc %>%
  filter(tide2 == "HH") %>%
  summarise(
    avg_hh = mean(level),
    se_hh = sd(level)/sqrt(n()),
    count = n()) %>%
  mutate(across(avg_hh:se_hh, ~round(., 3)))

glimpse(wlr_tides_stats2)

# Step 3 - Mean lower low tide (MLLW)

# MLLW is calculated based on the identified LL tides from the HL function
# It should be noted that the MLLW in reality for most creek water level
# recorder deployments is essentially the elevation of the water level recorder

wlr_tides_stats3 <- wlr_tides_qaqc %>%
  filter(tide2 == "LL") %>%
  summarise(
    avg_ll = mean(level),
    se_ll = sd(level)/sqrt(n()),
    count = n()) %>%
  mutate(across(avg_ll:se_ll, ~round(., 3)))

glimpse(wlr_tides_stats3)


# Step 4 - Mean Sea Level (MSL) 
# MSL is calculated in this script as the mean of ALL low and high tides in the dataset, 
# and again is partly a function of the elevation of the water level recorder due to the 
# the low and lower low tides being essentially the elevation of the instrument

wlr_tides_mean <- wlr_tides_qaqc %>%
  summarise(avg_mean = mean(level),
            se_mean = sd(level)) %>%
  mutate(across(avg_mean:se_mean, ~round(., 3)))

# Step 5 - Maximum observable tide (Spring Tide)

# Spring tide is simply calculated as the maximum water elevation in the dataset

wlr_tides_max <- wlr_tides_qaqc %>%
  summarise(
    max_tide = max(level)) %>%
  mutate(max_tide = round(max_tide, 3))



#Chapter 5: Tidal Datums Table ------------------

# Tidal datums are compiled into one dataset to be exported 
# and then manually compiled into a larger table for the project 

wlr_stats <- data.frame(matrix(nrow = 1,
                               ncol = 12)) %>%
  SetNames(c("WLR", "Mean_LT", "SE_LT", "Mean_LLT", "SE_LLT", 
             "Mean_HT", "SE_HT", "Mean_HHT", "SE_HHT", "Mean_MT", "SE_MT", "Max_Tide")) %>%
  mutate(
    WLR = Creek_Logger,
    
    Mean_LT = wlr_tides_stats$avg_elev[wlr_tides_stats$tide == "L"],
    SE_LT = wlr_tides_stats$se_elev[wlr_tides_stats$tide == "L"],
    
    Mean_HT = wlr_tides_stats$avg_elev[wlr_tides_stats$tide == "H"],
    SE_HT = wlr_tides_stats$se_elev[wlr_tides_stats$tide == "H"],
    
    Mean_HHT = wlr_tides_stats2$avg_hh,
    SE_HHT = wlr_tides_stats2$se_hh,
    
    Mean_LLT = wlr_tides_stats3$avg_ll,
    SE_LLT = wlr_tides_stats3$se_ll,
    
    Mean_MT = wlr_tides_mean$avg_mean,
    SE_MT = wlr_tides_mean$se_mean,
    
    Max_Tide = wlr_tides_max$max_tide)


write.csv(wlr_stats,
          paste("Output Stats\\", Site, Year, "Creek Hydrology Stats.csv", 
                collapse = ""))


# Continue onto Script 3 for the analysis of groundwater hydrology



