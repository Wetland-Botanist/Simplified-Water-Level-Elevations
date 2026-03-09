# Project: Analysis of an individual water level recorder for salt marsh monitoring
# Script 3: Flooding Metrics of Groundwater Wells and Pool Instruments
# Authors: Jenny Gibson (jennifer.gibson@unhs.edu), Grant McKown (james.mckown@unh.edu)

# Last Updated: March 9th, 2026


# Script Description:
# Script 3 calculates flooding metrics for for groundwater table and/or pool
# surface water dynamics including flooding frequency (%), flooding duration (%),
# mean unsaturated zone depth (cm), and maximum unsaturated zone depth (cm). The
# script outputs a single table with all flooding metrics rounded to 1 decimal place.

# Metrics are based on the marsh platform elevation representative of conditions
# surrounding the groundwater well or pool instrument and is supplied by the user
# in the "Marsh Platform Elevation" dataset. 

#Chapter 1: Package Library -------------------

#Data organization and formatting
library(tidyr)
library(dplyr)
library(lubridate)
library(DescTools)

#Hydrology Analysis
library(VulnToolkit)


#Chapter 2: Import Datasets ----------------------

#Step 1 - User input name of Site, Year, and creek logger name (column name)

# User input data allows the program to easily retrieve the hydrology time series
# dataset from the Formatted Datasets folder without the user having to change
# the file path. The user input data are the same in Script 2 as well

# Logger name = water level recorder column header
# Creek logger = creek water level recorder column header

Logger_Name <- "NAC" ; Creek_Logger <- "Creek" ; Site <- "Kents Island" ; Year <- "2025"



# Step 2 - Import hydrology time series dataset

wlr_format <- read.csv(paste("Formatted Datasets\\", Site, Year, "WLR Formatted Dataset.csv", 
                      collapse = "")) %>%
  # Format the Date.Time column
  mutate(Date.Time = as.POSIXct(Date.Time, format = '%Y-%m-%d %H:%M:%S')) %>%
  # Remove miscellaneous empty column that is created when dataset is exported to csv
  dplyr::select(-X) %>%
  # Rename the groundwater hydrology water level recorder to WLR for coding purposes
  rename(WLR = Logger_Name) %>%
  # Subset the dataset to just the Date/Time and the water elevations of the water logger in question
  dplyr::select(Date.Time, WLR) %>%
  # QAQC and remove any empty rows
  filter(!is.na(WLR),
         !is.na(Date.Time))

glimpse(wlr_format)

# Step 3 - Import the low and high tide time series dataset of the creek waterlevel recorder

creek_tides <- read.csv(paste("Formatted Datasets\\", Site, Year, "Tidal Hydrology Dataset.csv", 
                        collapse = "")) %>%
  select(-X) %>%
  mutate(Date.Time = as.POSIXct(Date.Time, format = '%Y-%m-%d %H:%M:%S'))

glimpse(creek_tides)



# Step 4 - Import the elevation dataset of marsh platform and root zone

# User needs to create a csv file of the elevations of the marsh platform and root zone
# related to the groundwater level recorders in the site. Each row will be consist
# of the water logger name, marsh platform elevation, and root zone elevation (5 cm belowground)

# Elevation dataset is needed to compute flooding frequency, drainage duration, and
# drainage depth

elevs <- read.csv("Input Data\\Kents Island WLR Elevations Example.csv") %>%
  # Reduce the elevation dataset to just the groundwater level recorder in question
  filter(WLR == Logger_Name)

glimpse(elevs)



#Chapter 3: Flooding Duration (%) --------------------

# Flooding duration is the amount of time the marsh platform and root zone are 
# submerged based on the groundwater water level recorder. Flooding duration
# is reported as the number of hours and percentage of monitoring duration

#To accomplish using the fld.dur() function, which returns the flooding duration (as a percent of monitoring time)

# Step 1 - Calculate the flooding duration based on elevation dataset

wlr_flood <- wlr_format %>%
  # Calculate flooding duration
  summarise(marsh_flood = fld.dur(z = elevs$Marsh_Elevation,
                                  level = WLR) * 100,
            # State the marsh elevation for posterity
            marsh_elev = elevs$Marsh_Elevation) %>%
  ungroup() %>%
  # Round the flooding durations to 1 decimal place
  mutate(across(marsh_flood, ~round(., 1)))

glimpse(wlr_flood)

# Step 2 - Reformat the flooding duration for easier use later in script

wlr_flood_format <- wlr_flood %>%
  select(-marsh_elev) %>%
  gather(key = zone, value = Flood_Duration_Percent, marsh_flood) %>%
  mutate(zone = ifelse(zone == "marsh_flood", "Marsh Platform")) %>%
  arrange(zone)

glimpse(wlr_flood_format)


# Chapter 4: Flooding Frequency (%) ----------------------------

# Flooding frequency is the number of times the high tide water elevation was greater 
# than the elevation of the marsh platform and the root zone. Flooding frequency is based
# on the time series of low and high tide dataset created in Script 2. 

#Step 1 -  Calculate the high tide flooding frequency (%) with the fld.frq() function
# The function requires two inputs:   
# (1) z = tidal hydrology (water elevations) from the creek tides dataset
# (2) ht = list of high tides

wlr_freq <- creek_tides %>%
  # Subset datasets to just the high tides
  filter(tide == "H") %>%
  # Remove the Date.Time column from dataset
  select(-Date.Time) %>%
  # Calculate high tide flooding frequency
  summarise(marsh_freq = fld.frq(z = elevs$Marsh_Elevation, ht = level) *100,
            # State the marsh and root zone elevations for posterity
            marsh_elev = elevs$Marsh_Elevation) %>%
  
  ungroup() %>%
#Flooding frequency columns are in a 'list' format, reformat to double and round to 1 decimal point
  mutate(marsh_freq = round(marsh_freq, 1))

glimpse(wlr_freq)

# Step 2 - Reformat the flooding frequency for easier use later in script
wlr_freq_format <- wlr_freq %>%
  select(-marsh_elev) %>%
  gather(key = zone, 
         value = Flood_Frequency_Percent, 
         marsh_freq) %>%
  mutate(zone = ifelse(zone == "marsh_freq", "Marsh Platform")) %>%
  arrange(zone)

glimpse(wlr_freq_format)


#Chapter 4: Mean Unsaturated Zone Depth ------------------------------

# The unsaturated zone is the depth from the marsh platform to the groundwater table.
# For tidal marshes, the unsaturated zone depth can be described as the lowest elevation
# of the groundwater table between two successive high tides. Due to the nature of groundwater
# drainage and seasonal tidal dynamics, the timing of the groundwater table reaching
# its lowest elevation can differ between each tidal cycle. 

# To calculate the unsaturated zone depth, the lowest groundwater elevation is located
# between each successive high tide based on the timing of the "low" tides of the 
# local creek water level recorder (e.g., low - high tide list from Script 2)

# A 6.25 hour window is created on either side of each "low" tide to capture the 
# approximate tidal window. The lowest groundwater elevation is then extracted for each
# tidal cycle. The mean +/- standard error of the unsaturated zone depth is then
# calculated. 

#Step 1 - Locate windows between successive high tides with the low and high tide time series dataset

# In order to do this, we create a 6.25 hr window on either side of the low tide event

tides_creek_seq_low <- creek_tides %>%
  #Subset dataset to only low tides to create tidal buffer window
  filter(tide == "L") %>%
  #Calculate the time 6.25 hr before and after each low tide to capture the full tidal window
  mutate(seq_low = Date.Time - minutes(380),
         seq_high = Date.Time + minutes(380)) %>%
  group_by(Date.Time) %>%
  #Create a 12.5 hour buffer (w/ 10 min intervals) before and after each low (encompassing the full tidal window),
  #Use the map function to create sequence independently for each WLR, tide type
  reframe(tide_seq = seq(from = seq_low, to = seq_high, by = '10 mins')) %>%
  ungroup() %>%
  #Rename columns for merging 
  rename(Date_Group = "Date.Time",
         Date.Time = "tide_seq")

glimpse(tides_creek_seq_low)

#Step 2 - Calculate the minimum groundwater elevation in each 12.5 hour tidal window

gw_tides_low <- wlr_format %>%
  # Merge the associated creek column of the elevations dataset to the water elevations dataset
  # Merge the 1 hour high/low tide sequences with the groundwater elevations dataset
  # based on the Associated_Creek and Date-Time columns
  merge(tides_creek_seq_low, by = c("Date.Time")) %>%
  group_by(Date_Group) %>%
  #Calculate the high tide as the maximum water elevation in the time buffer,
  mutate(tide_elev = min(WLR)) %>%
  ungroup() %>%
#Remove the tidal elevation of the select time to reduce confusion
  select(-WLR) %>%
arrange(Date.Time) %>%
  #Remove duplicates from the dataset
  distinct(Date_Group, .keep_all = TRUE)

glimpse(gw_tides_low)

#Step 3: Calculate mean unsaturated zone depth (cm)

unsat_zone <- gw_tides_low %>%
  summarise(unsat_zone_depth = elevs$Marsh_Elevation - mean(tide_elev)) %>%
  mutate(
    unsat_zone_depth = round(unsat_zone_depth * 100, 1))

glimpse(unsat_zone)

# Chapter 5: Maximum Unsaturated Zone Depth (cm) ------------------------

# The maximum unsaturated zone depth illustrates the greatest groundwater drainage
# the salt marsh experiences during the neap tides. It is calculated as the 
# difference between the marsh platform elevation and the minimum groundwater depth

# Step 1: Calculate maximum unsaturated zone depth (cm)

max_unsat_zone_depth <- 
  round((elevs$Marsh_Elevation[1] - min(wlr_format$WLR)) * 100, 1)

glimpse(max_unsat_zone_depth)

#Chapter 7: Compiled Metrics Table --------------

# All of the groundwater hydrology metrics are compiled into one nice and neat
# table to be exported for the single groundwater instrument

gw_stats <- data.frame(matrix(nrow = 1,
                               ncol = 8)) %>%
  SetNames(c("WLR", "Season", "Zone", "Elevation", "Flood_Duration_percent",
             "Flood_Frequency_percent", "Mean_Unsaturated_Zone_cm",
             "Max_Unsaturated_Zone_cm")) %>%
  mutate(
    WLR = Logger_Name,
    Season = Year,
    Zone = c("Marsh Platform"),
    Elevation = elevs$Marsh_Elevation[1],
    Flood_Duration_percent = wlr_flood_format$Flood_Duration_Percent,
    Flood_Frequency_percent = wlr_freq_format$Flood_Frequency_Percent,
    Mean_Unsaturated_Zone_cm = unsat_zone$unsat_zone_depth,
    Max_Unsaturated_Zone_cm = max_unsat_zone_depth)

glimpse(gw_stats)

# Export the flooding metrics dataset 
write.csv(gw_stats,
          paste("Output Stats\\", Site, Logger_Name, Year, "Flooding Stats.csv", 
                collapse = ""))


# Proceed to Script 6 to analyze flooding occurrence and frequency for sparrow
# islands on the marsh platform

# Proceed to Script 5 for visualizing the creek and groundwater hydrology time series
# data. 

# Proceed to Script 6 after all of the groundwater level recorders are processed and flooding metrics
# are compiled into one table for a given site for data visualizations.   


