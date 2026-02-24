# Project: Analysis of an individual water level recorder for salt marsh monitoring
# Analysis: Groundwater Hydrology Analysis
#Script: Water Level Recorder Processing - Flooding Metrics of Groundwater WLRs

#Authors: Grant McKown (james.mckown@unh.edu)


# In Script 3 (this code) of the 'Simplified Water Level Recorder Analysis', users provide
# elevations of the marsh platform and root zone (5 cm below ground) in the 'Marsh and Root Zone
# Elevations' input dataset. The code uses the Formatted Water Level Series and the Tidal Regime 
# Water Level Series (list of low and high tides), created in Script 2, to calculate the flooding 
# frequency and flooding duration for given marsh elevations. 

# It should be noted the script has multiple data inputs including:
# (1) Formatted continuous water level elevations series (created in Script 1),
# (2) High and low tides list (created in Script 2),
# (3) Marsh Platform and Root zone Elevations (user provided)


#Chapter 1: Package Library

#Data organization and 
library(tidyr)
library(dplyr)
library(lubridate)
library(DescTools)
library(purr)

#Hydrology Analysis
library(VulnToolkit)


#Chapter 2: Import hydrology and elevation datasets ----------------------

#Step 1 - User input name of Site, Year, and creek logger name (column name)

# User input data allows the program to easily retrieve the hydrology time series
# dataset from the Formatted Datasets folder without the user having to change
# the file path. The user input data are the same in Script 2 as well

# Logger name = water level recorder column header
# Creek logger = creek water level recorder column header

Logger_Name <- "NAC" ; Creek_Logger <- "Creek" ; Site <- "Essex South" ; Year <- "2024"



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
  select(Date.Time, WLR) %>%
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

elevs <- read.csv("Input Data\\EssexSouth_MarshElevations_2024.csv") %>%
  # Reduce the elevation dataset to just the groundwater level recorder in question
  filter(WLR == Logger_Name)

glimpse(elevs)



#Chapter 3: Calculate the Flooding Duration --------------------

# Flooding duration is the amount of time the marsh platform and root zone are 
# submerged based on the groundwater water level recorder. Flooding duration
# is reported as the number of hours and percentage of monitoring duration

#To accomplish using the fld.dur() function, which returns the flooding duration (as a percent of monitoring time)

# Step 1 - Calculate the flooding duration based on elevation dataset

wlr_flood <- wlr_format %>%
  # Calculate flooding duration
  summarise(marsh_flood = fld.dur(z = elevs$Marsh_Elevation,
                                  level = WLR) * 100,
            root_flood = fld.dur(z = elevs$Rootzone_Elevation,
                                 level = WLR) * 100,
            # State the marsh and root zone elevations for posterity
            marsh_elev = elevs$Marsh_Elevation,
            
            
            root_elev = elevs$Rootzone_Elevation) %>%
  ungroup() %>%
  # Round the flooding durations to 1 decimal place
  mutate(across(marsh_flood:root_flood, ~round(., 1)))

glimpse(wlr_flood)

# Step 2 - Reformat the flooding duration for easier use later in script

wlr_flood_format <- wlr_flood %>%
  select(-marsh_elev, -root_elev) %>%
  gather(key = zone, value = Flood_Duration_Percent, marsh_flood, root_flood) %>%
  mutate(zone = ifelse(zone == "marsh_flood", "Marsh Platform", "Root Zone")) %>%
  arrange(zone)


# Chapter 4: High tide flooding frequency ----------------------------

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
            
            root_freq = fld.frq(z = elevs$Rootzone_Elevation, ht = level) * 100,
            # State the marsh and root zone elevations for posterity
            marsh_elev = elevs$Marsh_Elevation,
            
            root_elev = elevs$Rootzone_Elevation) %>%
  
  ungroup() %>%
#Flooding frequency columns are in a 'list' format, reformat to double and round to 1 decimal point
  mutate(across(c(marsh_freq, root_freq), ~as.numeric(.)),
         across(c(marsh_freq, root_freq), ~round(., 1)))

glimpse(wlr_freq)

# Step 2 - Reformat the flooding frequency for easier use later in script
wlr_freq_format <- wlr_freq %>%
  select(-marsh_elev, -root_elev) %>%
  gather(key = zone, value = HT_Frequency_Percent, marsh_freq, root_freq) %>%
  mutate(zone = ifelse(zone == "marsh_freq", "Marsh Platform", "Root Zone")) %>%
  arrange(zone)

glimpse(wlr_freq_format)


#Chapter 4: Drainage Depth ------------------------------

# Drainage depth is the average minimum water elevation depth between two successive
# high tides. Drainage depth provides insight into the groundwater table elevation
# normally affecting vegetation. Drainage depth is calculated for the marsh platform
# and root zone (- 5 cm)

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


#Task 3: Calculate drainage depth

# Task 2 calculated the groundwater table elevation, yet drainage depth is relative
# to the elevation of the marsh platform and root zone. Therefore groundwater table 
# calculated in Step 2 is substrated from the marsh platform and root zone elevations.

# Positive drainage depth = groundwater depths BELOW marsh platform or root zone
# Negative drainage depth = Marsh platform or root zone are normally SUBMERGED

drainage_depth <- gw_tides_low %>%
  summarise(Marsh_Drainage = elevs$Marsh_Elevation - mean(tide_elev),
            RootZone_Drainage = elevs$Rootzone_Elevation - mean(tide_elev)) %>%
  mutate(across(Marsh_Drainage:RootZone_Drainage,
                ~round(., 3)))

glimpse(drainage_depth)



# Chapter 6: Drainage Amplitude ----------------------------

# Drainage amplitude calculates the additional drainage seen during neap tide
# cycles and provides insight into the level of drainage during the 1 - 2 weeks
# the marsh platform is not submerged from tidal flooding. You can consider it
# as the extra drainage the marsh receives ON TOP of the normal drainage from
# between average high tides. 

# Drainage amplitude is calculated as the difference between the mean
# groundwater table elevation and the minimum groundwater elevation of the dataset


# Step 1 - Calculate the mean groundwater elevation

gw_mean_elev <- gw_tides_low %>%
  summarise(llt = mean(tide_elev, na.rm = TRUE))
  
gw_mean_elev
  
#Step 2 - Calculate drainage amplitude

drainage_amplitude <- round(
  gw_mean_elev[1,1] - min(wlr_format$WLR), 3)

glimpse(drainage_amplitude)

#Chapter 7: Flooding Metrics Statistics Table --------------

# All of the groundwater hydrology metrics are compiled into one nice and neat
# table to be exported for the single groundwater instrument

gw_stats <- data.frame(matrix(nrow = 2,
                               ncol = 8)) %>%
  SetNames(c("WLR", "Season", "Zone", "Elevation", "Flood_Duration_percent",
             "HT_Frequency", "Drainage_Depth_m", "Drainage_Amplitude_m")) %>%
  mutate(
    WLR = Logger_Name,
    Season = Year,
    Zone = c("Marsh Platform", "Root Zone"),
    Elevation = ifelse(Zone == "Marsh Platform", elevs$Marsh_Elevation, elevs$Rootzone_Elevation),
    Flood_Duration_percent = wlr_flood_format$Flood_Duration_Percent,
    HT_Frequency = wlr_freq_format$HT_Frequency_Percent,
    Drainage_Depth_m = ifelse(Zone == "Marsh Platform", drainage_depth$Marsh_Drainage, drainage_depth$RootZone_Drainage),
    Drainage_Amplitude_m = drainage_amplitude[1,1])

glimpse(gw_stats)

# Export the flooding metrics dataset 
write.csv(gw_stats,
          paste("Output Stats\\", Site, Logger_Name, Year, "Flooding Stats.csv", 
                collapse = ""))



# Proceed to Script 4 for visualizing the creek and groundwater hydrology time series
# data. 

# Proceed to Script 5 after all of the groundwater level recorders are processed and flooding metrics
# are compiled into one table for a given site for data visualizations.   

# Proceed to Script 6 after all of the groundwater level recorders are processed and flooding metrics
# are compiled into one project-wide table for analysis at the project scale.  

