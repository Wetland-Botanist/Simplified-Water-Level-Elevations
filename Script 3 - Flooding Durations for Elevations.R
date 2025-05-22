#Project: Analysis of an individual water level recorder for salt marsh monitoring

#Code: Script 3 - Flooding statistics of groundwater and pool water level recorders

#Authors: Grant McKown (james.mckown@unh.edu), Jennifer Gibson (jennifer.gibson@unh.edu)

#Organization: Jackson Estuarine Laboratory, University of New Hampshire


#General Explanation of 'Simplified Water Level Analysis' Project:
# The code is largely a product of the VulnToolkit package created by Troy Hill at the EPA. He devised a fantastic 
# method to analyze the duration of inundation and frequency of inundation for a given elevation. Additionally,
# the VulnToolkit calculates the elevation of the average and maximum high and low tides. 

# The overall purpose of the 'simplified' water level recorder code is to:
# (1) Calculate tidal regime flooding statistics for a site given a water level recorder in a creek or ditch
# (2) Calculate elevation-based flooding statistics from groundwater or pool water level recorders
# (3) Graph the tidal hydrology for a given lunar cycle during the study period
# (4) Calculate elevation-based flooding statistics of marsh mounds/sparrow islands from creek water level recorders
# (5) Visualize change in flooding statistics over time for given groundwater and pool water level recorders


# Script 3 (this code) of the 'Simplified Water Level Recorder Analysis', calculates flooding statistics 
# for a single water level recorder (specified by user) and exports the dataset to Microsoft Excel CSV. The code
# specifically completes the following tasks:

# (1) Import the formatted water level series dataset (Script 1), low and high tide time series dataset (Script 2),
#     and elevations of the marsh platform and root zone for each respective water level recorder (user provided)
# (2) Calculates flooding statistics for marsh platform and root zone elevations for the 30 - day monitoring window:
#       (a) Flooding Duration (%) - the amount of time water elevations are greater than the elevation
#       (b) High Tide Flooding Frequency (%) - percentage of time elevations are flooded based on creek water level recorder
#       (c) Drainage Depth (cm) - difference in elevation and average low tides based on groundwater/pools
#       (d) Drainage Amplitude (cm) - difference in average low tide and minimum water elevation
# (3) Compiles the elevations, flooding statistics for the water level recorder into a single dataset


# Data Inputs of Script 3:

# (1) Formatted Water Level Series Dataset (created and exported in Script 1)
# (2) Low and High Tide Series Dataset of Creek Water Level Recorder (created and exported in Script 2)
# (3) Marsh Platform and Root Zone Elevations of Groundwater/Pool Water Level Recorders (user provided)

# Details for Elevation Dataset:

# WLR - Name of the groundwater and pool water level recorder 
    # Names must match those of the columns of the formatted time series dataset
# Marsh_Elevation - elevation of the marsh platform surrounding the water level recorder
# Rootzone_Elevation - elevation of the root zone surrounding the water level recorder (typically 5 cm below ground)


# User Inputs of Script 3 (in Chapter 2 of the script):
# (1) Site Name 
# (2) Year
# (3) Groundwater/Pool Water Level Recorder Name

# Site Name and Year will be used to name exported tables at the end of Script 1. 
# Water level recorder name will be used to select the specific WLR for analysis. 




# Chapter 1: Package Library --------------------------------------------------

#Tidyr and dplyr are great data organization packages
library(tidyr)
library(dplyr)
library(pillar)
library(purrr)

#Lubridate is a special package that allows us to do some cool stuff with dates and time arithmetic
library(lubridate)

#DescTools is a special package that allows for some cool functions
library(DescTools)

#VulnToolkit is a special package that does all the meat and potatoes for this R-script
library(VulnToolkit)




#Chapter 2: Import datasets and user generated inputs -----------------------------------------

#Step 1 - User generated inputs

# Name of the water level recorder that will be selected for data anlaysis
Logger_Name <- "RUN_1"

# Name of the creek water level recorder (used in Script 2)
Creek_Logger <- "Creek"

# Name of the Salt Marsh and Site (used in Scripts 1 and 2)
Site <- "Essex Site 1"

# Year of monitoring season (used in Scripts 1 and 2)
Year <- "2020"

# The needed datasets are: (1) Formatted continuous water level elevations series,
#                          (2) Tidal regime series (list of low and high tides), and
#                          (3) Marsh Platform and Root zone Elevations (user provided)

# Step 2 -  Import the formatted water elevation time series dataset

wlr_format <- read.csv(paste("Formatted Datasets\\", Site, Year, "WLR Formatted Dataset.csv", 
                      collapse = "")) %>%
  mutate(Date.Time = as.POSIXct(Date.Time, format = '%Y-%m-%d %H:%M:%S')) %>%
  dplyr::select(-X) %>%
  rename(WLR = Logger_Name) %>%
  select(Date.Time, WLR) %>%
  filter(!is.na(Creek_Logger),
         !is.na(WLR),
         !is.na(Date.Time))

glimpse(wlr_format)

# Step 3 - Import the low and high tide time series dataset of the creek water level recorder

creek_tides <- read.csv(paste("Formatted Datasets\\", Site, Creek_Logger, Year, "Tidal Hydrology Dataset.csv", 
                        collapse = "")) %>%
  select(-X) %>%
  mutate(Date.Time = as.POSIXct(Date.Time, format = '%Y-%m-%d %H:%M:%S'))

glimpse(creek_tides)



# Step 4 - Import the marsh platform and root one elevation dataset
elevs <- read.csv("Input Data\\Essex Marsh Elevations.csv") %>%
  filter(WLR == Logger_Name)

glimpse(elevs)



#Chapter 3: Calculate the Flooding Duration ------------------------------------

#Step 1 - Calculate the flooding duration of the marsh platform and root zone elevation

#To accomplish using the fld.dur() function, which returns the flooding duration (as a percent of monitoring time)

#Additionally, we will create two new columns that "pulls" the marsh platform and root zone elevations from
#the 'elevs' dataframe.

wlr_flood <- wlr_format %>%
  summarise(marsh_flood = fld.dur(z = elevs$Marsh_Elevation,
                                  level = WLR) * 100,
            root_flood = fld.dur(z = elevs$Rootzone_Elevation,
                                 level = WLR) * 100,
            
            marsh_elev = elevs$Marsh_Elevation,
            
            
            root_elev = elevs$Rootzone_Elevation) %>%
  ungroup() %>%
  mutate(across(marsh_flood:root_flood, ~round(., 1)))

glimpse(wlr_flood)

# Step 2 - Reformat flooding duration for future dataset

wlr_flood_format <- wlr_flood %>%
  select(-marsh_elev, -root_elev) %>%
  gather(key = zone, value = Flood_Duration_Percent, marsh_flood, root_flood) %>%
  mutate(zone = ifelse(zone == "marsh_flood", "Marsh Platform", "Root Zone")) %>%
  arrange(zone)



# Chapter 4: Calculate the high tide flooding frequency ----------------------------

#Step 1: Calculate the high tide flooding frequency (%) with the fld.frq() function
# The function requires two inputs:   
# (1) z = tidal hydrology (water elevations) from the creek tides dataset
# (2) ht = list of high tides

# Use the low and high tide time series dataset of the creek water level recorder
# to calculate flooding frequency of the marsh platform and root zone

wlr_freq <- creek_tides %>%
  filter(tide == "H") %>%
  select(-Date.Time) %>%
  summarise(marsh_freq = fld.frq(z = elevs$Marsh_Elevation, ht = level) *100,
            
            root_freq = fld.frq(z = elevs$Rootzone_Elevation, ht = level) * 100,
            
            marsh_elev = elevs$Marsh_Elevation,
            
            root_elev = elevs$Rootzone_Elevation) %>%
  
  ungroup() %>%
#Flooding frequency columns are in a 'list' format, reformat to double and round to 2 decimal points
  mutate(across(c(marsh_freq, root_freq), ~as.numeric(.)),
         across(c(marsh_freq, root_freq), ~round(., 1)))

glimpse(wlr_freq)

# Step 2: Reformat the high tide flooding frequency for future dataset
wlr_freq_format <- wlr_freq %>%
  select(-marsh_elev, -root_elev) %>%
  gather(key = zone, value = HT_Frequency_Percent, marsh_freq, root_freq) %>%
  mutate(zone = ifelse(zone == "marsh_freq", "Marsh Platform", "Root Zone")) %>%
  arrange(zone)

glimpse(wlr_freq_format)




#Chapter 5: Calculate Drainage Depth -------------------------------------------

# Drainage depth is defined as the difference between the marsh platform or root zone
# elevation and the average "low tide" of a water level recorder. However, pool and
# groundwater hydrology do not follow idealized sinusoidal curves like creek water
# level recorders. We redefined "low tide" for these circumstances to the minimum
# water elevation between high tides (e.g., every 12 hours) based on the timing
# of high tides of the creek water level recorder. 

#Step 1: Divide the entire monitoring period into 12.5 hour windows to determine low tides

tides_creek_seq_low <- creek_tides %>%
  #Only focusing on low tides to create tidal buffer window
  filter(tide == "L") %>%
  #Calculate the time 6.25 hr before and after each low tide to capture the full tidal window
  mutate(seq_low = Date.Time - minutes(380),
         seq_high = Date.Time + minutes(380)) %>%
  group_by(Date.Time) %>%
  #Create a 12.5 hour buffer (10 min intervals) before and after each low (encompassing the full tidal window),
  #Use the map function to create sequence independently for each WLR, tide type
  reframe(tide_seq = seq(from = seq_low, to = seq_high, by = '10 mins')) %>%
  ungroup() %>%
  #Rename columns for merging 
  rename(Date_Group = "Date.Time",
         Date.Time = "tide_seq")

glimpse(tides_creek_seq_low)

#Step 2: Calculate the minimum water elevation or "low tide" in each 12.5 hour window

gw_tides_low <- wlr_format %>%
  # Merge the Associated Creek column of the elevations dataset to the water elevations dataset
  #Merge the 1 hour high/low tide sequences with the groundwater elevations dataset
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


#Step 3: Calculate the average low tide and then subtract from the marsh platform and root zone elevations

drainage_depth <- gw_tides_low %>%
  summarise(Marsh_Drainage = elevs$Marsh_Elevation - mean(tide_elev),
            RootZone_Drainage = elevs$Rootzone_Elevation - mean(tide_elev)) %>%
  mutate(across(Marsh_Drainage:RootZone_Drainage,
                ~round(., 3)))

glimpse(drainage_depth)



# Chapter 6: Calculate Drainage Amplitude -------------------------------------------

# Drainage amplitude is an elevation blind metric to evaluate drainage, especially during neap tidal cycles

# The thought is that with drainage enhancement with runnels, the "drainage amplitude" will increase as 
# the minimum water elevation decreases into the root zone compared to staying stagnant on the surface as a pool

# Drainage amplitude is calculated as the difference between average low tide and the minimum water elevation of the dataset

# Step 1: Calculate average low tide based on results of Chapter 5

mlt <- gw_tides_low %>%
  summarise(mlt = mean(tide_elev, na.rm = TRUE))
  
mlt
  
#Step 2: Calculate drainage amplitude

drainage_amplitude <- round(
  mlt[1,1] - min(wlr_format$WLR), 
                   3)

glimpse(drainage_amplitude)



#Chapter 7: Combine flooding statistics--------------

# Elevations and flooding statistics are compiled into one comprehensive table
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

# Flooding statistics table is exported to Microsoft Excel CSV
write.csv(gw_stats,
          paste("Output Stats\\", Site, Logger_Name, Year, "Flooding Stats.csv", 
                collapse = ""))

# Continue onto Script 4 - Visualize Hydrology During Analysis Timeperiod 

