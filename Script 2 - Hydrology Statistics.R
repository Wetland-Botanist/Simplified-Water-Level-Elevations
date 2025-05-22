#Project: Analysis of an individual water level recorder for salt marsh monitoring

#Code: Script 2 - Analysis of Tidal Hydrology Regime of Creek Water Level Recorders

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

# In Script 2 (this code), of the 'Simplified Water Level Recorder Analysis', tidal hydrology
# statistics are calculated for a SINGLE water level recorder in a creek, ditch, or hydrologic pathway.
# The code is not appropriate for groundwater level recorders, since determination of low and high tides
# can be impossible with the unique drainage patterns of groundwater. 

# (1) Identifies each individual low and high tides in the time series of the Creek water level recorder
# (2) Calculates the the following tidal statistics (mean +/- standard error):
# (a) Mean Lower Low Tide Elevation
# (b) Mean Low Tide Elevation
# (c) Mean High Tide Elevation
# (d) Mean Higher High Tide Elevation
# (e) Maximum Tidal Elevation observed
# (3) Exports the tidal statistics calculations in both raw (mean and standard error
#     in separate columns) and formatted (mean +/- standard error in single column)


# Data Inputs of Script 2:

# (1) Formatted Water Level Series Dataset (created and exported in Script 1)

# User should input the Site Name and Year used in Script 1 and the script will automatically
# import the dataset from the 'Formatted Dataset' folder


# Script 2 user inputs are in Chapter 2 of the script and are as follows:
# (1) Site Name 
# (2) Year

# Site Name will be used to name exported tables at the end of Script 1. 


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



#Chapter 2: Import the formatted water level elevation time series dataset ----------------

#Step 1: User inputs for Script 2

# User inputs are required to automatically pull and load the formatted time series dataset
# from the 'Formatted Dataset' Folder and select the proper 'Creek' column from the dataset

# Name of the creek water level recorder column in the formatted time series dataset
Creek_Logger <- "Creek"

#Name of the Salt Marsh and Site used in Script 1 to create the title of the formatted time series dataset
Site <- "Essex Site 3"

# Year generated in Script 1 to create the title of the formatted time series dataset
Year <- "2023"



# Step 2: Import the formatted time series dataset, remove miscellaneous columns, select the Creek column

wlr_format <- read.csv(paste("Formatted Datasets\\", Site, Year, "WLR Formatted Dataset.csv", 
                                       collapse = "")) %>%
  mutate(Date.Time = as.POSIXct(Date.Time, format = '%Y-%m-%d %H:%M:%S')) %>%
  select(-X) %>%
  rename(WLR = Creek_Logger) %>%
  filter(is.na(Date.Time) == FALSE)

glimpse(wlr_format)



#Chapter 3: Calculate the High and Low Tide for every 12.5 hours (every full tidal cycle) ------------------

# The function HL() from VulnToolKit determines the individual low tides and high tides in the time series
  # Function also identifies the lower low tide and higher high tide for each individual day
wlr_tides <-HL(level = wlr_format$WLR, 
               time = wlr_format$Date.Time, period = 12.5) %>%
  rename(Date.Time = time)

glimpse(wlr_tides)

# Export the low and high tide time series dataset to the 'Formatted Datasets' Folder
# be used in Script 3 for groundwater and pool hydrology analysis

write.csv(wlr_tides,
          paste("Formatted Datasets\\", Site, Creek_Logger, Year, "Tidal Hydrology Dataset.csv", 
                collapse = ""))


#Chapter 4: Calculate tidal datums for site-level hydrology --------------------

#Step 1: Calculate the mean and standard error for the daily low and high tides

wlr_tides_stats <- wlr_tides %>%
  group_by(tide) %>%
  summarise(
    avg_elev = mean(level),
    se_elev = sd(level)/sqrt(n()),
    count = n()) %>%
  mutate(across(avg_elev:se_elev, ~round(., 3)))

glimpse(wlr_tides_stats)

#Step 2: Calculate the mean and standard error of higher high tide for the daily tides

wlr_tides_stats2 <- wlr_tides %>%
  filter(tide2 == "HH") %>%
  summarise(
    avg_hh = mean(level),
    se_hh = sd(level)/sqrt(n()),
    count = n()) %>%
  mutate(across(avg_hh:se_hh, ~round(., 3)))

glimpse(wlr_tides_stats2)

#Step 3: Calculate the mean and standard error of lower low tide for the daily tides

wlr_tides_stats3 <- wlr_tides %>%
  filter(tide2 == "LL") %>%
  summarise(
    avg_ll = mean(level),
    se_ll = sd(level)/sqrt(n()),
    count = n()) %>%
  mutate(across(avg_ll:se_ll, ~round(., 3)))

glimpse(wlr_tides_stats3)


#Step 4: Calculate the mean and standard error of the mean tide 
# Mean tide is calculated in this script as the mean of ALL low and high tides in the dataset

wlr_tides_mean <- wlr_tides %>%
  summarise(avg_mean = mean(level),
            se_mean = sd(level)) %>%
  mutate(across(avg_mean:se_mean, ~round(., 3)))

#Step 5: Calculate the maximum water elevation observed throughout the dataset

wlr_tides_max <- wlr_tides %>%
  summarise(
    max_tide = max(level)) %>%
  mutate(max_tide = round(max_tide, 3))



#Chapter 6: Compile the tidal datums into one comprehensive data table -------------

#Step 1 - Generate data table with mean and standard errors as separte columns (unformatted data table)

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

# Export the unformatted tidal datum table to the 'Output Stats' folder
write.csv(wlr_stats,
          paste("Output Stats\\", Site, Creek_Logger, Year, "Hydrology Stats.csv", 
                collapse = ""))


# Step 2: Compile the mean and standard error of tidal datums into a single column (formatted data table)

wlr_stats_format <- wlr_stats %>%
  mutate(
    LT = paste(Mean_LT, SE_LT, sep = " +/- "),
    
    LLT = paste(Mean_LLT, SE_LLT, sep = " +/- "),
    
    HT = paste(Mean_HT, SE_HT, sep = " +/- "),
    
    HHT = paste(Mean_HHT, SE_HHT, sep = " +/- "),
    
    MT = paste(Mean_MT, SE_MT, sep = " +/- ")) %>%
    dplyr::select(WLR, LT, LLT, HT, HHT, MT, Max_Tide)
  
# Export the formatted data table to the 'Output Stats' folder
write.csv(wlr_stats_format,
          paste("Output Stats\\", Site, Creek_Logger, Year, "Hydrology Stats FORMATTED.csv", 
                collapse = ""))




#Continue onto Script 3 - Groundwater and Pool water level recorder analysis

