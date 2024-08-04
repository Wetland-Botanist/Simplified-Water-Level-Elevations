#Project: Analysis of an individual water level recorder for salt marsh monitoring

#Code: Part 2 - Analysis of Tidal Hydrology Regime of the Creek Water Level Recorders

#Authors: Grant McKown (james.mckown@unh.edu), Jennifer Gibson (jennifer.gibson@unh.edu)

#Organization: Jackson Estuarine Laboratory, University of New Hampshire


#General Explanation of 'Simplified Water Level Analysis' Project:
# The code is largely a product of the VulnToolkit package created by Troy Hill at the EPA. He devised a fantastic 
# method to analyze the duration of inundation and frequency of inundation for a given elevation. Additionally,
# the VulnToolkit calculates the elevation of the average and maximum high and low tides. 

# The overall purpose of the 'simplified' water level recorder code is to:
# (1) Calculate tidal regime flooding statistics for a given water level recorder
# (2) Calculate flooding statistics for given elevations based on the input water level recorder
# (3) Graph the tidal hydrology for a given lunar cycle during the study period


# In Script 2 (this code), of the 'Simplified Water Level Recorder Analysis', tidal hydrology
# statistics are calculated for a SINGLE water level recorder in a creek, ditch, or hydrologic pathway.
# The code is not appropriate for groundwater level recorders, since determination of low and high tides
# can be impossible with the unique drainage patterns of groundwater. The script calculates the
# the following tidal statistics:
  # (1) Mean Lower Low Tide Elevation
  # (2) Mean Low Tide Elevation
  # (3) Mean High Tide Elevation
  # (4) Mean Higher High Tide Elevation
  # (5) Maximum Tidal Elevation observed


#Chapter 1: Library of packages for necessary work

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



#Chapter 2: Import the formatted water level elevation time series dataset

# The code removes the pesky "X" column created to label row names

wlr_format <- read.csv(paste("Formatted Datasets\\", Logger_Name, "WLR Formatted Dataset.csv", 
                      collapse = "")) %>%
  mutate(Date.Time = as.POSIXct(Date.Time, format = '%Y-%m-%d %H:%M:%S')) %>%
  select(-X)

glimpse(wlr)


#Chapter 3: Calculate the High and Low Tide for every 12.5 hours (every full tidal cycle)

wlr_tides <-HL(level = wlr_format$WLR, 
               time = wlr_format$Date.Time, period = 12.5)

glimpse(wlr_tides)

write.csv(wlr_tides,
          paste("Formatted Datasets\\", Logger_Name, "Tidal Hydrology Dataset.csv", 
                collapse = ""))


#Chapter 5: Calculate the mean and standard error for hydrology metrics

#First, calculate the mean and standard error for the daily low and high tides

wlr_tides_stats <- wlr_tides %>%
  group_by(tide) %>%
  summarise(
    avg_elev = mean(level),
    se_elev = sd(level)/sqrt(n()),
    count = n()) %>%
  mutate(across(avg_elev:se_elev, ~round(., 3)))

#Second, calculate the mean and standard higher high tide for the daily tides

wlr_tides_stats2 <- wlr_tides %>%
  filter(tide2 == "HH") %>%
  summarise(
    avg_hh = mean(level),
    se_hh = sd(level)/sqrt(n()),
    count = n()) %>%
  mutate(across(avg_hh:se_hh, ~round(., 3)))


#Third, calculate the mean and standard lower low tide for the daily tides

wlr_tides_stats3 <- wlr_tides %>%
  filter(tide2 == "LL") %>%
  summarise(
    avg_ll = mean(level),
    se_ll = sd(level)/sqrt(n()),
    count = n()) %>%
  mutate(across(avg_ll:se_ll, ~round(., 3)))


#Fourth, calculate the mean and standard error of the mean tide 
# Mean tide is calculated in thsi script as the mean of ALL low and high tides in the dataset

wlr_tides_mean <- wlr_tides %>%
  summarise(avg_mean = mean(level),
            se_mean = sd(level)) %>%
  mutate(across(avg_mean:se_mean, ~round(., 3)))

#Fifth, calculate the max tide observed throughout the dataset

wlr_tides_max <- wlr_tides %>%
  summarise(
    max_tide = max(level)) %>%
  mutate(max_tide = round(max_tide, 3))



#Chapter 6: Wrap up all of the stats into one coherent data frame

wlr_stats <- data.frame(matrix(nrow = 1,
                               ncol = 12)) %>%
  SetNames(c("WLR", "Mean_LT", "SE_LT", "Mean_LLT", "SE_LLT", 
             "Mean_HT", "SE_HT", "Mean_HHT", "SE_HHT", "Mean_MT", "SE_MT", "Max_Tide")) %>%
  mutate(
    WLR = Logger_Name,
    
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
          paste("Output Stats\\", Logger_Name, "Hydrology Stats.csv", 
                collapse = ""))


#Continue onto the Groundwater and Pool water level recorder analysis R code



