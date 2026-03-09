# Project: Analysis of an individual water level recorder for salt marsh monitoring
# Script 1: Formatting and Organization of Water Level Elevation Dataset
# Authors: Jenny Gibson (jennifer.gibson@unhs.edu), Grant McKown (james.mckown@unh.edu)

# Last Updated: March 9th, 2026

#Project Description:
# The code is largely a product of the VulnToolkit package created by Troy Hill 
# at the EPA. He devised a fantastic method to analyze the duration of 
# inundation and frequency of inundation for a given elevation. Additionally,
# the VulnToolkit calculates the elevation of the average and maximum high and low tides. 

# The overall purpose of the code is to analyze water level elevation data to 
# accurately describe and summarise the tidal hydrology regime for salt marsh 
# systems and groundwater regimes at individual water level recorders


# Script Description: 
# Script imports time series groundwater hydrology data of a 
# series of groundwater water level recorders and a single creek water level recorder
# at one of the sites in the project. Script formats the dataset, subsets the dataset
# to a 30 day lunar cycle based on the median date in the  dataset. Descriptions
# of deployment and collection times, monitoring duration, and other site metadata
# for the probes are detailed and exported. 


#Chapter 1: Package Library ------------------------------------

# Data Organization and Formatting
library(tidyr)
library(dplyr)
library(lubridate)
library(DescTools)


#Chapter 2: Import Water Elevations Dataset -----------------

# Step 1: User input Site Name
# The site name is recorded in the CSV and Image exports for all formatted datasets, 
# tidal regime statistics, flooding statistics, and water level elevation graphs in
# the whole R project. Strictly for naming and file export and reading purposes.

Site <- "Kents Island"


wlr <- read.csv("Input Data\\Kents Island 2025 Compiled WLRs R.csv") %>%
  filter(!is.na(Date.Time),
         Date.Time != "")

glimpse(wlr)





#Chapter 3: Format hydrology dataset -------------------

# Convert the 'Date.Time' column to a POSIT data type to make data calculations in 'lubridate' 
# package easier to reduce the entire dataset to 30-day lunar tidal cycle

#Step 1 - Conversion of Date.Time

wlr_format <- wlr %>%
  mutate(Date.Time = as.POSIXct(Date.Time, format = '%m/%d/%Y %H:%M'))

glimpse(wlr_format)

Year <- min(unique(year(wlr_format$Date.Time)))
  
#Step 2 - Reduce the dataset to the 30 day lunar tidal cycle

#For research methods for the lab at University of New Hampshire, we normally deploy the WLRs for 
# roughly 30 - 40 days. We usually are not able to collect the WLRs perfectly at the end of the 
# lunar tidal cycle. We decided to subset the WLR data set to roughly a spring tidal cycle (~30 days). 
# We calculate the exact center date in the monitoring period and +/- 15 days. 

lunar_cycle <- data.frame(matrix(nrow = 1, ncol = 8)) %>%
  setNames(c("Site_Name", 'Year', 'Deployment_Duration', 'Deployment_Days', 'Analysis_Duration', 'Analysis_Days',
             'Start_Date', 'End_Date')) %>%
  mutate( Site_Name = Site,
          WLR = colnames(wlr_format)[2],
          Year = year(wlr_format$Date.Time[1]),
    
    #Deployment time is calculated as the total duration of the water level elevation dataset
    #Deployment time is calculated with the as.duration() function that converts the entire dataset to the number of seconds
    #Deployment time is then divided by the number of days to calculate the deployment time in days
        
          Deployment_Days = as.duration(min(wlr_format$Date.Time) %--% max(wlr_format$Date.Time))/ddays(1),
          Deployment_Duration = paste(min(wlr_format$Date.Time), max(wlr_format$Date.Time), sep = " - "),
    
    #Middle, Start, and End dates are calculated with the lubridate() package with quick mathematics of dates
    #Currently, start and end dates are calculated as +/- 15 days from the middle date
         Middle_Date = days(round(Deployment_Days/ 2, 0)) + wlr_format$Date.Time[1],
         
         Start_Date = Middle_Date - days(15),
         End_Date = Middle_Date + days(15),
    
          Analysis_Days = as.duration(Start_Date %--% End_Date) / ddays(1),
          Analysis_Duration = paste(Start_Date, End_Date, sep = " - "))

glimpse(lunar_cycle)


# Export the metadata of the deployment of the water level recorders
write.csv(lunar_cycle,
          paste("Output Stats\\", Site, Year, "WLR Metadata.csv", 
                collapse = ""))


# Step 3 - Next filter the hydrology dataset based on the start and end dates

wlr_subset <- wlr_format %>%
  filter(Date.Time > lunar_cycle$Start_Date,
         Date.Time < lunar_cycle$End_Date)

# Step 4 - Export the formatted continuous water level recorder dataset 

write.csv(wlr_subset,
          paste("Formatted Datasets\\", Site, Year, "WLR Formatted Dataset.csv", 
                collapse = ""))



# Tidal datums are calculated in Script 2 based on the creek water level recorder

