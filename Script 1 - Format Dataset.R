#Project: Analysis of an individual water level recorder for salt marsh monitoring

#Code: Script 1 - Format the Water Level Recorder Dataset

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

# Script 1 (this code), of the 'Simplified Water Level Analysis' project formats the continuous water
# level series input dataset (user provided) for analysis and graphing in Scripts 2 - 4. The 
# following is completed by Script 1:
#   (1) Reduce the study period to a 30-day lunar cycle, anchored in the middle of the study period
#   (2) Calculate the start and end of both water level recorder deployment and analysis time periods
#   (3) Export a table of the metadata of the deployment and analysis time periods
#   (4) Export a table of the formatted water level recorder series

# Data Inputs of Script 1:

# A time-series of water level recorders across a given site with only 1 creek water level recorder and all
# associated groundwater and pool water level recorders. Columns are:
# (1) 'Date.Time' - Date and time of the time series 
# (2) 'Creek' - Water elevations of the creek water level recorder; must have 'Creek' within the full column name
# (3) Pool and Groundwater level recorders - water elevations of remaining associated water level recorders, names
#                                            are up to user. 


# User Inputs for Script 1 are in Chapter 2 of the script and are as follows:
# (1) File Directory Location of the water level recorder time series dataset
# (2) Site Name 

# Site Name will be used to name exported tables at the end of Script 1. 




# Chapter 1: Package Library --------------------------------------------------

#Tidyr and dplyr are great data organization packages
library(tidyr)
library(dplyr)
library(pillar)

#Lubridate is a special package that allows us to do some cool stuff with dates and time arithmetic
library(lubridate)

#DescTools is a special package that allows for some cool functions
library(DescTools)


#Chapter 2: Import the Water Level Elevation Time Series Dataset -------------------

# User must provide the file directory pathway for the dataset (dataset should be stored in 'Input Data' folder)
wlr <- read.csv("Input Data\\Kents Island Hydrology TIme Series Example.csv") %>%
  filter(is.na(Creek) == FALSE)

glimpse(wlr)


# User must provide the name of the Salt Marsh and Site that will be used for naming exported tables

Site <- "Kents Island"


#Chapter 3: Format the Water Level Elevation Dataset ----------------------------

# Converts the 'Date.Time' column to a POSIT data type to make data calculations in 'lubridate' 
# package easier to reduce the entire dataset to 30-day lunar tidal cycle

#Step 1: Conversion of Date.Time

wlr <- wlr %>%
  mutate(Date.Time = as.POSIXct(Date.Time, format = '%m/%d/%Y %H:%M')) %>%
  filter(!is.na(Date.Time))

  glimpse(wlr)

Year <- max(unique(Year(wlr$Date.Time)))
  
#Step 2: Reduce the dataset to the 30 day lunar tidal cycle

#For research methods for the lab at University of New Hampshire, we normally deploy the WLRs for 
# roughly 30 - 40 days. We usually are not able to collect the WLRs perfectly at the end of the 
# lunar tidal cycle. We decided to subset the WLR data set to roughly a spring tidal cycle (~30 days). 
# We calculate the exact center date in the monitoring period and +/- 15 days. 

lunar_cycle <- data.frame(matrix(nrow = 1, ncol = 8)) %>%
  setNames(c("Site_Name", 'Year', 'Deployment_Duration', 'Deployment_Days', 'Analysis_Duration', 'Analysis_Days',
             'Start_Date', 'End_Date')) %>%
  mutate( Site_Name = Site,
          WLR = colnames(wlr)[2],
          Year = year(wlr$Date.Time[1]),
    
    #Deployment time is calculated as the total duration of the water level elevation dataset
    #Deployment time is calculated with the as.duration() function that converts the entire dataset to the number of seconds
    #Deployment time is then divided by the number of days to calculate the deployment time in days
        
          Deployment_Days = as.duration(min(wlr$Date.Time) %--% max(wlr$Date.Time))/ddays(1),
          Deployment_Duration = paste(min(wlr$Date.Time), max(wlr$Date.Time), sep = " - "),
    
    #Middle, Start, and End dates are calculated with the lubridate() package with quick mathematics of dates
    #Currently, start and end dates are calculated as +/- 15 days from the middle date
         Middle_Date = days(round(Deployment_Days/ 2, 0)) + wlr$Date.Time[1],
         
         Start_Date = Middle_Date - days(15),
         End_Date = Middle_Date + days(15),
    
          Analysis_Days = as.duration(Start_Date %--% End_Date) / ddays(1),
          Analysis_Duration = paste(Start_Date, End_Date, sep = " - "))

# Export the analysis and deployment metadata to Microsoft Excel CSV file
write.csv(lunar_cycle,
          paste("Output Stats\\", Site, Year, "WLR Metadata.csv", 
                collapse = ""))


#Step 3: Next filter the 'wlr' dataset within the parameters of the start and end dates

wlr_format <- wlr %>%
  filter(Date.Time > lunar_cycle$Start_Date,
         Date.Time < lunar_cycle$End_Date)

#Step 4: Export the formatted continuous water level recorder dataset 

write.csv(wlr_format,
          paste("Formatted Datasets\\", Site, Year, "WLR Formatted Dataset.csv", 
                collapse = ""))


#Continue onto Script 2 - Creek Hydrology Analysis code 

