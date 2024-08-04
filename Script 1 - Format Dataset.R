#Project: Analysis of an individual water level recorder for salt marsh monitoring

#Code: Part 1 - Format the Water Level Recorder Dataset


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

# Script 1 (this code), of the 'Simplified Water Level Analysis' project formats the continuous water
# level series input dataset (user provided) for analysis and graphing in Scripts 2 - 4. The 
# water level series is formatted by:
#   (1) Reduces the study period to a 30-day lunar cycle, anchored in the middle of the study period


#Chapter 1: Library of packages for necessary work

#Tidyr and dplyr are great data organization packages
library(tidyr)
library(dplyr)
library(pillar)

#Lubridate is a special package that allows us to do some cool stuff with dates and time arithmetic
library(lubridate)

#DescTools is a special package that allows for some cool functions
library(DescTools)


#Chapter 2: Import the water level elevation time series dataset

wlr <- read.csv("Input Data\\Water Level Elevations Time Series.csv")

glimpse(wlr)


# The logger name is recorded in the CSV and Image exports for all formatted datasets, 
# tidal regime statistics, flooding statistics, and water level elevation graphs in
# the whole R project. 'Logger_Name' has no impact on the functionality of the code. 

Logger_Name <- "Logger"


#Chapter 3: Format the Continuous Water Level Elevation dataset

# Convert the 'Date.Time' column to a POSIT data type to make data calculations in 'lubridate' 
# package easier to reduce the entire dataset to 30-day lunar tidal cycle

#Step 1: Conversion of Date.Time

wlr <- wlr %>%
  mutate(Date.Time = as.POSIXct(Date.Time, format = '%m/%d/%Y %H:%M'))

  glimpse(wlr)

  
#Step 2: Reduce the dataset to the 30 day lunar tidal cycle

#For research methods for the lab at University of New Hampshire, we normally deploy the WLRs for 
# roughly 30 - 40 days. We usually are not able to collect the WLRs perfectly at the end of the 
# lunar tidal cycle. We decided to subset the WLR data set to roughly a spring tidal cycle (~30 days). 
# We calculate the exact center date in the monitoring period and +/- 15 days. 

lunar_cycle <- data.frame(matrix(nrow = 1, ncol = 6)) %>%
  setNames(c("Site", 'Year', 'Start_Date', 'End_Date', 'Middle_Date', 'Deployment_Time')) %>%
  mutate( WLR = colnames(wlr)[2],
          Year = year(wlr$Date.Time[1]),
    
    #Deployment time is calculated as the total duration of the water level elevation dataset
    #Deployment time is calculated with the as.duration() function that converts the entire dataset to the number of seconds
    #Deployment time is then divided by the number of days to calculate the deployment time in days
        Deployment_Time = as.duration(min(wlr$Date.Time) %--% max(wlr$Date.Time))/ddays(1),
        Deployment_Time = round(Deployment_Time, 2),
    
    #Middle, Start, and End dates are calculated with the lubridate() package with quick mathematics of dates
    #Currently, start and end dates are calculated as +/- 15 days from the middle date
         Middle_Date = days(round(Deployment_Time / 2, 0)) + wlr$Date.Time[1],
         Start_Date = Middle_Date - days(15),
         End_Date = Middle_Date + days(15),
    
          Analysis_Time = as.duration(Start_Date %--% End_Date) / ddays(1))

write.csv(lunar_cycle,
          paste("Formatted Datasets\\", Logger_Name, "WLR Deployment Metadata.csv", 
                collapse = ""))


#Step 3: Next filter the 'wlr' dataset within the parameters of the start and end dates

wlr_format <- wlr %>%
  filter(Date.Time > lunar_cycle$Start_Date,
         Date.Time < lunar_cycle$End_Date)

#Step 4: Export the formatted continuous water level recorder dataset 

write.csv(wlr_format,
          paste("Formatted Datasets\\", Logger_Name, "WLR Formatted Dataset.csv", 
                collapse = ""))


#Continue onto the Creek Hydrology Analysis code to describe the tidal regime for each creek WLR

