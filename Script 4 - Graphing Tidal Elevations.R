#Project: Analysis of an individual water level recorder for salt marsh monitoring

#Code: Script 4 - Visualization of Tidal Hydrology

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


# Script 4 (this code) of the 'Simplified Water Level Recorder Analysis', visualizes the water elevation profiles
# of a groundwater/pool level recorder and associated creek water level recorder in the 30 day monitoring period. 


# Data Inputs of Script 4:

# (1) Formatted Water Level Series Dataset (created and exported in Script 1)
# (2) Marsh Platform and Root Zone Elevations of Groundwater/Pool Water Level Recorders (user provided)


# User Inputs of Script 4 (in Chapter 2 of the script):
# (1) Site Name 
# (2) Year
# (3) Groundwater/Pool Water Level Recorder Name
# (4) Creek Water Level Recorder Name

# Site Name and Year will be used to name exported tables at the end of Script 1. 
# Water level recorder name will be used to select the specific WLR for analysis. 


# Chapter 1: Package Library --------------------------------------------------

#Tidyr and dplyr are great data organization packages
library(tidyr)
library(dplyr)
library(pillar)

#Lubridate is a special package that allows us to do some cool stuff with dates and time arithmetic
library(lubridate)

#DescTools is a special package that allows for some cool functions
library(DescTools)

#VulnToolkit is a special package that does all the meat and potatoes for this R-script
library(ggplot2)
library(scales)



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



# Step 2: Import the formatted water elevation time series dataset

wlr_format <- read.csv(paste("Formatted Datasets\\", Site, Year, "WLR Formatted Dataset.csv", 
                    collapse = ""))  %>%
  select(-X) %>%
  mutate(Date.Time = as.POSIXct(Date.Time, format = '%Y-%m-%d %H:%M:%S')) %>%
  rename(WLR = Logger_Name,
        Creek = Creek_Logger) %>%
  filter(!is.na(Creek_Logger),
         !is.na(WLR),
         !is.na(Date.Time))

glimpse(wlr_format)


#Step 3: Import the marsh platform and root zone elevation dataset

elevs <- read.csv("Input Data\\Little River WLR Elevations.csv") %>%
  filter(WLR == Logger_Name)

glimpse(elevs)



#Chapter 3: Graph the tidal water elevations --------------------------------

options(scipen = 999)

# Step 1 - Visualize the water elevations with ggplot()

Tidegraph <- ggplot(data = wlr_format, 
                    aes(x = Date.Time)) + 
  #Creek level elevation line (blue)
  geom_line(aes(y = Creek),
                linewidth = 1.25, colour = "blue") + 
  #Groundwater level elevation line (orange)
  geom_line(aes(y = WLR),
            linewidth = 1.25, colour = "orange") + 
  #Marsh platform elevation reference line (dashed black)
  geom_hline(yintercept = elevs$Marsh_Elevation, linetype = "dashed", 
             color = "black", size = 1.25) +
  #Marsh root zone elevation reference line (dashed green)
  geom_hline(yintercept = elevs$Rootzone_Elevation, linetype = "dashed", 
             color = "green", size = 1.25) +
  #X and Y Axis Labels
  labs(x = "", y = "Water Elevation (NAVD88 m)") +
  #Scale the x-axis by the minimum and maximum date and times of the monitoring period (+/- 1 day)
  scale_x_datetime(limits = c(wlr_format$Date.Time[which.min(wlr_format$Date.Time)] - days(1), 
                              wlr_format$Date.Time[which.max(wlr_format$Date.Time)] + days(1) ),
                   expand = c(0, 0),
                   breaks = "3 days",
                   date_labels = "%m-%d") +
  #Scale the y-axis by the minimum and maximum tidal water elevations
  scale_y_continuous(limits = c(0.4, 1.7),
                     breaks = seq(0.4, 1.7, 0.20), 
                     labels = scales::label_number(accuracy = 0.10)) + 
  #All the fun text resizing and coloring
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(size = 20, colour = "black"),
    axis.text.y = element_text(size = 20, colour = "black"),
    axis.title.y = element_text(size = 20, colour = "black"))

Tidegraph



#Step 2 - Export the figure to the 'Figures' folder

ggsave(Tidegraph,
       filename = paste("Figures\\", Site, Logger_Name, Year, " Water Level Elevations Graph.jpg", sep = " "), 
       height = 9, width = 14, 
       units = "in", dpi = 300)


# Once a specific water level recorder has been processed for a number of monitoring seasons,
# please proceed to Script 5 - Visualize Compiled Flooding Statistics

# If the tidal hydrology should be analyzed with respect to marsh mound or sparrow island elevations,
# please proceed to Script 6 - Sparrow Islands
