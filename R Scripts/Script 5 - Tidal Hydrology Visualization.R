# Project: Analysis of an individual water level recorder for salt marsh monitoring
# Script 4: Tidal and Groundwater Visualization
# Authors: Jenny Gibson (jennifer.gibson@unhs.edu), Grant McKown (james.mckown@unh.edu)

# Last Updated: March 9th, 2026


# Script Description: 
# Script 5 visualizes the 30-day continuous water level elevations of the selected
# creek and groundwater level recorders to show concurrent surface tidal hydrology
# and groundwater dynamics. The marsh platform and root zone (~ 5 cm belowground)
# are also shown for context. If sparrow elevations are desired, the graph has
# built-in (yet hashed) lines to show those elevations instead. 


#Chapter 1: Package Library ----------------------------

#Data Organization
library(tidyr)
library(dplyr)
library(lubridate)
library(DescTools)

#Data visualization
library(ggplot2)
library(scales)

#Chapter 2: Import Datasets ----------------------------------

#Task 1 - User input name of Site, Year, and creek logger name (column name)

# User input data allows the program to easily retrieve the hydrology time series
# dataset from the Formatted Datasets folder without the user having to change
# the file path. The user input data are the same in Script 2 as well

# Logger name = water level recorder column header
# Creek logger = creek water level recorder column header

Logger_Name = "NAC" ; Creek_Logger = "Creek" ; Site = "Kents Island" ; Year = "2025"

# Task 2 - Import hydrology time series dataset

wlr_format <- read.csv(paste("Formatted Datasets\\", Site, Year, "WLR Formatted Dataset.csv", 
                      collapse = "")) %>%
  # Remove miscellaneous empty column that is created when dataset is exported to csv
  select(-X) %>%
  # Format the Date.Time column
  mutate(Date.Time = as.POSIXct(Date.Time, format = '%Y-%m-%d %H:%M:%S')) %>%
  # Rename the creek and groundwater level recorders in question for easier coding
  rename(WLR = Logger_Name,
        Creek = Creek_Logger) %>%
  # QAQC to remove any blank rows
  filter(!is.na(Creek_Logger),
         !is.na(WLR),
         !is.na(Date.Time))

glimpse(wlr_format)


# Task 3 - Import the elevation dataset of marsh platform and root zone

# User needs to create a csv file of the elevations of the marsh platform and root zone
# related to the groundwater level recorders in the site. Each row will be consist
# of the water logger name, marsh platform elevation, and root zone elevation (5 cm belowground)

# Elevation dataset is needed to compute flooding frequency, drainage duration, and
# drainage depth


elevs <- read.csv("Input Data\\Kents Island WLR Elevations Example.csv") %>%
  filter(WLR == Logger_Name) %>%
  # Root Zone is calculated at 5 cm belowground (can be altered)
  mutate(Rootzone_Elevation = Marsh_Elevation - 0.05)

glimpse(elevs)


#Chapter 2: Hydrology Visualization ----------------------------

options(scipen = 999)

Tidegraph <- ggplot(data = wlr_format, 
                    aes(x = Date.Time)) + 
  #Creek level elevation line
  geom_line(aes(y = Creek),
                linewidth = 1.25, colour = "blue") + 
  #Groundwater level elevation line
  geom_line(aes(y = WLR),
            linewidth = 1.25, colour = "orange") + 
  #Marsh platform elevation reference line
  geom_hline(yintercept = elevs$Marsh_Elevation, linetype = "dashed", 
             color = "black", linewidth = 1.25) +
  #Marsh root zone elevation reference line
  geom_hline(yintercept = elevs$Rootzone_Elevation, linetype = "dashed", 
             color = "green", linewidth = 1.25) +
  #X and Y Axis Labels
  labs(x = "", y = "Water Elevation (NAVD88 m)") +
  #Scale the x-axis by the minimum and maximum date and times
  scale_x_datetime(limits = c(wlr_format$Date.Time[which.min(wlr_format$Date.Time)] - days(1), 
                              wlr_format$Date.Time[which.max(wlr_format$Date.Time)] + days(1) ),
                   expand = c(0, 0),
                   breaks = "3 days",
                   date_labels = "%m-%d") +
  #Scale the y-axis by the minimum and maximum tidal water elevations
  scale_y_continuous(limits = c(0.4, 2.05),
                     breaks = seq(0.4, 2.0, 0.2), 
                     labels = scales::label_number(accuracy = 0.1)) + 
  #All the fun text resizing and coloring
  theme_bw() +
  theme(
    #legend.position = c(0.125, 0.95),
    #legend.text = element_text(size = 20),
    #legend.title = element_blank(),
    #legend.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(size = 20, colour = "black"),
    axis.text.y = element_text(size = 20, colour = "black"),
    axis.title.y = element_text(size = 20, colour = "black"))

Tidegraph



#Save the graph to the 'Figures' Folder of the project

ggsave(Tidegraph,
       filename = paste("Output Figures\\", Site, Logger_Name, Year, " Water Level Elevations Graph.jpg", sep = " "), 
       height = 9, width = 14, 
       units = "in", dpi = 300)

