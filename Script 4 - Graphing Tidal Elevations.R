#Project: Analysis of an individual water level recorder for salt marsh monitoring

#Code: Part 5 - Graphing Tidal Water Elevations

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


# Script 4 (this code), graphs the 30-day continuous water level elevations from the 'Formatted WLR 
# dataset' with horizontal reference lines of the marsh platform and rootzone elevation. The code
# is highly editable by the user to format the graph to their purposes. The code for the graph was
# designed for automation. 


#Chapter 1: Library of packages for necessary work

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


#Chapter 2: Import Datasets

#To graph, we will need to import all three datasets:
# 1) Formatted water level elevation dataset
# 2) Individual water level recorder and elevation dataset
# 3) Sparrow island elevation dataset

#Import formatted water level elevation dataset


#First input is the formatted water level elevations from the Reformat R Code
# The code removes the pesky "X" column created to label row names, then 
# formats the Date.Time column to a POSIXct format

wlr_format <- read.csv(paste("Formatted Datasets\\", Logger_Name, "WLR Formatted Dataset.csv", 
                      collapse = "")) %>%
  select(-X) %>%
  mutate(Date.Time = as.POSIXct(Date.Time, format = '%Y-%m-%d %H:%M:%S'))

glimpse(wlr_format)


#Second input is the marsh platform and root zone elevations

#USER INPUT NEEDED:
elevs <- read.csv("Input Data\\Marsh and Root Zone Elevations.csv")

glimpse(elevs)


#Chapter 2: Graph the tidal water elevations


Tidegraph <- ggplot(data = wlr_format, 
                    aes(x = Date.Time, y = WLR)) + 
  #Water level elevation line
  geom_line(size = 1.25, colour = "blue") + 
  #Marsh platform elevation reference line
  geom_hline(yintercept = elevs$Marsh_Elevation, linetype = "dashed", 
             color = "black", size = 1.25) +
  #Marsh root zone elevation reference line
  geom_hline(yintercept = elevs$Rootzone_Elevation, linetype = "dashed", 
             color = "green", size = 1.25) +
  #X and Y Axis Labels
  labs(x = "", y = "Water Elevation (NAVD88 m)") +
  #Scale the x-axis by the minimum and maximum date and times
  scale_x_datetime(limits = c(min(wlr_format$Date.Time), 
                              max(wlr_format$Date.Time)),
                   breaks = "3 days",
                   date_labels = "%m-%d") +
  #Scale the y-axis by the minimum and maximum tidal water elevations
  scale_y_continuous(breaks = seq(round(min(wlr_format$WLR), 1) - 0.20, 
                                  round(max(wlr_format$WLR), 1) + 0.1, 0.10)) + 
  #All the fun text resizing and coloring
  theme_bw() +
  theme(
    legend.position = c(0.125, 0.95),
    legend.text = element_text(size = 18),
    legend.title = element_blank(),
    legend.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = 0, size = 18, colour = "black"),
    axis.text.y = element_text(size = 18, colour = "black"),
    axis.title.y = element_text(size = 18, colour = "black"))

Tidegraph



#Save the graph to the 'Figures' Folder of the project

ggsave(Tidegraph,
       filename = paste("Figures\\", Logger_Name, " Water Level Elevations Graph.jpg", sep = ""), 
       height = 806, width = 1536, 
       units = "px", dpi = 125)
