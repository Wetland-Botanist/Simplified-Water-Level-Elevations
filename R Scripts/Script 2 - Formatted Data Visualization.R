# Project: Analysis of an individual conductivity meter 
# Script 2: Visualization of Formatted Datasets of Salinity and Conductivity
# Authors: Grant McKown (james.mckown@unh.edu)

# Last Updated: June 2026

#Project Description:


# Script Description: 



#Chapter 1: Package Library ------------------------------------

# Data Organization and Formatting
library(tidyverse)
library(DescTools)

# Data Visualization
library(ggplot2)
library(scales)


#Chapter 2: Import Datasets ----------------------------------

# Step 1: User input name of Site, Year, and creek logger name (column name)

# User input data allows the program to easily retrieve the hydrology time series
# dataset from the Formatted Datasets folder without the user having to change
# the file path. The user input data are the same in Script 2 as well

# Logger_Name is the name of the salinity and conductivity probe 

Site = "Philbrook Pond" ; Year = "2026"

# Step 1: Import salinity time series dataset

salinity <- read.csv(paste("Formatted Datasets\\ ", Site, " - ", Year, " - Salinty psu Formatted.csv")) %>%
  # Remove miscellaneous empty column that is created when dataset is exported to csv
  # Format the Date.Time column
  mutate(Date.Time = as.POSIXct(Date.Time, format = '%Y-%m-%d %H:%M:%S')) %>%
  # QAQC to remove any blank rows
  filter(!is.na(Date.Time)) %>%
  # Remove miscellaneous column
  select(-X, -Tidal_Cycle)

glimpse(salinity)

# Step 3: Import the conductivity time series dataset
conductivity <- read.csv(paste("Formatted Datasets\\ ", Site, " - ", Year, " - Conductivity mS-cm Formatted.csv")) %>%
  # Remove miscellaneous empty column that is created when dataset is exported to csv
  # Format the Date.Time column
  mutate(Date.Time = as.POSIXct(Date.Time, format = '%Y-%m-%d %H:%M:%S')) %>%
  # QAQC to remove any blank rows
  filter(!is.na(Date.Time)) %>%
  # Remove miscellaneous column
  select(-X, -Tidal_Cycle)


glimpse(conductivity)


#Chapter 3: Salinity Time Series Visualization ----------------------------

# Step 1: Prep the salinity dataset to show all of the probes at a single site

salinity_prep <- salinity %>%
  pivot_longer(
    2:ncol(.),
    names_to = "Logger",
    values_to = "Salinity_psu") %>%
  mutate(
    Logger = str_replace(Logger, "_", " "),
    Logger = factor(Logger, 
      levels = c("Garland Brook", "Phragmites", "West Platform")))

glimpse(salinity_prep)


# Step 2: Overlapping time series of salinity (psu) visualization

salinity_graph <- ggplot(data = salinity_prep, 
                    aes(x = Date.Time,
                        y = Salinity_psu)) + 
  #Groundwater level elevation line
  geom_line(aes(colour = Logger),
            linewidth = 1.25) + 
  #X and Y Axis Labels
  labs(x = "", y = "Groundwater Salinity (psu)") +
  # Viridis color pallette for the line colors
  scale_color_manual(values = c(
    "deepskyblue", "orange", "darkgreen")) + 
  #Scale the x-axis by the minimum and maximum date and times
  scale_x_datetime(limits = c(salinity_prep$Date.Time[which.min(salinity_prep$Date.Time)] - days(1), 
                              salinity_prep$Date.Time[which.max(salinity_prep$Date.Time)] + days(1) ),
                   expand = c(0, 0),
                   breaks = "5 days",
                   date_labels = "%m-%d") +
  #Scale the y-axis by the minimum and maximum tidal water elevations
  scale_y_continuous(limits = c(0, 32.1),
                     breaks = seq(0, 32, 4)) + 
  #All the fun text resizing and coloring
  theme_bw() +
  theme(
    legend.position = c(0.90, 0.925),
    legend.text = element_text(size = 18),
    legend.title = element_blank(),
    legend.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(size = 20, colour = "black"),
    axis.text.y = element_text(size = 20, colour = "black"),
    axis.title.y = element_text(size = 20, colour = "black"))

salinity_graph



#Save the graph to the 'Figures' Folder of the project
ggsave(salinity_graph,
       filename = paste("Figures\\", Site, " - ", Year, " - Salinity psu Timeseries.jpg", sep = " "), 
       height = 9, width = 14, 
       units = "in", dpi = 300)


#Chapter 4: Conductivity Time Series Visualization ----------------------------

# Step 1: Prep the Conductivity dataset to show all of the probes at a single site

conductivity_prep <- conductivity %>%
  pivot_longer(
    2:ncol(.),
    names_to = "Logger",
    values_to = "Conductivity_mS") %>%
  mutate(
    Logger = str_replace(Logger, "_", " "),
    Logger = factor(Logger, 
                    levels = c("Garland Brook", "Phragmites", "West Platform")))

glimpse(conductivity_prep)


# Step 2: Overlapping time series of Conductivity (mS / cm) visualization

conductivity_graph <- ggplot(data = conductivity_prep, 
                         aes(x = Date.Time,
                             y = Conductivity_mS)) + 
  #Groundwater level elevation line
  geom_line(aes(colour = Logger),
            linewidth = 1.25) + 
  #X and Y Axis Labels
  labs(x = "", y = "Specific Conductivity (mS / cm)") +
  # Viridis color pallette for the line colors
  scale_color_manual(values = c(
    "deepskyblue", "orange", "darkgreen")) + 
  #Scale the x-axis by the minimum and maximum date and times
  scale_x_datetime(limits = c(conductivity_prep$Date.Time[which.min(conductivity_prep$Date.Time)] - days(1), 
                              conductivity_prep$Date.Time[which.max(conductivity_prep$Date.Time)] + days(1) ),
                   expand = c(0, 0),
                   breaks = "5 days",
                   date_labels = "%m-%d") +
  #Scale the y-axis by the minimum and maximum tidal water elevations
  scale_y_continuous(limits = c(0, 51),
                     breaks = seq(0, 50, 5)) + 
  #All the fun text resizing and coloring
  theme_bw() +
  theme(
    legend.position = c(0.90, 0.925),
    legend.text = element_text(size = 18),
    legend.title = element_blank(),
    legend.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(size = 20, colour = "black"),
    axis.text.y = element_text(size = 20, colour = "black"),
    axis.title.y = element_text(size = 20, colour = "black"))

conductivity_graph



#Save the graph to the 'Figures' Folder of the project
ggsave(conductivity_graph,
       filename = paste("Figures\\", Site, " - ", Year, " - Conductivity mS-cm Timeseries.jpg", sep = " "), 
       height = 9, width = 14, 
       units = "in", dpi = 300)

