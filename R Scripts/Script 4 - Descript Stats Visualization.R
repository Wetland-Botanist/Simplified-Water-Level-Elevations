# Project: Analysis of an individual conductivity meter 
# Script 4: Visualization of Descriptive Statistics
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


#Chapter 2: Import & Format Datasets ----------------------------------

# Step 1: User input name of Site, Year, and creek logger name (column name)

# User input data allows the program to easily retrieve the hydrology time series
# dataset from the Formatted Datasets folder without the user having to change
# the file path. The user input data are the same in Script 2 as well

# Logger_Name is the name of the salinity and conductivity probe 

Site = "Philbrook Pond" ; Year = "2026"

# Step 2: Import & Format daily salinity time series dataset
salinity_daily <- read.csv(paste("Formatted Datasets\\ ", Site, " - ", Year, "Salinity Daily Stats.csv")) %>%
  # Create and format the Date.Time column (with time now missing)
  mutate(Yearling = Year,
         Date.Time = paste(Yearling, "-", Month, "-", Day,
                           sep = ""),
         Date.Time = as.POSIXct(Date.Time, format = '%Y-%m-%d')) %>%
  # Remove miscellaneous columns
  select(-X, -Tidal_Cycle, -Yearling, -Day, -Month) %>%
  # Remove possible NA columns
  filter(!is.na(daily_mean_psu),
         !is.na(max_psu),
         !is.na(min_psu))

glimpse(salinity_daily)

# Step 3: Import and format lunar salinity time series dataset
salinity_lunar <- read.csv(paste("Formatted Datasets\\ ", Site, " - ", Year, "Salinity Lunar Stats.csv")) %>%
  select(-X, -lunar_se_psu)

glimpse(salinity_lunar)

# Step 4: Import & Format conductivity time series dataset
conductivity_daily <- read.csv(paste("Formatted Datasets\\ ", Site, " - ", Year, "Conductivity Daily Stats.csv")) %>%
  # Create and format the Date.Time column (with time now missing)
  mutate(Yearling = Year,
         Date.Time = paste(Yearling, "-", Month, "-", Day,
                           sep = ""),
         Date.Time = as.POSIXct(Date.Time, format = '%Y-%m-%d')) %>%
  # Remove miscellaneous columns
  select(-X, -Tidal_Cycle, -Yearling, -Day, -Month) %>%
  # Remove possible NA columns
  filter(!is.na(daily_mean_mS),
         !is.na(max_mS),
         !is.na(min_mS))

glimpse(conductivity_daily)

# Step 5: Import and format lunar salinity time series dataset
conductivity_lunar <- read.csv(paste("Formatted Datasets\\ ", Site, " - ", Year, "Conductivity Lunar Stats.csv")) %>%
  select(-X, -lunar_se_mS)

glimpse(conductivity_lunar)


# Chapter 3: Salinity Daily Visualization ----------------------------

# Step 1: Prep the salinity dataset to show all of the probes at a single site

salinity_prep <- salinity_daily %>%
  mutate(
    Loggers = str_replace(Loggers, "_", " "),
    Loggers = factor(Loggers, 
      levels = c("Garland Brook", "Phragmites", "West Platform")))

glimpse(salinity_prep)


# Step 2: Overlapping time series of mean, max, and min salinity (psu) visualization

salinity_mean_graph <- ggplot(data = salinity_prep, 
                    aes(x = Date.Time,
                        colour = Loggers,
                        fill = Loggers)) + 
  # Standard error bar for the daily salinity mean
  geom_ribbon(
    aes(ymin = daily_mean_psu - daily_se_psu,
         ymax = daily_mean_psu + daily_se_psu),
     alpha = 0.25) + 
  #Daily salinity mean line
  geom_line(aes(y = daily_mean_psu),
            size = 1.25) +
  # Viridis color pallette for the line colors
  scale_color_manual(values = c(
    "deepskyblue", "orange", "darkgreen")) + 
  scale_fill_manual(values = c(
    "deepskyblue", "orange", "darkgreen")) + 
  #X and Y Axis Labels
  labs(x = "", y = "Groundwater Salinity (psu)") +
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
    legend.position = c(0.90, 0.90),
    legend.title = element_blank(),
    legend.text = element_text(size = 18),
    legend.background = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 18),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(size = 18, colour = "black"),
    axis.text.y = element_text(size = 18, colour = "black"),
    axis.title.y = element_text(size = 18, colour = "black"))

salinity_mean_graph



#Save the graph to the 'Figures' Folder of the project
ggsave(salinity_mean_graph,
       filename = paste("Figures\\", Site, " - ", Year, " - Salinity Daily Mean Timeseries.jpg", sep = " "), 
       height = 9, width = 14, 
       units = "in", dpi = 300)


#Chapter 4: Conductivity Daily Visualization ----------------------------

# Step 1: Prep the salinity dataset to show all of the probes at a single site

conductivity_prep <- conductivity_daily %>%
  mutate(
    Loggers = str_replace(Loggers, "_", " "),
    Loggers = factor(Loggers, 
                     levels = c("Garland Brook", "Phragmites", "West Platform")))

glimpse(conductivity_prep)


# Step 2: Overlapping time series of mean, max, and min conductivity (mS/cm) visualization

conductivity_mean_graph <- ggplot(data = conductivity_prep, 
                              aes(x = Date.Time,
                                  colour = Loggers,
                                  fill = Loggers)) + 
  # Standard error bar for the daily conductivity mean
  geom_ribbon(
    aes(ymin = daily_mean_mS - daily_se_mS,
        ymax = daily_mean_mS + daily_se_mS),
    alpha = 0.25) + 
  #Daily conductivity mean line
  geom_line(aes(y = daily_mean_mS),
            size = 1.25) +
  # Viridis color pallette for the line colors
  scale_color_manual(values = c(
    "deepskyblue", "orange", "darkgreen")) + 
  scale_fill_manual(values = c(
    "deepskyblue", "orange", "darkgreen")) + 
  #X and Y Axis Labels
  labs(x = "", y = "Groundwater Specific Conductivity (mS/cm)") +
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
    legend.position = c(0.90, 0.90),
    legend.title = element_blank(),
    legend.text = element_text(size = 18),
    legend.background = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 18),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(size = 18, colour = "black"),
    axis.text.y = element_text(size = 18, colour = "black"),
    axis.title.y = element_text(size = 18, colour = "black"))

conductivity_mean_graph



#Save the graph to the 'Figures' Folder of the project
ggsave(conductivity_mean_graph,
       filename = paste("Figures\\", Site, " - ", Year, " - Conductivity Daily Mean Timeseries.jpg", sep = " "), 
       height = 9, width = 14, 
       units = "in", dpi = 300)



# Chapter 5: Salinity Lunar Visualization ------------------

# Step 1: Prep the lunar salinity dataset for visualization
salinity_lunar_prep <- salinity_lunar %>%
  pivot_longer(lunar_mean_psu:lunar_min_psu,
               names_to = "Metric",
               values_to = "Salinity_psu") %>%
  mutate(
    Metric = ifelse(Metric == "lunar_mean_psu",
                    "Mean Salinity", Metric),
    Metric = ifelse(Metric == "lunar_max_psu",
                    "Maximum Salinity", Metric),
    Metric = ifelse(Metric == "lunar_min_psu",
                    "Minimum Salinity", Metric)) %>%
  mutate(
    Loggers = str_replace(Loggers, "_", " "),
    Loggers = factor(Loggers, 
                     levels = c("Garland Brook", "Phragmites", "West Platform"))) %>%
  as.data.frame(.)
  
glimpse(salinity_lunar_prep)


# Step 2: Visualize the lunar salinity dataset

salinity_lunar_graph <- ggplot(
  data = salinity_lunar_prep,
  aes(x = Loggers,
      y = Salinity_psu)) + 
  geom_point(
    aes(colour = Loggers),
    size = 7, position = position_jitter(0.1), alpha = 0.75) +
  # Color palette for the loggers
  scale_color_manual(values = c(
    "deepskyblue", "orange", "darkgreen")) + 
  # y-axis label
  labs(y = "Groundwater Salinity (psu)") + 
  # Scale the y-axis by the minimum and maximum tidal water elevations
  scale_y_continuous(limits = c(-0.5, 35.1),
                     breaks = seq(0, 35, 5)) + 
  # All the fun text resizing and coloring
  theme_bw() +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(size = 18),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(size = 18, colour = "black"),
    axis.text.y = element_text(size = 18, colour = "black"),
    axis.title.y = element_text(size = 18, colour = "black"),
    axis.title.x = element_blank()) + 
  facet_wrap(~Metric,
             nrow = 2, ncol = 2)

salinity_lunar_graph

#Save the graph to the 'Figures' Folder of the project
ggsave(salinity_lunar_graph,
       filename = paste("Figures\\", Site, " - ", Year, " - Salinity Lunar Points.jpg", sep = " "), 
       height = 9, width = 12, 
       units = "in", dpi = 300)



# Chapter 6: Conductivity Lunar Visualization ------------------

# Step 1: Prep the lunar conductivity dataset for visualization
conductivity_lunar_prep <- conductivity_lunar %>%
  pivot_longer(lunar_mean_mS:lunar_min_mS,
               names_to = "Metric",
               values_to = "Conductivity_mS") %>%
  mutate(
    Metric = ifelse(Metric == "lunar_mean_mS",
                    "Mean Conductivity", Metric),
    Metric = ifelse(Metric == "lunar_max_mS",
                    "Maximum Conductivity", Metric),
    Metric = ifelse(Metric == "lunar_min_mS",
                    "Minimum Conductivity", Metric)) %>%
  mutate(
    Loggers = str_replace(Loggers, "_", " "),
    Loggers = factor(Loggers, 
                     levels = c("Garland Brook", "Phragmites", "West Platform"))) %>%
  as.data.frame(.)

glimpse(conductivity_lunar_prep)


# Step 2: Visualize the lunar conductivity dataset

conductivity_lunar_graph <- ggplot(
  data = conductivity_lunar_prep,
  aes(x = Loggers,
      y = Conductivity_mS)) + 
  geom_point(
    aes(colour = Loggers),
    size = 7, position = position_jitter(0.1), alpha = 0.75) +
  # Color palette for the loggers
  scale_color_manual(values = c(
    "deepskyblue", "orange", "darkgreen")) + 
  # y-axis label
  labs(y = "Groundwater Specific Conductivity (mS/cm)") + 
  # Scale the y-axis by the minimum and maximum tidal water elevations
  scale_y_continuous(limits = c(-0.5, 50),
                     breaks = seq(0, 50, 10)) + 
  # All the fun text resizing and coloring
  theme_bw() +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(size = 18),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(size = 18, colour = "black"),
    axis.text.y = element_text(size = 18, colour = "black"),
    axis.title.y = element_text(size = 18, colour = "black"),
    axis.title.x = element_blank()) + 
  facet_wrap(~Metric,
             nrow = 2, ncol = 2)

conductivity_lunar_graph

#Save the graph to the 'Figures' Folder of the project
ggsave(conductivity_lunar_graph,
       filename = paste("Figures\\", Site, " - ", Year, " - Conductivity Lunar Points.jpg", sep = " "), 
       height = 9, width = 12, 
       units = "in", dpi = 300)
