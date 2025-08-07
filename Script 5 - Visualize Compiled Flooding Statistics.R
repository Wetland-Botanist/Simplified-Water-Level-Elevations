#Project: Analysis of an individual water level recorder for salt marsh monitoring

#Code: Script 5 - Visualizing Compiled Flooding Statistics Over Time

#Authors: Grant McKown (james.mckown@unh.edu)

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


# Script 5 (this code) of the 'Simplified Water Level Recorder Analysis', visualizes flooding statistics over time
# of various water level recorders, typically of the same treatment, to better understand long-term trends
# of groundwater and pool hydrology. Mean +/- standard error of flooding statistics are calculated
# for each monitoring season and each treatment. 


# Data Inputs of Script 5:

# (1) Compiled Flooding Statistics of Water Level Recorders

# Details of the Compiled Flooding Statistics Dataset
# Marsh - name of the salt marsh site
# Site - name of the sub-site within the larger salt marsh site (optional)
# WLR - name of the specific water level recorder
# Overall_Treatment - Name of the treatment of the water level recorder (e.g., Reference, Restoration, etc.)
# Year - Monitoring season of the flooding statistics
# Creek - Associated Creek (Optional)
# Elevation_NAVD88m - Elevation of the marsh platform for the water level recorder of that monitoring season
# Platform_Flood_Dur - Flooding duration (%) of the marsh platform elevation
# Platform_HT_Freq - High tide flooding frequency (%) of the marsh platform
# Platfrom_Drain_Depth - Drainage depth (m) of the marsh platform
# Root_Flood_Dur - Flooding duration (%) of the root zone
# Root_HT_Freq - High tide flooding frequency 9%) of the root zone
# Root_Drain_Depth - Drainage depth (m) of the root zone
# Drain_Amplitude - Drainage amplitude (m) of the water level recorder


# User Inputs of Script 5 (in Chapter 2 of the script):
# (1) Site Name 
# (2) Treatment 

# Site Name will be used to name exported tables at the end of Script 1. 
# Treatment will reduce the dataset or water level recorders to the selected treatment. 


# Chapter 1: Package Library --------------------------------------------------

#Tidyr and dplyr are great data organization packages
library(tidyr)
library(dplyr)
library(pillar)
library(stringr)

#Lubridate is a special package that allows us to do some cool stuff with dates and time arithmetic
library(lubridate)

#DescTools is a special package that allows for some cool functions
library(DescTools)

#VulnToolkit is a special package that does all the meat and potatoes for this R-script
library(ggplot2)
library(scales)


# Chapter 2: Import datasets and user generated inputs -----------------------------------------

#Step 1 - User generated inputs

Site_Name <- "Kents Island"

Treatment <- "Runnel1"

# Step 2 - Import the compiled flooding statistics dataset

gw_hydro_data <- read.csv("Input Data\\Essex Compiled Hydrology Statistics.csv")

glimpse(gw_hydro_data)



# Chapter 3: Calculate averages of hydrology statistics ------------------------- 

# Step 1 - Caluclate averages +/- standard error of flooding statistics 


gw_hydro_summary <- gw_hydro_data %>%
  #Convert drainage depth and drainage amplitude metrics from meters to centimeters
  mutate(across(c(Platform_Drain_Depth, 
                 Root_Drain_Depth, Drain_Amplitude),
                ~ . * 100)) %>%
  # Calculate mean and standard error of flooding statistics by Year and Treatment
  group_by(Overall_Treatment, Year) %>%
  summarise(
    across(Platform_Flood_Dur:Drain_Amplitude,
           list(
             mean = ~mean(.),
             se = ~sd(.)/sqrt(n()) ))) %>%
  ungroup() %>%
  # Round flooding statistics to 1 decimal point
  mutate(across(c(Platform_Flood_Dur_mean : Drain_Amplitude_se),
                ~round(., 1)))

glimpse(gw_hydro_summary)

# Step 2 - Compiled averages and standard errors into a formatted table

# Averages and standard errors combined into one single table 

gw_hydro_summary_format <- gw_hydro_summary %>%
  mutate(
    Platform_Flood_Dur = paste(Platform_Flood_Dur_mean, Platform_Flood_Dur_se, sep = " +/- "),
    Platform_HT_Freq = paste(Platform_HT_Freq_mean, Platform_HT_Freq_se, sep = " +/- "),
    Platform_Drain_Depth = paste(Platform_Drain_Depth_mean, Platform_Drain_Depth_se, sep = " +/- "),
    Root_Flood_Dur = paste(Root_Flood_Dur_mean, Root_Flood_Dur_se, sep = " +/- "),
    Root_HT_Freq = paste(Root_HT_Freq_mean, Root_HT_Freq_se, sep = " +/- "),
    Root_Drain_Depth = paste(Root_Drain_Depth_mean, Root_Drain_Depth_se, sep = " +/- "),
    Drain_Amplitude = paste(Drain_Amplitude_mean, Drain_Amplitude_se, sep = " +/- ")) %>%
  dplyr::select(Overall_Treatment, Year,
                Platform_Flood_Dur:Drain_Amplitude)

glimpse(gw_hydro_summary_format)


# Export the formatted table to the 'Output Stats' Folder
write.csv(gw_hydro_summary_format,
          paste("Output Stats\\", Site_Name, " Overall Treatment GW Stats.csv", sep = ""))


# Chapter 4: Visualize the Flooding Statistics Over Time -------------------------


# Step 1 - Format the flooding statistics dataset for visualization (reduce by treatment)

# Flooding Statistics dataset is compiled in Microsoft Excel after running all water level recorders of 
# a Trustees site for all monitoring years. The dataset is composed of water level recorder, overall treatment, and 
# the calculated metrics of flooding duration, high tide frequency, and drainage depth for marsh platform and root zone
# elevation. 

flooding_stats <- gw_hydro_data %>%
  filter(Marsh == Site_Name) %>%
  filter(str_detect(Treatment_Name, "Runnel")) %>%
  mutate(across(c(Platform_Drain_Depth, Root_Drain_Depth, Drain_Amplitude),
                ~ . * 100)) %>%
  mutate(Year = as.character(Year))

glimpse(flooding_stats)


# Step 1 - Flooding Duration

#Task 1: Format the flooding statistics dataset for the flooding duration graph

# The main transformation of the dataset is going from a wide --> long dataset

flood_dur_format <- flooding_stats %>%
  select(Treatment_Name, Overall_Treatment, Year, Platform_Flood_Dur, Root_Flood_Dur) %>%
  gather(Platform_Flood_Dur:Root_Flood_Dur, 
         key = "Zone",
         value = "Flood_Duration") %>%
  mutate(Zone = ifelse(Zone == "Platform_Flood_Dur", "Marsh Platform", "Root Zone")) 

# Task 2: Graph the flooding duration over time for selected treatment

flood_dur_viz <- ggplot(
  data = flood_dur_format,
  aes(x = Year,
      y = Flood_Duration)) +
  geom_point(aes(fill = Treatment_Name),
    shape = 21,
    size = 8, position = position_jitter(0.1)) +
  labs(x = "",
       y = "Flooding Duration Time (%)") +
  scale_y_continuous(
    limits = c(0, 102),
    breaks = seq(0, 100, 20)) + 
    theme_bw() + 
    theme(
      legend.position = c(0.15, 0.15),
      legend.background = element_blank(),
      legend.title = element_blank(),
      legend.text = element_text(size = 18, colour = "black"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.title = element_text(size = 18, colour = "black"),
      axis.text = element_text(size = 18, colour = "black"), 
      strip.background = element_blank(),
      strip.text = element_text(size = 18)) +
    facet_wrap(~Zone,
               nrow = 2)

flood_dur_viz


# Task 3: Export the figure to the 'Figures' Folder
ggsave(flood_dur_viz,
       filename = paste("Figures\\", Site_Name, Treatment, " Flooding Duration Graph.jpg", sep = " "), 
       height = 10, width = 12, 
       units = "in", dpi = 300)



# Step 3 - High Tide Flooding Frequency

# Task 1: Format the flooding statistics dataset for visualization

ht_freq_format <- flooding_stats %>%
  select(Treatment_Name, Overall_Treatment, Year, Platform_HT_Freq, Root_HT_Freq) %>%
  gather(Platform_HT_Freq:Root_HT_Freq, 
         key = "Zone",
         value = "High_Tide_Freq") %>%
  mutate(Zone = ifelse(Zone == "Platform_HT_Freq", "Marsh Platform", "Root Zone"))

# Task 2 - Visualize high tide flooding frequency by treatment over time 

ht_freq_viz <- ggplot(
  data = ht_freq_format,
  aes(x = Year,
      y = High_Tide_Freq)) +
  geom_point(aes(fill = Treatment_Name),
             shape = 21, 
             size = 8, position = position_jitter(0.1)) +
  labs(x = "",
       y = "High Tide Flooding Frequency (%)") +
  scale_y_continuous(
    limits = c(0, 102),
    breaks = seq(0, 100, 20)) + 
  theme_bw() + 
  theme(
    legend.position = c(0.15, 0.90),
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 18, colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 18, colour = "black"),
    axis.text = element_text(size = 18, colour = "black"), 
    strip.background = element_blank(),
    strip.text = element_text(size = 18)) +
  facet_wrap(~Zone,
             nrow = 2)

ht_freq_viz


# Task 3: Export the figure to the 'Figures' Folder
ggsave(ht_freq_viz,
       filename = paste("Figures\\", Site_Name, Treatment, " High Tide Frequency Graph.jpg", sep = " "), 
       height = 10, width = 12, 
       units = "in", dpi = 300)



# Step 4 - Drainage Depth

# Task 1: Format the flooding statistics dataset for visualization
drain_depth_format <- flooding_stats %>%
  select(Treatment_Name, Overall_Treatment, Year, Platform_Drain_Depth, Root_Drain_Depth) %>%
  gather(Platform_Drain_Depth:Root_Drain_Depth, 
         key = "Zone",
         value = "Drain_Depth") %>%
  mutate(Zone = ifelse(Zone == "Platform_Drain_Depth", "Marsh Platform", "Root Zone")) %>%
  mutate(Drain_Depth = Drain_Depth * -1) %>%
  filter(Zone == "Root Zone")

# Task 2: Visualize drainage depth by treatment over time

drain_depth_viz <- ggplot(
  data = drain_depth_format,
  aes(x = Year,
      y = Drain_Depth)) +
    geom_hline(yintercept = 0, linetype = "dashed", 
             color = "black", size = 1.25) +
  geom_point(aes(fill = Treatment_Name),
             shape = 21, 
             size = 8, position = position_jitter(0.1)) +
  labs(x = "",
       y = "Low Tide Drainage Depth (cm)") +
  scale_y_continuous(
    limits = c(-10, 32),
    breaks = seq(-10, 32, 10)) + 
  theme_bw() + 
  theme(
    legend.position = c(0.35, 0.85),
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 18, colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 18, colour = "black"),
    axis.text = element_text(size = 18, colour = "black"), 
    strip.background = element_blank(),
    strip.text = element_text(size = 18)) +
  facet_wrap(~Zone,
             nrow = 2)

drain_depth_viz

# Task 3: Export the figure to the 'Figures' Folder
ggsave(drain_depth_viz,
       filename = paste("Figures\\", Site_Name, Treatment, " Root Zone Drainage Depth Graph.jpg", sep = " "), 
       height = 8, width = 12, 
       units = "in", dpi = 300)



#Step 5 - Drainage Amplitude

# Task 1: Format the flooding statistics dataset for visualization

drain_amplitude_format <- flooding_stats %>%
  select(Treatment_Name, Overall_Treatment, Year, Drain_Amplitude)

# Task 2: Visualize drainage depth by treatment over time 

drain_amplitude_viz <- ggplot(
  data = drain_amplitude_format,
  aes(x = Year,
      y = Drain_Amplitude)) +
  geom_point(aes(fill = Treatment_Name),
             shape = 21, 
             size = 8, position = position_jitter(0.1)) +
  labs(x = "",
       y = "Drainage Amplitude (cm)") +
  scale_y_continuous(
    limits = c(-0.5, 20),
    breaks = seq(0, 20, 5)) + 
  theme_bw() + 
  theme(
    legend.position = c(0.15, 0.775),
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 18, colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 18, colour = "black"),
    axis.text = element_text(size = 18, colour = "black"), 
    strip.background = element_blank(),
    strip.text = element_text(size = 18))

drain_amplitude_viz

# Task 3: Export the figure to the 'Figures' Folder
ggsave(drain_amplitude_viz,
       filename = paste("Figures\\", Site_Name,Treatment, "Drainage Amplitude Graph.jpg", sep = " "), 
       height = 8, width = 12, 
       units = "in", dpi = 300)



# End of 'Simplified Water Level Recorder Analysis' Scripts.
