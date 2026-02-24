# Project: Atlantic Coast Joint Venture - Assessment of Runnel Effectiveness
# Analysis: Groundwater Hydrology Analysis
# Script: Water Level Recorder Processing - Visualization of Site-level Flooding Metrics

#Authors: Grant McKown (james.mckown@unh.edu)

# Script Description: Script 5 visualizes the flooding metrics across all groundwater 
# level recorders of a given site for quick comparison. If a site is monitored repeatedly
# over time, it can also be used to see how the site changed over time. 

# Script requires the user to compile the results of the flooding metrics 
# into a csv file. 


# Chapter 1: Library of packages for necessary work -------------

# Data organization and formatting
library(tidyr)
library(dplyr)
library(pillar)
library(stringr)
library(lubridate)
library(DescTools)

# Data visualization
library(ggplot2)
library(scales)


#Chapter 2: Import Site Flooding Metrics Dataset -------------

# Step 1: User provided name of the site 

Site_Name <- "Kents Island"

# Flooding Statistics dataset is compiled in Microsoft Excel after running all water level recorders of 
# a Trustees site for all monitoring seasons. 

# The dataset is composed of the following columns:
# (1) Water level recorder name 
# (2) Treatment
# (3) Columns for each of the flooding metrics (flooding duration, flooding frequency, drainage depth, and drainage amplitude)

# Step 2 - Import the compiled flooding metrics dataset

flooding_stats <- read.csv("Input Data\\Trustees Flooding Statistics.csv") %>%
  filter(Marsh == Site_Name)

glimpse(flooding_stats)


# Chapter 3: Flooding Duration ------------

#Step 1 - Format the flooding statistics dataset for the flooding duration graph

# The main transformation of the dataset is going from a wide --> long dataset

flood_dur_format <- flooding_stats %>%
  select(Treatment_Name, Overall_Treatment, Season, Platform_Flood_Dur, Root_Flood_Dur) %>%
  gather(Platform_Flood_Dur:Root_Flood_Dur, 
         key = "Zone",
         value = "Flood_Duration") %>%
  mutate(Zone = ifelse(Zone == "Platform_Flood_Dur", "Marsh Platform", "Root Zone")) %>%
  filter(!str_detect(Treatment_Name, "Runnel"))

# Step 2 - Visualize the flooding duration

flood_dur_viz <- ggplot(
  data = flood_dur_format,
  aes(x = Season,
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
      legend.position = c(0.125, 0.925),
      legend.background = element_blank(),
      legend.title = element_blank(),
      legend.text = element_text(size = 16, colour = "black"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.title = element_text(size = 16, colour = "black"),
      axis.text = element_text(size = 16, colour = "black"), 
      strip.background = element_blank(),
      strip.text = element_text(size = 16)) +
    facet_wrap(~Zone,
               nrow = 2)

flood_dur_viz



ggsave(flood_dur_viz,
       filename = paste("Figures\\", Site_Name,"Non-Runnel Flooding Duration Graph.jpg", sep = " "), 
       height = 10, width = 12, 
       units = "in", dpi = 300)



# Chapter 4: Flooding Frequency Frequency ----------------------------

#Step 1 - Format the flooding statistics dataset for the flooding frequency graph
ht_freq_format <- flooding_stats %>%
  select(Treatment_Name, Overall_Treatment, Season, Platform_HT_Freq, Root_HT_Freq) %>%
  gather(Platform_HT_Freq:Root_HT_Freq, 
         key = "Zone",
         value = "High_Tide_Freq") %>%
  mutate(Zone = ifelse(Zone == "Platform_HT_Freq", "Marsh Platform", "Root Zone"))%>%
  filter(!str_detect(Treatment_Name, "Runnel"))

# Step 2 - Visualize the flooding frequency
ht_freq_viz <- ggplot(
  data = ht_freq_format,
  aes(x = Season,
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
    legend.position = c(0.125, 0.15),
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 16, colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 16, colour = "black"),
    axis.text = element_text(size = 16, colour = "black"), 
    strip.background = element_blank(),
    strip.text = element_text(size = 16)) +
  facet_wrap(~Zone,
             nrow = 2)

ht_freq_viz



ggsave(ht_freq_viz,
       filename = paste("Figures\\", Site_Name," Non-runnel High Tide Frequency Graph.jpg", sep = " "), 
       height = 10, width = 12, 
       units = "in", dpi = 300)



#Chapter 5: Drainage Depth ------------------------

#Step 1 - Format the flooding statistics dataset for drainage depth
drain_depth_format <- flooding_stats %>%
  select(Treatment_Name, Overall_Treatment, Season, Platform_Drain_Depth, Root_Drain_Depth) %>%
  gather(Platform_Drain_Depth:Root_Drain_Depth, 
         key = "Zone",
         value = "Drain_Depth") %>%
  mutate(Zone = ifelse(Zone == "Platform_Drain_Depth", "Marsh Platform", "Root Zone")) %>%
  # Flips the graph in order to better understand drainage depth from a normal perspective
  mutate(Drain_Depth = Drain_Depth * -1) %>%
  filter(!str_detect(Treatment_Name, "Runnel")) %>%
  filter(Zone == "Marsh Platform")

# Step 2 - Visualize drainage depth
drain_depth_viz <- ggplot(
  data = drain_depth_format,
  aes(x = Season,
      y = Drain_Depth)) +
    geom_hline(yintercept = 0, linetype = "dashed", 
             color = "black", size = 1.25) +
  geom_point(aes(fill = Treatment_Name),
             shape = 21, 
             size = 8, position = position_jitter(0.1)) +
  labs(x = "",
       y = "Low Tide Drainage Depth (cm)") +
  scale_y_continuous(
    limits = c(-20, 10),
    breaks = seq(-30, 20, 5)) + 
  theme_bw() + 
  theme(
    legend.position = c(0.10, 0.90),
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 14, colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 16, colour = "black"),
    axis.text = element_text(size = 16, colour = "black"), 
    strip.background = element_blank(),
    strip.text = element_text(size = 16)) +
  facet_wrap(~Zone,
             nrow = 2)

drain_depth_viz


ggsave(drain_depth_viz,
       filename = paste("Figures\\", Site_Name," Non-Runnel Drainage Depth Graph.jpg", sep = " "), 
       height = 8, width = 12, 
       units = "in", dpi = 300)



#Chapter 6: Drainage Amplitude ----------

#Step 1 - Format the flooding statistics dataset for the flooding duration graph
drain_amplitude_format <- flooding_stats %>%
  select(Treatment_Name, Overall_Treatment, Season, Drainage_Amplitude)

# step 2 - Visualize drainage amplitude
drain_amplitude_viz <- ggplot(
  data = drain_amplitude_format,
  aes(x = Season,
      y = Drainage_Amplitude)) +
  geom_point(aes(fill = Treatment_Name),
             shape = 21, 
             size = 8, position = position_jitter(0.1)) +
  labs(x = "",
       y = "Drainage Amplitude (cm)") +
  scale_y_continuous(
    limits = c(-0.5, 25),
    breaks = seq(0, 25, 5)) + 
  theme_bw() + 
  theme(
    legend.position = c(0.125, 0.85),
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 14, colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 16, colour = "black"),
    axis.text = element_text(size = 16, colour = "black"), 
    strip.background = element_blank(),
    strip.text = element_text(size = 16))

drain_amplitude_viz


ggsave(drain_amplitude_viz,
       filename = paste("Figures\\", Site_Name,"Drainage Amplitude Graph.jpg", sep = " "), 
       height = 10, width = 14, 
       units = "in", dpi = 300)
