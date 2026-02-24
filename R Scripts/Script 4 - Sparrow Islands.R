#Project: Analysis of an individual water level recorder for salt marsh monitoring

#Script: Part 6 - Analysis of Tidal Flooding on Constructed Sparrow Islands

#Authors: Grant McKown (james.mckown@unh.edu), Jennifer Gibson (jennifer.gibson@unh.edu)

#Organization: Jackson Estuarine Laboratory, University of New Hampshire


#General Explanation of Code:
# The code is largely a product of the VulnToolkit package created by Troy Hill at the EPA. He devised a fantastic 
# method to analyze the duration of inundation and frequency of inundation for a given elevation. Additionally,
# the VulnToolkit calculates the elevation of the average and maximum high and low tides. 

# The overall purpose of the code is to analyze water level elevation data to accurately describe and summarise
# the tidal hydrology regime for salt marsh systems and groundwater regimes at individual water level recorders

#The fourth part of the code is to analyze the flooding conditions of the constructed sparrow islands. Sparrow islands
# are piles of excavated peat from restoration activities designed to provide refuge to nesting saltmarsh sparrows
# above the Mean Higher High Water elevation of the local creek

# Essentially, the code calculates the flooding frequency, flooding duration, elevation above mean high higher water, 
# and maximum time between flooding events (or a maximum dry period) to gauge island success. Most of the code is 
# similar to the groundwater level recorder analysis code with the functions used. Additionally, note that the 
# hydrology analysis is performed with the creek water level recorders, not the groundwater water level recorders. 


#Chapter 1: Package Library ----------------------------------------------------

#Tidyr and dplyr are great data organization packages
library(tidyr)
library(dplyr)
library(pillar)
library(purrr)
library(ggplot2)

#Lubridate is a special package that allows us to do some cool stuff with dates and time arithmetic
library(lubridate)

#DescTools is a special package that allows for some cool functions
library(DescTools)

#VulnToolkit is a special package that does all the meat and potatoes for this R-script
library(VulnToolkit)


#Chapter 2: Import Hydrology and Sparrow Island Elevation Datasets------------------

# Task 1: User required data and names

Site <- "Kents Island" ; Year <- "2025"

Creek_Logger_Name = "Creek"


#Task 1: Import Sparrow Island Elevation dataset

#Sparrow island elevations dataset includes individual island, island elevation, island group, and 
# respective creek water level recorder

sparrow <- read.csv("Input Data\\Kents Island Sparrow Elevations 2025.csv")

glimpse(sparrow)


# Task 2: Import the formatted Water Elevation dataset

#Please note the water elevation profile used in this script is the Raw Water Elevations (input data in Script 1),
# not the formatted and reduced water elevation profiles (~ 28 day length). We want to know the maximum number of 
# days throughout the monitoring timeframe. Reduction to just the 28 days does not provide an accurate representation of
# maximum consecutive dry days, especially if spring tide flooding is in the middle of the dataset. 


wlr <- read.csv("Input Data\\Kents Island 2025 Compiled WLRs R.csv") %>%
  mutate(Date.Time = as.POSIXct(Date.Time, format = '%m/%d/%Y %H:%M')) %>%
  rename(Creek_Logger = contains("Creek"))

glimpse(wlr)


# Task 3: Create  high tide - low tide creek hydrology dataset 

# High and low tides for the entire creek hydrology dataset is created, similar to
# the protocol followed in Script 2 (for the 30-day dataset)

tides <- HL(level = wlr$Creek_Logger, 
            time = wlr$Date.Time, period = 12.5) %>%
  rename(Date.Time = time) %>%
  mutate(Creek_Logger = Creek_Logger_Name)

glimpse(tides)

# Step 2 - Quality Control

#First check if there are consecutive high or low tides in the dataset
tides_qaqc <- tides %>%
  # Calculate previous tidal stage using lag() function
  mutate(previous_tide = lag(tide, 1)) %>%
  # Remove subsequent low tide from the dataset
  filter(!(tide == "L" & previous_tide == "L")) %>%
  # Remove subsequent high tide from the dataset
  filter(!(tide == "H" & previous_tide == "H")) %>%
  # Remove duplicates (not sure why this happened...)
  distinct(Date.Time, .keep_all = TRUE) %>%
  dplyr::select(-previous_tide)

glimpse(tides_qaqc)


#Chapter 3: Calculate hydrology metrics for the sparrow island elevations-------

#Task 1: Calculate Flooding Duration (%) of each sparrow island

#First, we need to create a mega dataset where each sparrow island has a respective water elevation profile
# by merging the water elevation profile dataset (reduced to creek water logger) and the sparrow island
# elevation datasets

wlr_format <- wlr %>%
  #Select only the creek water level recorders
  dplyr::select(Date.Time, Creek_Logger) %>%
  gather(key = Creek_Logger, value = elev, 
         colnames(select(., Creek_Logger))) %>%
  mutate(Creek_Logger = "Creek") %>%
  #Combines the sparrow island dataset with the formatted creek WLR dataset, so now
  #each sparrow island is associated with a full water level elevation dataset in the monitoring period
  merge(dplyr::select(sparrow, Island, Creek_Logger, Elevation), 
        by = "Creek_Logger") %>%
  dplyr::select(Island, Elevation, Date.Time, Creek_Logger, elev) %>%
  rename(sparrow_elev = "Elevation") %>%
  arrange(Island, Date.Time)

#Second, we can now easily calculate flooding duration of each sparrow island 
# using summarise(), fld.dur functions

sparrow_flood <- wlr_format %>%
  group_by(Island) %>%
  summarise(
         island_flood = fld.dur(z = sparrow_elev[1],
                                    level = elev) * 100,
         island_elev = sparrow_elev[1]) %>%
  ungroup() %>%
  mutate(island_flood = as.numeric(island_flood),
           island_flood = round(island_flood, 1))

glimpse(sparrow_flood)


#Task 2: Calculate High Tide Flooding Frequency of each sparrow island

#We can do this quickly with a similar approach of the flooding duration through
# first merging the creek logger tidal hydrology (low - high tides) and sparrow island elevations
# datasets. Second, we can then use summarise and fld.frq functions to calculate high tide
# flooding frequency 

sparrow_freq <- tides %>%
  filter(tide == "H") %>%
  merge(dplyr::select(sparrow, Island, Elevation, Creek_Logger), 
        by = "Creek_Logger") %>%
  group_by(Island) %>%
  summarise(island_freq = fld.frq(z = Elevation[1], 
                                  ht = tides$level),
            island_elevation = Elevation[1]) %>%
  ungroup() %>%
  mutate(island_freq = round(island_freq, 2) * 100)

glimpse(sparrow_freq)

#Task 3: Maximum Time Period Between Island Flooding Events (Maximum Dry Period)

# One of the main metrics of the sparrow construction is remaining dry (or not flooding) for at least several weeks
# to allow for sparrow fledglings to hatch and retreat from flooding

#Calculate the max dry period with the diff.date() function, which calculates the difference in two time periods
# and can be used as a substitute for a for loop when processing through an entire column.
# We want to calculate the maximum difference between two chronological time periods when the island was flooded

sparrow_dry <- wlr_format %>%
  group_by(Island) %>%
  #Sets the first and last water elevation to 2.5 NAVD88 m to artificially bound the analysis
  # The analysis will start counting days at the first time and end the counting at the last time
  mutate(elev = ifelse(row_number() == 1, 2.5, 
                       ifelse(row_number() == nrow(.), 2.5, elev))) %>%
  #Keeps only the times that the island was flooded
  filter(elev > sparrow_elev) %>%
  #Sometimes there might be an NA inserted into the dataset, remove them
  filter(!is.na(Date.Time)) %>%
  #Calculates maximum time difference through the entire dataset
  summarise(island_dry_days = max(diff.Date(Date.Time))/ddays(1),
            island_elevation = sparrow_elev[1]) %>%
  mutate(island_dry_days = as.numeric(island_dry_days),
         island_dry_days = round(island_dry_days, 2)) %>%
  ungroup()

glimpse(sparrow_dry)


#Chapter 4: Wrap up the Sparrow Island Stats into one useful table-------------

# Task 1: Compile all of the statistics and sparrow island information into one table

sparrow_stats <- sparrow %>%
  #Series of merging tables to the original sparrow island table,
  # For ease of reading, I broke up the code for each hydrology metric
  merge(select(sparrow_flood, Island, island_flood), by = "Island") %>%
  merge(select(sparrow_freq, Island, island_freq), by = "Island") %>%
  merge(select(sparrow_dry, Island, island_dry_days), by = "Island") %>%
  rename(Flooding_Duration_Percent = island_flood,
         HT_Frequency_Percent = island_freq,
         Max_Dry_Period_days = island_dry_days) %>%
  dplyr::select(Island, Island_Group, Creek_Logger, Elevation,
         Flooding_Duration_Percent, HT_Frequency_Percent, Max_Dry_Period_days)

glimpse(sparrow_stats)

#Task 2: Export the sparrow island statistics table to Excel

write.csv(sparrow_stats,
          paste("Output Stats\\", Site, Year, "Sparrow Island Hydrology Stats.csv",
                collapse = ""))


#Chapter 5: Calculate descriptive statistics for Island Group------------------

island_stats <- sparrow_stats %>%
  group_by(Island_Group) %>%
  summarise(across(Flooding_Duration_Percent:Max_Dry_Period_days,
            list(
             mean = ~ mean(., na.rm = TRUE),
             se = ~ sd(., na.rm = TRUE) / sqrt(n())
            ))) %>%
  ungroup() %>%
  mutate(across(Flooding_Duration_Percent_mean:Max_Dry_Period_days_se,
                ~round(., 1))) %>%
  mutate(
    Flooding_Duration = paste(Flooding_Duration_Percent_mean, Flooding_Duration_Percent_se,
                              sep = " +/- "),
    Flood_Frequency = paste(HT_Frequency_Percent_mean, HT_Frequency_Percent_se,
                            sep = " +/- "),
    Max_Days_Dry = paste(Max_Dry_Period_days_mean, Max_Dry_Period_days_se,
                         sep = " +/- ")) %>%
  dplyr::select(Island_Group, Flooding_Duration, Flood_Frequency, Max_Days_Dry)

glimpse(island_stats)

write.csv(island_stats,
          paste("Output Stats\\", Site, Year, "Sparrow Island Group Hydrology Stats.csv",
                collapse = ""))



#Chapter 2: Graph the tidal water elevations

options(scipen = 999)

Tidegraph <- ggplot(data = wlr, 
                    aes(x = Date.Time)) + 
  #Creek level elevation line
  geom_line(aes(y = Creek),
            linewidth = 1.25, colour = "blue") + 
  #Marsh platform elevation reference line
  geom_hline(yintercept = 1.348, 
             linetype = "dashed",
             color = "black", linewidth = 1.25) +
  #Marsh root zone elevation reference line
  geom_hline(yintercept = 1.298, 
             linetype = "dashed", 
             color = "green", linewidth = 1.25) +
  # Mean Sparrow Island Elevation line
  geom_line(aes(y = 1.427),
            linewidth = 1.25, linetype = "dashed",
            colour = "brown") + 
    #X and Y Axis Labels
  labs(x = "", y = "Water Elevation (NAVD88 m)") +
  #Scale the x-axis by the minimum and maximum date and times
  scale_x_datetime(limits = c(wlr$Date.Time[which.min(wlr$Date.Time)] - days(1), 
                              wlr$Date.Time[which.max(wlr$Date.Time)] + days(1) ),
                   expand = c(0, 0),
                   breaks = "5 days",
                   date_labels = "%m-%d") +
  #Scale the y-axis by the minimum and maximum tidal water elevations
  scale_y_continuous(limits = c(0.6, 2.2),
                     breaks = seq(0.6, 2.2, 0.20), 
                     labels = scales::label_number(accuracy = 0.10)) + 
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

ggsave(Tidegraph,
       filename = paste("Figures\\", Site, Year, " Sparrow Island Flooding Graph.jpg", sep = " "), 
       height = 9, width = 14, 
       units = "in", dpi = 300)












