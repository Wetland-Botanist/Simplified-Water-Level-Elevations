#Project: Analysis of an individual water level recorder for salt marsh monitoring

#Code: Part 3 - Flooding frequency and duration for marsh platform and root zone elevations

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


# In Script 3 (this code) of the 'Simplified Water Level Recorder Analysis', users provide
# elevations of the marsh platform and root zone (5 cm below ground) in the 'Marsh and Root Zone
# Elevations' input dataset. The code uses the Formatted Water Level Series and the Tidal Regime 
# Water Level Series (list of low and high tides), created in Script 2, to calculate the flooding 
# frequency and flooding duration for given marsh elevations. 

# In the end, the code compiles the flooding statistics and exports them into an easy-to-read CSV file.


#Chapter 1: Library of packages for necessary work

#Tidyr and dplyr are great data organization packages
library(tidyr)
library(dplyr)
library(pillar)
library(purrr)

#Lubridate is a special package that allows us to do some cool stuff with dates and time arithmetic
library(lubridate)

#DescTools is a special package that allows for some cool functions
library(DescTools)

#VulnToolkit is a special package that does all the meat and potatoes for this R-script
library(VulnToolkit)




#Chapter 2: Import the needed datasets for Script 3

# The needed datasets are: (1) Formatted continuous water level elevations series,
#                          (2) Tidal regime series (list of low and high tides), and
#                          (3) Marsh Platform and Rootzone Elevations (user provided)

# The first input is the formatted continuous water level elevation series

wlr_format <- read.csv(paste("Formatted Datasets\\", Logger_Name, "WLR Formatted Dataset.csv", 
                      collapse = "")) %>%
  mutate(Date.Time = as.POSIXct(Date.Time, format = '%Y-%m-%d %H:%M:%S')) %>%
  select(-X) 

glimpse(wlr)


#The second input is the tidal hydrology (low and high tides) of the water level recorders

tides <- read.csv(paste("Formatted Datasets\\", Logger_Name, "Tidal Hydrology Dataset.csv", 
                        collapse = "")) %>%
  select(-X) %>%
  mutate(Date.Time = as.POSIXct(time, format = '%Y-%m-%d %H:%M:%S')) %>%
  select(-time)

glimpse(tides)


#The third input is a dataframe that contains the elevations of the marsh platform and root zone

elevs <- read.csv("Input Data\\Marsh and Root Zone Elevations.csv")

glimpse(elevs)



#Chapter 3: Calculate the Flooding Duration of the Marsh Platform and Rootzone Elevations

#First, assess the flooding duration of the marsh platform and root zone elevation

#To accomplish using the fld.dur() function, which returns the flooding duration (as a percent of monitoring time)

#Additionally, we will create two new columns that "pulls" the marsh platform and root zone elevations from
#the 'elevs' dataframe.

wlr_flood <- wlr_format %>%
  summarise(marsh_flood = fld.dur(z = elevs$Marsh_Elevation,
                                  level = WLR) * 100,
            root_flood = fld.dur(z = elevs$Rootzone_Elevation,
                                 level = WLR) * 100,
            
            marsh_elev = elevs$Marsh_Elevation,
            
            
            root_elev = elevs$Rootzone_Elevation) %>%
  ungroup() %>%
  mutate(across(marsh_flood:root_flood, ~round(., 1)))



# Reformat the flooding duration for easier input into the overall statistics table later on (Chapter 4)

wlr_flood_format <- wlr_flood %>%
  select(-marsh_elev, -root_elev) %>%
  gather(key = zone, value = Flood_Duration_Percent, marsh_flood, root_flood) %>%
  mutate(zone = ifelse(zone == "marsh_flood", "Marsh Platform", "Root Zone")) %>%
  arrange(zone)


#Second, assess the high tide flooding frequency of the marsh platform and root zone elevations

#We calculate the high tide flooding frequency (%) with the fld.frq() function
# The function requires two inputs:   
# (1) z = tidal hydrology (water elevations) from the data set
# (2) ht = list of high tides

# We will pull the ht (list of high tides) from the wlr_tides dataset we created earlier in the code

wlr_freq <- tides %>%
  filter(tide == "H") %>%
  select(-tide2, -Date.Time) %>%
  summarise(marsh_freq = fld.frq(z = elevs$Marsh_Elevation, ht = level) *100,
            
            root_freq = fld.frq(z = elevs$Rootzone_Elevation, ht = level) * 100,
            
            marsh_elev = elevs$Marsh_Elevation,
            
            root_elev = elevs$Rootzone_Elevation) %>%
  
  ungroup() %>%
  #Flooding frequency columns are in a 'list' format, reformat to double and round to 2 decimal points
  mutate(across(c(marsh_freq, root_freq), ~as.numeric(.)),
         across(c(marsh_freq, root_freq), ~round(., 1)))

glimpse(wlr_freq)


# Reformat the high tide flooding frequency for easier input into the overall statistics table later on (Chapter 9)
wlr_freq_format <- wlr_freq %>%
  select(-marsh_elev, -root_elev) %>%
  gather(key = zone, value = HT_Frequency_Percent, marsh_freq, root_freq) %>%
  mutate(zone = ifelse(zone == "marsh_freq", "Marsh Platform", "Root Zone")) %>%
  arrange(zone)



#Chapter 5: Combine flooding statistics into an exportable CSV file

flooding_stats <- cbind(elevs,
                   select(wlr_flood_format, Flood_Duration_Percent),
                   select(wlr_freq_format, HT_Frequency_Percent))


write.csv(flooding_stats,
          paste("Output Stats\\", Logger_Name, "Hydrology Flooding Stats.csv", 
                collapse = ""))


