Analysis of Tidal Water Elevations of An Individual Water Level Recorder for Salt Marsh Monitoring

Authors: Grant McKown (james.mckown@unh.edu), Jennifer Gibson (jennifer.gibson@unh.edu)

Organization: Coastal Habitat Restoration Team, Jackson Estuarine Laboratory, University of New Hampshire

Purpose:

The R project was created to expedite the analysis of tidal water elevation - time series data. The package calculates common tidal hydrology metrics (mean low tide, high tide, etc.) and flooding parameters for given elevations of the salt marsh surface (flooding duration, high tide fooding frequency, etc.) for a single water level recorder over 30 days (single lunar cycle). The R project is designed for the water level recorder to have been deployed in a creek, ditch, or other hydrologic pathway that captures the local tidal hydrology. The R package is not designed nor appropriate for the analysis of a groundwater level recorder 

Any user is free (and encouraged!) to download and use the R project. Note of Warning - the code may be updated from time to time to ensure compatibility with R package updates and enhance capabilities. 

Monitoring Design Requirements:

The R code is specifically written to meet the then needs of broad hydrology monitoring protocols for salt marshes and coastal wetlands. The code is built on the R package 'VulnToolkit' by Troy Hill (https://cran.r-project.org/web/packages/VulnToolkit/index.html), which was specifically designed to help identify low and high tides and calculate flooding frequency and durations for given elevations. The R project is a wrapper for the 'VulnToolkit' to quickly analyze single water level recorders and calculate flooding statistics for given elevations. Additionally, the R package creates a nice graph with the ggplot2 package of the continuous water level elevation time series.

An example of Kents Island monitoring from 2021 by the Burdick Lab (University of New Hampshire) is provided and the filepaths in the code are preserved for the example. Users will need to change the input filepaths for their own hydrology dataset. 


Project Overview:

A brief description of the five R code scripts is provided:

1) Format Dataset - R script formats the water level elevation - time series dataset to allow for use in the rest of the R scripts

2) Tidal Hydrology Statistics of Creek Water Level Recorders - R script calculates mean low tide, high tide, higher high tide, and maximum tide elevation for the water level recorder

3) Flooding Statistics of Groundwater and Pool Water Level Recorders - R script calculates flooding duration (% of monitoring time) and high tide flooding frequency (%) for given elevations based on the water level recorder time series dataset.

4) Graphing Tidal Hydrology - R script graphs the continuous tidal water elevations over the 30 day study period (single lunar cycle) as well as reference lines for the marsh platform and root zone elevations.

5) Sparrow Islands - R scrpt calculates the flooding duration, frequency, and time not flooded for elevations of marsh mounds or sparrow islands. 

6) Visualize Compiled flooding Statistics - R script visualizes the trends of flooding statistics for groups of water level recorders over time for specified treatments.


Folders:

See individual README files in each folder for more in-depth details.

1) Input Data - folder the user of the R project will need to deposit the necessary datasets. See the folder README for in-depth details on the dataset needs for the R scripts. Example datasets are provided in the folder. R script is currently written to run with the example datasets. 
   
2) Dataset Templates - blank CSV files with appropriate column headers (and blank cells) for the needed input datasets
   
3) Formatted Datasets -  an intermediary folder that holds various water level elevation datasets that are created in one R script and utilized in the another R script'
   
4) Output Stats - destination of finalized tables for the descriptive statistics of tidal hydrology of creek, groundwater, and pool water level recorders and flooding for groundwater level recoders and constructed sparrow islands.
   
5) Figures - output of water level tidal elevation graphs in JPG format
