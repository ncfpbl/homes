#Title: Geocoding Homes in Sarasota
#Subtitle: Exact Location
#Author: Angela Kothe
#Date: May 5th, 2022
#Purpose: Attempting to Draw a Map of Homes in SRQ by Decade Built
#Requires: homesbuilt.dta from Sarasota County Accessor's Website
#Output: Internal
#Credit Where its Due: https://www.storybench.org/geocode-csv-addresses-r/

library(maptools)
library(rgeos)
library(tidyverse)
library(rgdal)
library(ggthemes)
library(socviz)
library(scales)
library(cowplot)
library(ggmap)
library(dplyr)
library(showtext)
library(sf)
library(extrafont)
library(haven)
library(here)

wd <- here("Documents", "Github", "pbl")

#data, tree height; zip code and roads shapefile
data <- read_dta(here(wd, "data", "addresses.dta"))


#my api; get your own
#register_google(key = "AIzaSyBaD64_Rhda_rzyRQRyb6u0z7rEeEskQSQ", write = TRUE)


# Loop through the addresses to get the latitude and longitude 
#of each address and add it to the

for(i in 1:nrow(data))
{
  result <- geocode(data$full_address[i], 
                    output = "latlona", source = "google")
  data$lon[i] <- as.numeric(result[1])
  data$lat[i] <- as.numeric(result[2])
  data$geoAddress[i] <- as.character(result[3])
}

#backup file
write.csv(data, here(wd, "data", "geocoded.csv"), row.names = FALSE)
