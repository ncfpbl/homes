#Title: Street Map 2.0
#Subtitle: Type
#Author: Angela Kothe
#Date: April 30th, 2022
#Purpose: Attempting to Draw a Map of Streets in SRQ by Type
#Requires: coast and street shapefiles from OpenSarasota Website
#Output: png files

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

#color palette
colors <- c("skyblue", "hotpink", "green", "red", "orange", "purple", 
            "yellow", "pink", "white")

#streets and roads shapefile; data retrieved from shape
s <- readOGR(here(wd, "shapes", "streets", "Street.shp"))
s <- spTransform(s, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
s.df <- fortify(s, region = "zipr")

data <- read_dta(here(wd, "data", "streets.dta"))
data <- data %>%
          rename(id = objectid)

#merge data with shapefile
data$id <- as.character(data$id)
data <- left_join(s.df, data, by = "id", sort = FALSE)
data$type[is.na(data$type)] = "Other"

#plot font
font_add_google(name = "Lato", family = "lato") 
showtext_auto()

#plot
ggplot() +
      geom_path(data = data, 
                aes(x = long, y = lat, group = group, color = type),
                size = .3, alpha = 1) +
      coord_sf(xlim = c(-82.7, -82.4), 
               ylim = c(27.2, 27.4),
               expand = FALSE) +
      scale_color_manual(values = colors) + 
      theme_map() +
      theme(legend.position = "top", 
            legend.justification = "center",
            legend.title = element_blank(),
            legend.text = element_text(color = "white", 
                                       size = 28, 
                                       family = "lato"),
            legend.key.height = unit(1, "cm"),
            legend.key.width = unit(1, "cm"),
            plot.title = element_text(size = 40, 
                                      family = "lato", 
                                      face = "bold", 
                                      hjust = .5,
                                      color = "white"),
            plot.background = element_rect(fill = "black", 
                                           color = "black"),
            plot.subtitle = element_text(family = "lato", 
                                         size = 16, 
                                         hjust = .5, 
                                         color = "white")) +
      labs(title = "STREETS IN SARASOTA", 
             subtitle = "Street Type")

ggsave(width = 15, height = 12, device = "png", 
       here(wd, "plots", "streettype.png"),
       bg = "black", dpi = 700)

