#Title: Homes Built in Sarasota 4.0
#Subtitle: Exact Location
#Author: Angela Kothe
#Date: May 15th, 2022
#Purpose: Attempting to Draw a Map of Homes in SRQ by Decade Built
#Requires: homesbuilt.dta from Sarasota County Accessor's Website
#Output: Internal

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
library(viridis)

wd <- here("Documents", "Github", "pbl")

#data, tree height; zip code and roads shapefile
data <- read_csv(here(wd, "data", "geocoded.csv"))

s <- readOGR(here(wd, "shapes", "streets", "Street.shp"))
s <- spTransform(s, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
s.df <- fortify(s, region = "zipr")

c <- readOGR(here(wd, "shapes", "coast", "CoastalRegulatoryLine.shp"))
c <- spTransform(c, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
c.df <- fortify(c)


font_add_google(name = "Lato", family = "lato") 
showtext_auto()

#Pools

ggplot(s.df) +
  geom_path(data = c, aes(long, lat, group = group), 
            colour = "skyblue", 
            size = .5) +
  geom_path(aes(long, lat, group = group), 
            colour = "black", 
            size = .2,
            alpha = .4) +
  coord_sf(xlim = c(-82.65, -82.4), 
           ylim = c(27.2, 27.4),
           expand = FALSE) +
  geom_point(data = na.omit(data), 
             aes(lon, lat, colour = pool), 
             alpha = .3,
             size = 1.5) +
  theme_map() +
  theme(legend.position = "none", 
        plot.title = element_text(size = 40, 
                                  family = "lato", 
                                  face = "bold", 
                                  hjust = .5,
                                  color = "black"),
        plot.background = element_rect(fill = "white", 
                                       color = NA),
        plot.subtitle = element_text(family = "lato", 
                                     size = 16, 
                                     hjust = .5, 
                                     color = "black")) +
  labs(title = "THE EXACT LOCATION OF POOLS IN SARASOTA")

ggsave(width = 15, height = 12, device = "png", 
       here("plots", "srqpools2.png"),
       bg = "white", dpi = 700)


#bedrooms*
colors <- c("orange", "#025b0e", "#9db802", "#000080", "#80b1d3", 
            "mediumvioletred", "mediumpurple1")

nbed <- read_dta(here(wd, "data", "nbedrooms.dta"))

bdata <- left_join(data, nbed, by = "full_address")

ggplot(s.df) +
  geom_path(data = c, aes(long, lat, group = group), 
            colour = "skyblue", 
            size = .5) +
  geom_path(aes(long, lat, group = group), 
            colour = "black", 
            size = .2,
            alpha = .4) +
  coord_sf(xlim = c(-82.65, -82.4), 
           ylim = c(27.2, 27.4),
           expand = FALSE) +
  geom_point(data = na.omit(bdata), 
             aes(lon, lat, colour = bedrooms.y), 
             position = position_jitter(h = 0.005), 
             alpha = .3,
             size = 1.5) +
  scale_fill_viridis_c(alpha = 1,
                       begin = 0,
                       end = 1,
                       direction = -1,
                       option = "H",
                       values = NULL,
                       space = "Lab",
                       na.value = "grey50",
                       guide = "colourbar",
                       aesthetics = "colour",
                       breaks = c(2, 3, 4, 5, 6, 7, 8, 9),
                       labels = c("", "3", "4", "5", 
                                  "6", "7", "8", ""),
                       limits = c(2, 9)) +
  theme_map() +
  theme(legend.position = "top", 
        legend.justification = "center",
        legend.title = element_blank(),
        legend.text = element_text(color = "black", 
                                   size = 15, 
                                   family = "lato"),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(3, "cm"),
        plot.title = element_text(size = 40, 
                                  family = "lato", 
                                  face = "bold", 
                                  hjust = .5,
                                  color = "black"),
        plot.background = element_rect(fill = "white", 
                                       color = NA),
        plot.subtitle = element_text(family = "lato", 
                                     size = 16, 
                                     hjust = .5, 
                                     color = "black")) +
  labs(title = "HOMES IN SARASOTA",
       subtitle = "NUMBER OF BEDROOMS")

ggsave(width = 15, height = 12, device = "png", 
       here(wd, "plots", "srqbedrooms2.png"),
       bg = "white", dpi = 700)

#build year (try continuous)
#data$buildYear <- as.character(data$buildYear)

colors2 <- c("orange", "#025b0e", "#9db802", "#000080", "#80b1d3", 
             "mediumvioletred", "mediumpurple1", "#c5e90b", "#f7d969",
             "#f4c2c2", "tomato", "hotpink", "black")

ydata <- subset(data, select = c(lon, lat, buildYear))

ydata <- na.omit(ydata)

ggplot(s.df) +
  geom_path(data = c, aes(long, lat, group = group), 
            colour = "skyblue", 
            size = .5) +
  geom_path(aes(long, lat, group = group), 
            colour = "black", 
            size = .2,
            alpha = .4) +
  coord_sf(xlim = c(-82.65, -82.4), 
           ylim = c(27.2, 27.4),
           expand = FALSE) +
  geom_point(data = na.omit(ydata), 
             aes(lon, lat, colour = buildYear), 
             alpha = .3,
             size = 1.5) +
  scale_fill_viridis_c(alpha = 1,
                       begin = 0,
                       end = 1,
                       direction = 1,
                       option = "A",
                       values = NULL,
                       space = "Lab",
                       na.value = "grey50",
                       guide = "colourbar",
                       aesthetics = "colour",
                       breaks = c(1870, 1900, 1930, 1960, 1990, 2020),
                       labels = c("1870", "1900", "1930", "1960", 
                                  "1990", "2020")) +
  theme_map() +
  theme(legend.position = "top", 
        legend.justification = "center",
        legend.title = element_blank(),
        legend.text = element_text(color = "black", 
                                   size = 15, 
                                   family = "lato"),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(3, "cm"),
        plot.title = element_text(size = 40, 
                                  family = "lato", 
                                  face = "bold", 
                                  hjust = .5,
                                  color = "black"),
        plot.background = element_rect(fill = "white", 
                                       color = NA),
        plot.subtitle = element_text(family = "lato", 
                                     size = 16, 
                                     hjust = .5, 
                                     color = "black")) +
  labs(title = "HOMES IN SARASOTA",
       subtitle = "YEAR BUILT")

ggsave(width = 15, height = 12, device = "png", 
       here(wd, "plots", "srqbuilt2.png"),
       bg = "white", dpi = 700)

#sold

ggplot(s.df) +
  geom_path(data = c, aes(long, lat, group = group), 
            colour = "skyblue", 
            size = .5) +
  geom_path(aes(long, lat, group = group), 
            colour = "black", 
            size = .2,
            alpha = .4) +
  coord_sf(xlim = c(-82.65, -82.4), 
           ylim = c(27.2, 27.4),
           expand = FALSE) +
  geom_point(data = na.omit(data), 
             aes(lon, lat, colour = sold), 
             alpha = .3,
             size = 1.5) +
  scale_fill_viridis_c(alpha = 1,
                       begin = 0,
                       end = 1,
                       direction = 1,
                       option = "A",
                       values = NULL,
                       space = "Lab",
                       na.value = "grey50",
                       guide = "colourbar",
                       aesthetics = "colour",
                       breaks = c(1920, 1940, 1960, 1980, 
                                  2000, 2020),
                       labels = c("1920", "1940", "1960", "1980", 
                                  "2000", "2020")) +
  theme_map() +
  theme(legend.position = "top", 
        legend.justification = "center",
        legend.title = element_blank(),
        legend.text = element_text(color = "black", 
                                   size = 15, 
                                   family = "lato"),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(3, "cm"),
        plot.title = element_text(size = 40, 
                                  family = "lato", 
                                  face = "bold", 
                                  hjust = .5,
                                  color = "black"),
        plot.background = element_rect(fill = "white", 
                                       color = NA),
        plot.subtitle = element_text(family = "lato", 
                                     size = 16, 
                                     hjust = .5, 
                                     color = "black")) +
        labs(title = "HOMES IN SARASOTA",
             subtitle = "LAST YEAR SOLD")

ggsave(width = 15, height = 12, device = "png", 
       here(wd, "plots", "srqsold2.png"),
       bg = "white", dpi = 700)

#assessed
value <- read_dta(here("data", "value.dta"))

data <- left_join(data, value, by = "full_address")

vdata <- subset(data, select = c(lon, lat, value))

vdata <- na.omit(vdata)

ggplot(s.df) +
  geom_path(data = c, aes(long, lat, group = group), 
            colour = "skyblue", 
            size = .5) +
  geom_path(aes(long, lat, group = group), 
            colour = "black", 
            size = .2,
            alpha = .4) +
  coord_sf(xlim = c(-82.65, -82.4), 
           ylim = c(27.2, 27.4),
           expand = FALSE) +
  geom_point(data = na.omit(vdata), 
             aes(lon, lat, colour = value), 
             alpha = .3,
             size = 1.8) +
  scale_color_gradient2(low = "yellow", 
                        mid = "darkblue", 
                        high = "red", 
                        midpoint = median(vdata$value),
                        #breaks = c(100, 500, 1000),
                        labels = c("200,000", "400,000", "600,000", 
                                   "800,000", "1,000,000"),
                        na.value = NA,
                        limits = c(200, 1000)) +
  theme_map() +
  theme(legend.position = "top", 
        legend.justification = "center",
        legend.title = element_blank(),
        legend.text = element_text(color = "black", 
                                   size = 15, 
                                   family = "lato"),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(3, "cm"),
        plot.title = element_text(size = 40, 
                                  family = "lato", 
                                  face = "bold", 
                                  hjust = .5,
                                  color = "black"),
        plot.background = element_rect(fill = "white", 
                                       color = NA),
        plot.subtitle = element_text(family = "lato", 
                                     size = 16, 
                                     hjust = .5, 
                                     color = "black")) +
  labs(title = "HOMES IN SARASOTA",
       subtitle = "ASSESSED VALUE")

ggsave(width = 15, height = 12, device = "png", 
       here("plots", "srqassessed2.png"),
       bg = "white", dpi = 700)
