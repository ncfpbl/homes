#Title: Homes Built in Sarasota 3.0
#Subtitle: Exact Location
#Author: Angela Kothe
#Date: May 5th, 2022
#Purpose: Attempting to Draw a Map of Homes in SRQ by Decade Built
#Requires: homesbuilt.dta from Sarasota County Accessor's Website
#Output: Internal
#Credit Where its Due: https://blog.cultureofinsight.com/2017/06/
#building-dot-density-maps-with-uk-census-data-in-r/

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

#add viridis scales

#data, tree height; zip code and roads shapefile
data <- read_csv(here("data", "geocoded.csv"))

s <- readOGR(here("shapes", "streets", "Street.shp"))
s <- spTransform(s, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
s.df <- fortify(s, region = "zipr")

c <- readOGR(here("shapes", "coast", "CoastalRegulatoryLine.shp"))
c <- spTransform(c, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
c.df <- fortify(c)


font_add_google(name = "Lato", family = "lato") 
showtext_auto()

#
ggplot() +
  geom_point(data = data, 
             aes(lon, lat, colour = "black"), 
             #position = position_jitter(h = 0.5, w = 0.5),
             size = .1)

#make a pool map
#data$pool[is.na(data$pool)] = "0"

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
             #position = position_jitter(h = 0.005),
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


#bedrooms
colors <- c("orange", "#025b0e", "#9db802", "#000080", "#80b1d3", 
            "mediumvioletred", "mediumpurple1")

ggplot(s.df) +
  geom_path(data = c, aes(long, lat, group = group), 
            colour = "skyblue", 
            size = .5) +
  geom_path(aes(long, lat, group = group), 
            colour = "darkgrey", 
            size = .3) +
  coord_sf(xlim = c(-82.7, -82.4), 
           ylim = c(27.2, 27.4),
           expand = FALSE) +
  geom_point(data = na.omit(data), 
             aes(lon, lat, colour = bedrooms), 
             size = .1,
             position = position_jitter(h = 0.005),
             alpha = .6) +
  scale_colour_manual(values = colors) +
  theme_map() +
  theme(legend.position = "top", 
        legend.justification = "center",
        legend.title = element_blank(),
        legend.text = element_text(color = "black", 
                                   size = 28, 
                                   family = "lato"),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm"),
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
  guides(colour = guide_legend(override.aes = list(size = 2.5))) +
  labs(title = "HOMES IN SARASOTA",
       subtitle = "NUMBER OF BEDROOMS")

ggsave(width = 15, height = 12, device = "png", 
       here(wd, "plots", "srqbedrooms.png"),
       bg = "white", dpi = 700)

#build year
data$buildYear <- as.character(data$buildYear)

colors2 <- c("orange", "#025b0e", "#9db802", "#000080", "#80b1d3", 
            "mediumvioletred", "mediumpurple1", "#c5e90b", "#f7d969",
            "#f4c2c2", "tomato", "hotpink", "black")

ggplot(s.df) +
  geom_path(data = c, aes(long, lat, group = group), 
            colour = "skyblue", 
            size = .5) +
  geom_path(aes(long, lat, group = group), 
            colour = "darkgrey", 
            size = .3) +
  coord_sf(xlim = c(-82.7, -82.4), 
           ylim = c(27.2, 27.4),
           expand = FALSE) +
  geom_point(data = na.omit(data), 
             aes(lon, lat, colour = buildYear), 
             size = .1,
             position = position_jitter(h = 0.005),
             alpha = .8) +
  scale_colour_manual(values = colors2) +
  theme_map() +
  theme(legend.position = "top", 
        legend.justification = "center",
        legend.title = element_blank(),
        legend.text = element_text(color = "black", 
                                   size = 28, 
                                   family = "lato"),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm"),
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
  guides(colour = guide_legend(override.aes = list(size = 2.5))) +
  labs(title = "HOMES IN SARASOTA",
       subtitle = "YEAR BUILT")

ggsave(width = 15, height = 12, device = "png", 
       here(wd, "plots", "srqbuilt.png"),
       bg = "white", dpi = 700)

#sold
colors3 <- c("orange", "#025b0e", "#9db802", "#000080", "#80b1d3", 
            "mediumvioletred", "mediumpurple1", "#c5e90b", "#f7d969",
            "#f4c2c2", "tomato")

data$sold <- as.character(data$sold)

data.1920 <- filter(data, sold == "1920")
data.1930 <- filter(data, sold == "1930")
data.1940 <- filter(data, sold == "1940")
data.1950 <- filter(data, sold == "1950")
data.1960 <- filter(data, sold == "1960")
data.1970 <- filter(data, sold == "1970")
data.1980 <- filter(data, sold == "1980")
data.1990 <- filter(data, sold == "1990")
data.2000 <- filter(data, sold == "2000")
data.2010 <- filter(data, sold == "2010")
data.2020 <- filter(data, sold == "2020")

ggplot(s.df) +
  geom_path(data = c, aes(long, lat, group = group), 
            colour = "skyblue", 
            size = .5) +
  geom_path(aes(long, lat, group = group), 
            colour = "darkgrey", 
            size = .3) +
  coord_sf(xlim = c(-82.7, -82.4), 
           ylim = c(27.2, 27.4),
           expand = FALSE) +
  geom_point(data = data.2010, aes(lon, lat, colour = sold), 
             position = position_jitter(h = 0.005),
             size = .1, alpha = 1) +
  geom_point(data = data.2020, aes(lon, lat, colour = sold),
             position = position_jitter(h = 0.005),
             size = .1, alpha = .6) +
  geom_point(data = data.2000, aes(lon, lat, colour = sold), 
             position = position_jitter(h = 0.005),
             size = .1, alpha = .6) +
  geom_point(data = data.1990, aes(lon, lat, colour = sold), 
             position = position_jitter(h = 0.005),
             size = .1, alpha = .6) +
  geom_point(data = data.1980, aes(lon, lat, colour = sold), 
             position = position_jitter(h = 0.005),
             size = .1, alpha = .6) +
  geom_point(data = data.1970, aes(lon, lat, colour = sold), 
             position = position_jitter(h = 0.005),
             size = .1, alpha = .6) +
  geom_point(data = data.1960, aes(lon, lat, colour = sold), 
             position = position_jitter(h = 0.005),
             size = .1, alpha = .6) +
  geom_point(data = data.1950, aes(lon, lat, colour = sold), 
             position = position_jitter(h = 0.005),
             size = .1, alpha = .6) +
  geom_point(data = data.1940, aes(lon, lat, colour = sold), 
             position = position_jitter(h = 0.005),
             size = .1, alpha = .6) +
  geom_point(data = data.1930, aes(lon, lat, colour = sold), 
             position = position_jitter(h = 0.005),
             size = .1, alpha = .6) +
  geom_point(data = data.1920, aes(lon, lat, colour = sold), 
             position = position_jitter(h = 0.005),
             size = .1, alpha = .6) +
  scale_colour_manual(values = colors3) +
  theme_map() +
  theme(legend.position = "top", 
        legend.justification = "center",
        legend.title = element_blank(),
        legend.text = element_text(color = "black", 
                                   size = 28, 
                                   family = "lato"),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm"),
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
  guides(colour = guide_legend(override.aes = list(size = 2.5))) +
  labs(title = "HOMES IN SARASOTA",
       subtitle = "LAST YEAR SOLD")

ggsave(width = 15, height = 12, device = "png", 
       here(wd, "plots", "srqsold.png"),
       bg = "white", dpi = 700)

#assessed
value <- read_dta(here(wd, "data", "value.dta"))

data <- left_join(data, value, by = "full_address")

vdata <- subset(data, select = c(lon, lat, value))

vdata <- na.omit(vdata)

ggplot(s.df) +
  geom_path(data = c, aes(long, lat, group = group), 
            colour = "skyblue", 
            size = .5) +
  geom_path(aes(long, lat, group = group), 
            colour = "darkgrey", 
            size = .3) +
  coord_sf(xlim = c(-82.7, -82.4), 
           ylim = c(27.2, 27.4),
           expand = FALSE) +
  geom_point(data = na.omit(vdata), 
             aes(lon, lat, colour = value), 
             position = position_jitter(h = 0.005),
             size = .1) +
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
                                   size = 8, 
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
       here(wd, "plots", "srqassessed.png"),
       bg = "white", dpi = 700)
