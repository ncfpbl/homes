#Title: Homes Built in Sarasota
#Subtitle: The Dot Density Map
#Author: Angela Kothe
#Date: March 10th, 2022
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

wd <- here("Documents", "Github", "pbl")

#data, tree height; zip code and roads shapefile
data <- read_dta(here(wd, "data", "homesbuilt.dta"))
data$zip_code <- as.character(data$zipcode)

map <- readOGR(here(wd, "shapes", "zips", "ZipCode.shp"))
map <- spTransform(map, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
map.df <- fortify(map, region = "zip_code")

s <- readOGR(here(wd, "shapes", "streets", "Street.shp"))
s <- spTransform(s, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
s.df <- fortify(s, region = "zipr")

c <- readOGR(here(wd, "shapes", "coast", "CoastalRegulatoryLine.shp"))
c <- spTransform(c, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
c.df <- fortify(c)

#palette
colors <- c("#845ec2", "#d65db1", "coral3", "limegreen", "#0d759f", "#f9f871",
            "skyblue", "pink", "orange", "tomato", "black")

#plot
mdata <- left_join(map@data, data, by = "zip_code", sort = FALSE)

num.dots <- select(mdata, b1870:b2020) / 10
num.dots <- round(num.dots, 0)

#max of seven categories/variables in the num.dots, split in half
num.dots <- num.dots[c(5:15)]

sp.dfs <- lapply(names(num.dots), function(x) {
  dotsInPolys(map, as.integer(num.dots[, x]), f = "random")
})

dfs <- lapply(sp.dfs, function(x) {
  data.frame(coordinates(x)[,1:2])
})

year <- c("1920", "1930", "1940", "1950", "1960", "1970", "1980", "1990", 
          "2000", "2010", "2020")
for (i in 1:length(year)) {
  dfs[[i]]$year <- year[i]
}

dots.final <- bind_rows(dfs)
dots.final$year <- factor(dots.final$year, levels = year)

#separate it out for visual clarity
dots.1920 <- filter(dots.final, year == "1920")
dots.1930 <- filter(dots.final, year == "1930")
dots.1940 <- filter(dots.final, year == "1940")
dots.1950 <- filter(dots.final, year == "1950")
dots.1960 <- filter(dots.final, year == "1960")
dots.1970 <- filter(dots.final, year == "1970")
dots.1980 <- filter(dots.final, year == "1980")
dots.1990 <- filter(dots.final, year == "1990")
dots.2000 <- filter(dots.final, year == "2000")
dots.2010 <- filter(dots.final, year == "2010")
dots.2020 <- filter(dots.final, year == "2020")

font_add_google(name = "Lato", family = "lato") 
showtext_auto()

ggplot(s.df) +
  geom_path(data = c, aes(long, lat, group = group), 
            colour = "skyblue", 
            size = 1) +
  geom_path(aes(long, lat, group = group), 
            colour = "darkgrey", 
            size = .3) +
  geom_point(data = dots.1980, aes(x, y, colour = year), 
             size = 1, alpha = .2) +
  geom_point(data = dots.1970, aes(x, y, colour = year), 
             size = 1, alpha = 1) +
  geom_point(data = dots.2020, aes(x, y, colour = year), 
             size = 1, alpha = .6) +
  geom_point(data = dots.2000, aes(x, y, colour = year), 
             size = 1, alpha = .6) +
  geom_point(data = dots.1990, aes(x, y, colour = year), 
             size = 1, alpha = .6) +
  geom_point(data = dots.1960, aes(x, y, colour = year), 
             size = 1, alpha = .6) +
  geom_point(data = dots.1950, aes(x, y, colour = year), 
             size = 1, alpha = .6) +
  geom_point(data = dots.2010, aes(x, y, colour = year), 
             size = 1, alpha = .6) +
  geom_point(data = dots.1940, aes(x, y, colour = year), 
             size = 1, alpha = .6) +
  geom_point(data = dots.1930, aes(x, y, colour = year), 
             size = 1, alpha = .6) +
  geom_point(data = dots.1920, aes(x, y, colour = year), 
             size = 1, alpha = .6) +
  coord_sf(xlim = c(-82.7, -82.4), 
           ylim = c(27.2, 27.4),
           expand = FALSE) +
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
       subtitle = "By Zip Code \nYear Built",
       caption =  "One Point is Ten Homes")

ggsave(width = 15, height = 12, device = "png", 
       here(wd, "plots", "homesbuilt.png"), dpi = 700)
