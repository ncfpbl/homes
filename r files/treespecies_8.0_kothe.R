#Title: Tree Map 8.0
#Subtitle: The Dot Density Map
#Author: Angela Kothe
#Date: March 10th, 2022
#Purpose: Attempting to Draw a Map of Trees in SRQ by Species
#Requires: trees.dta from OpenSarasota Website
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
data <- read_dta(here(wd, "data", "treetype.dta"))
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
colors <- c("#fd7c84", "#025b0e", "#9db802", "#000080", "#80b1d3", 
            "mediumvioletred", "mediumpurple1")

#plot
mdata <- left_join(map@data, data, by = "zip_code", sort = FALSE)
mdata[is.na(mdata)] <- 0

num.dots <- select(mdata, other:braz) / 20

sp.dfs <- lapply(names(num.dots), function(x) {
  dotsInPolys(map, as.integer(num.dots[, x]), f="random")
})

dfs <- lapply(sp.dfs, function(x) {
  data.frame(coordinates(x)[,1:2])
})

type <- c("other", "oak", "palm", "pine", "holly", "crape", "braz")
for (i in 1:length(type)) {
  dfs[[i]]$type <- type[i]
}

dots.final <- bind_rows(dfs)
dots.final$type <- factor(dots.final$type, levels = type)

font_add_google(name = "Lato", family = "lato") 
showtext_auto()

ggplot(s.df) +
  geom_path(data = c, aes(long, lat, group = group), 
            colour = "skyblue", 
            size = 1) +
  geom_path(aes(long, lat, group = group), 
            colour = "darkgrey", 
            size = .3) +
  coord_sf(xlim = c(-82.7, -82.4), 
           ylim = c(27.2, 27.4),
           expand = FALSE) +
  geom_point(data = dots.final, 
             aes(x, y, colour = type), 
             size = 2) +
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
  labs(title = "TREES IN SARASOTA", 
       subtitle = "Species Planted \nOne Point is Twenty Trees")

ggsave(width = 15, height = 12, device = "png", 
       here(wd, "plots", "treetype.png"),
       bg = "white", dpi = 700)
