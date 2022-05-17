#Title: Tree Map 7.0
#Subtitle: Year Planted - Dot Density Map
#Author: Angela Kothe
#Date: April 30th, 2022
#Purpose: Attempting to Draw a Map of Trees in SRQ by height
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
data <- read_dta(here(wd, "data", "treedate.dta"))
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
colors <- c("mediumvioletred", "tan2", "#9db802", "mediumpurple1", "#bebada")

#plot
data <- subset(data, select = -c(d2010))

mdata <- full_join(map@data, data, by = "zip_code", sort = FALSE)
mdata[is.na(mdata)] <- 0

num.dots <- select(mdata, d1970:d2000) / 10

num.dots <- round(num.dots, 0)

sp.dfs <- lapply(names(num.dots), function(x) {
  dotsInPolys(map, as.integer(num.dots[, x]), f = "random")
})

dfs <- lapply(sp.dfs, function(x) {
  data.frame(coordinates(x)[,1:2])
})


year <- c("d1970", "d1980", "d1990", "d2000")
for (i in 1:length(year)) {
  dfs[[i]]$year <- year[i]
}

dots.final <- bind_rows(dfs)
dots.final$type <- factor(dots.final$year, levels = year)

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
  scale_colour_manual(values = colors,
                      labels = c("1970", "1980", "1990", "2000")) +
  geom_point(aes(x = -82.50446, 
                 y = 27.29673), 
             size = 2,
             colour = "mediumvioletred") +
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
       subtitle = "Decade Planted \nOne Point is Ten Trees")

ggsave(width = 15, height = 12, device = "png", 
       here(wd, "plots", "treeyear.png"),
       bg = "white", dpi = 700)
