
library(tidyverse)
library(lubridate)

life_expectancy <- read_csv("life_expectancy_over_years.csv")

#convert Year column to date data 

life_expectancy$Year <-make_date(life_expectancy$Year)

life_expectancy

ggplot(data=life_expectancy,aes(x=Year,y=Life_expectancy_in_years)) +
  geom_line(size =1)+
  labs(title = "Life Expectancy by Year")+
  ylab("Life Expectancy")+
  xlab("Year")+
  scale_x_date(date_breaks = "2 year",date_labels = "%b-%y")+
  theme(axis.text.x = element_text(angle = 45))


#Question 4, Moran's I for Raccial Status and Housing 

Franklin_svi<- st_read("franklin_svi_and_themes.geojson")

nb <- poly2nb(Franklin_svi, queen = TRUE)

weights <- nb2listw(nb, style="W", zero.policy=TRUE)
weights_style_B <- nb2listw(nb, style="B", zero.policy=TRUE)

moran_race <- moran.mc(Franklin_svi$RPL_THEME3, weights, nsim = 999)
moran_race

moran_housing <- moran.mc(Franklin_svi$RPL_THEME4, weights, nsim = 999)
moran_housing 


#Questions 5, SVI hot spots in Franklin Co. 
library(sf)
library(RColorBrewer)
library(classInt)
library(gridExtra)

Franklin_svi<- st_read("franklin_svi_and_themes.geojson")

svi_intervals <- classIntervals(Franklin_svi$SVI, n = 5, style = "jenks")

Franklin_svi <- Franklin_svi %>%
  mutate(SVI_class = cut(SVI,svi_intervals$brks, include.lowest = TRUE))

source("/users/PAS2562/abbyshores24/osc_classes/PUBHLTH_5015_OSU/materials/Spatial/custom_functions.R")

SVI_Map <- generate_choropleth_map(sf_object = Franklin_svi, 
                                column_name_as_string = "SVI",
                                classification_scheme = "quantile",
                                number_of_classes = 5,
                                color_palette = "YlOrRd",
                                plot_title = "SVI")

SVI_Map

library(leaflet)
library(ggrepel)
library(spdep)

nb <- poly2nb(Franklin_svi, queen = TRUE)

weights <- nb2listw(nb, style="W", zero.policy=TRUE)
weights_style_B <- nb2listw(nb, style="B", zero.policy=TRUE)

local_moran_svi <- localmoran_perm(Franklin_svi$SVI, weights, nsim = 99999)
local_moran_svi

svi_hotspots <-  hotspot(local_moran_svi, Prname = "Pr(z != E(Ii)) Sim", cutoff = 0.05,droplevels = FALSE)

svi_hotspot_map <-ggplot(data = Franklin_svi) +
  geom_sf(aes(fill = svi_hotspots)) +
  scale_fill_manual(values = c("Low-Low" = "blue", 
                               "High-High"= "red", 
                               "High-Low" = "pink",
                               "Low-High" = "lightblue"), na.value = "white") +
  labs(title = "svi_hotspot_map",
       subtitle = "SVI Clusters",
       fill = "SVI Categories")

grid.arrange(SVI_Map, svi_hotspot_map, ncol=2)
