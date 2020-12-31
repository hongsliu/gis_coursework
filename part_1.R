library(sf)
library(tmap)
library(tmaptools)
library(tidyverse)
library(here)
GM <- st_read(here::here("GM", "GM.shp")) %>%
  st_set_crs(27700)

