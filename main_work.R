library(sf)
library(tmap)
library(tmaptools)
library(tidyverse)
library(here)
library(janitor)
GM <- st_read(here::here("GM", "GM.shp")) %>%
  st_set_crs(27700)

Obesity <- read_csv(here::here("data", "child_obesity_data", "child_obesity_data.csv"),
                     na = c("NA", "n/a")) %>% 
  clean_names() %>%
  mutate(across(.cols = 5:8, as.numeric))

Poverty <- read_csv(here::here("data", "poverty_data", "households_in_poverty_14.csv"),
                    na = c("NA", "n/a")) %>% 
  clean_names() %>%
  mutate(across(.cols = 5:6, as.numeric))

GM_joined <- GM %>%
  left_join(.,
            Obesity,
            by= c("msoa11cd" = "msoa_code"))

GM_joined <- GM_joined %>%
  left_join(.,
            Poverty,
            by= c("msoa11cd" = "msoa_code"))%>%

drop.cols <- c('msoa11nmw',
               'msoa_name.x',
               'msoa_name.y',
               'la_code.y',
               'la_name.y')

GM_joined_cleaned <- GM_joined %>% select(-one_of(drop.cols))
rm(Obesity, Poverty)

GMlist <- c('Manchester',
            'Stockport',
            'Tameside',
            'Oldham',
            'Rochdale',
            'Bury',
            'Bolton',
            'Wigan',
            'Salford',
            'Trafford')

LA_cases_overweight <- read_csv(here::here("data", "child_obesity_data", "LA_cases_overweight.csv"),
                    na = c("NA", "n/a")) %>% 
  clean_names() %>%
  mutate(across(.cols = 3:4, as.numeric))%>%
  filter(area_name %in% GMlist)

LA_cases_obese <- read_csv(here::here("data", "child_obesity_data", "LA_cases_obese.csv"),
                                na = c("NA", "n/a")) %>% 
  clean_names() %>%
  mutate(across(.cols = 3:4, as.numeric))%>%
  filter(area_name %in% GMlist)

GM_overweight_rate = (sum(LA_cases_overweight$numerator)/sum(LA_cases_overweight$denominator)) *100
GM_obese_rate = (sum(LA_cases_obese$numerator)/sum(LA_cases_obese$denominator)) *100

GM_joined_cleaned <- GM_joined_cleaned %>%
  mutate(sir_year6_overweight = )