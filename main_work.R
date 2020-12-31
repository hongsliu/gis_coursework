library(sf)
library(tmap)
library(tmaptools)
library(tidyverse)
library(here)
library(janitor)
library(spdep)
library(RColorBrewer)
GM <- st_read(here::here("GM", "GM.shp")) %>%
  st_set_crs(27700)

Obesity <- read_csv(here::here("data", "child_obesity_data", "child_obesity_data.csv"),
                     na = c("NA", "n/a")) %>% 
  clean_names() %>%
  mutate(across(.cols = 5:6, as.numeric))

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
            by= c("msoa11cd" = "msoa_code"))

drop.cols <- c('msoa11nmw',
               'msoa_name.x',
               'msoa_name.y',
               'la_code.y',
               'la_name.y')

GM_joined_cleaned <- GM_joined %>% select(-one_of(drop.cols))

rm(Obesity, Poverty)

joined <- GM_joined_cleaned %>%
  mutate(log_poverty_ahc_rate = log(poverty_ahc_rate))

qtm(joined, fill = "year6_obese_rate")
plot(joined$log_poverty_ahc_rate, joined$year6_obese_rate)

Regressiondata<- joined%>%
  dplyr::select(year6_obese_rate, 
                log_poverty_ahc_rate)

model1 <- Regressiondata %>%
  lm(year6_obese_rate ~
       log_poverty_ahc_rate,
     data=.)
summary(model1)

joined_f <- joined %>%
  drop_na()

coordsW <- joined_f%>%
  st_centroid()%>%
  st_geometry()
plot(coordsW,axes=TRUE)

MSOA_nb <- joined_f %>%
  poly2nb(., queen=T)
plot(MSOA_nb, st_geometry(coordsW), col="red")
plot(joined_f$geometry, add=T)
MSOA.lw <- MSOA_nb %>%
  nb2listw(., style="C")
head(MSOA.lw$neighbours)

I_MSOA_Global_obese <- joined_f %>%
  pull(year6_obese_rate) %>%
  as.vector()%>%
  moran.test(., MSOA.lw)

I_MSOA_Global_obese

C_MSOA_Global_obese <- joined_f %>%
  pull(year6_obese_rate) %>%
  as.vector()%>%
  geary.test(., MSOA.lw)

C_MSOA_Global_obese

G_MSOA_Global_obese <- joined_f %>%
  pull(year6_obese_rate) %>%
  as.vector()%>%
  globalG.test(., MSOA.lw)

G_MSOA_Global_obese

I_MSOA_Local_obese <- joined_f %>%
  pull(year6_obese_rate) %>%
  as.vector()%>%
  localmoran(., MSOA.lw)%>%
  as.tibble()

slice_head(I_MSOA_Local_obese, n=5)

joined_f <- joined_f %>%
  mutate(obese_rate_I = as.numeric(I_MSOA_Local_obese$Ii))%>%
  mutate(obese_rate_Iz =as.numeric(I_MSOA_Local_obese$Z.Ii))


breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)
MoranColours<- rev(brewer.pal(8, "RdGy"))

tmap_mode("view")
tm_shape(joined_f) +
  tm_polygons("obese_rate_Iz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, year6_obese_rate in GM")

Gi_MSOA_local_obese <- joined_f %>%
  pull(year6_obese_rate) %>%
  as.vector()%>%
  localG(., MSOA.lw)

joined_f <- joined_f%>%
  mutate(obese_rate_G = as.numeric(Gi_MSOA_local_obese))

GIColours<- rev(brewer.pal(8, "RdBu"))

tm_shape(joined_f) +
  tm_polygons("obese_rate_G",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, year6_obese_rate in GM")

