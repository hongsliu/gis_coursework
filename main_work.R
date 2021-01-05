library(sf)
library(tmap)
library(tmaptools)
library(tidyverse)
library(here)
library(janitor)
library(spdep)
library(RColorBrewer)
library(ggplot2)
library(broom)
library(corrr)
library(regclass)
library(car)

#data loading and cleaning
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

Income <- read_csv(here::here("data", "poverty_data", "annual_income.csv"),
                    na = c("NA", "n/a")) %>% 
  clean_names() %>%
  mutate(across(.cols = 2, as.numeric))

Active_life <- read_csv(here::here("data", "active_life_data", "active_life.csv"),
                    na = c("NA", "n/a")) %>% 
  clean_names() %>%
  mutate(across(.cols = 2:3, as.numeric))

GM_joined <- GM %>%
  left_join(.,
            Obesity,
            by= c("msoa11cd" = "msoa_code"))

GM_joined <- GM_joined %>%
  left_join(.,
            Poverty,
            by= c("msoa11cd" = "msoa_code"))

GM_joined <- Income %>%
  select(msoa_code, 
         total_annual_income) %>%
  left_join(GM_joined,.,by = c("msoa11cd" = "msoa_code") )

GM_joined <- Active_life %>%
  select(msoa11, 
         pr_inactive,
         pr_active) %>%
  left_join(GM_joined,.,by = c("msoa11cd" = "msoa11") )

drop.cols <- c('msoa11nmw',
               'msoa_name.x',
               'msoa_name.y',
               'la_code.y',
               'la_name.y')

joined <- GM_joined %>% select(-one_of(drop.cols))

rm(Obesity, Poverty, Income, Active_life)

qtm(joined, fill = "year6_obese_rate")

joined_f <- joined %>%
  drop_na()
joined_na <- joined[is.na(joined$year6_obese_rate),]

#spatial autocorrelation
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

joined <- joined_f %>%
  select(msoa11cd, 
         obese_rate_I,
         obese_rate_Iz) %>%
  st_drop_geometry()%>%
  left_join(joined,.,by = 'msoa11cd' )

breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)
MoranColours<- rev(brewer.pal(8, "RdGy"))

tmap_mode("view")
tm_shape(joined) +
  tm_polygons("obese_rate_Iz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              colorNA = "white", 
              textNA = "No data",
              title="Local Moran's I, year6_obese_rate in GM")

Gi_MSOA_local_obese <- joined_f %>%
  pull(year6_obese_rate) %>%
  as.vector()%>%
  localG(., MSOA.lw)

joined_f <- joined_f%>%
  mutate(obese_rate_G = as.numeric(Gi_MSOA_local_obese))


GIColours<- rev(brewer.pal(8, "RdBu"))

joined <- joined_f %>%
  select(msoa11cd, 
         obese_rate_G) %>%
  st_drop_geometry()%>%
  left_join(joined,.,by = 'msoa11cd' )

tm_shape(joined) +
  tm_polygons("obese_rate_G",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              colorNA = "white", 
              textNA = "No data",
              midpoint=NA,
              title="Gi*, year6_obese_rate in GM")


#check variables for regression
ggplot(joined, aes(x=year6_obese_rate)) + 
  geom_histogram()

ggplot(joined, aes(x=total_annual_income)) + 
  geom_histogram()

ggplot(joined, aes(x=pr_inactive)) + 
  geom_histogram()

ggplot(joined, aes(x=poverty_ahc_rate)) + 
  geom_histogram()

symbox(~poverty_ahc_rate, 
       joined, 
       na.rm=T,
       powers=seq(-3,3,by=.5))

joined <- joined %>%
  mutate(log_poverty_ahc_rate = log(poverty_ahc_rate))

ggplot(joined, aes(x=log_poverty_ahc_rate)) + 
         geom_histogram()

q1 <- qplot(x = `log_poverty_ahc_rate`, 
           y = `year6_obese_rate`, 
           data=joined)

q1 + stat_smooth(method="lm", se=FALSE, size=1)

q2 <- qplot(x = `total_annual_income`, 
           y = `year6_obese_rate`, 
           data=joined)

q2 + stat_smooth(method="lm", se=FALSE, size=1)

q3 <- qplot(x = `pr_inactive`, 
           y = `year6_obese_rate`, 
           data=joined)

q3 + stat_smooth(method="lm", se=FALSE, size=1)+
  geom_jitter()

Correlation <- joined %>%
  st_drop_geometry()%>%
  dplyr::select(year6_obese_rate,
                log_poverty_ahc_rate,
                total_annual_income,
                pr_inactive) %>%
  correlate() %>%
  focus(-year6_obese_rate, mirror = TRUE) 

rplot(Correlation)

#modelling
Regressiondata<- joined%>%
  dplyr::select(year6_obese_rate, 
                log_poverty_ahc_rate,
                total_annual_income)

model1 <- Regressiondata %>%
  lm(year6_obese_rate ~
       log_poverty_ahc_rate,
     data=.)
glance(model1)

model2 <- Regressiondata %>%
  lm(year6_obese_rate ~
       total_annual_income,
     data=.)
glance(model2)

model3 <- Regressiondata %>%
  lm(year6_obese_rate ~
       total_annual_income + log_poverty_ahc_rate,
     data=.)
glance(model3)

Regressiondata <- Regressiondata %>% drop_na()

model_data3 <- model3 %>%
  augment(., Regressiondata)

joined_f <- joined_f %>%
  mutate(model3resids = residuals(model3))

joined <- joined_f %>%
  select(msoa11cd, 
         model3resids) %>%
  st_drop_geometry()%>%
  left_join(joined,.,by = 'msoa11cd' )

#check assumptions
VIF(model3)

model_data3%>%
  dplyr::select(.resid)%>%
  pull()%>%
  qplot()+ 
  geom_histogram()+
  stat_bin(bins=20)

par(mfrow=c(2,2))
plot(model3)

#autocorrelation of residuals
DW <- durbinWatsonTest(model3)
tidy(DW)

tmap_mode("view")

tm_shape(joined) +
  tm_polygons("model3resids",
              palette = "RdYlBu")

joined_r <- joined%>%
  drop_na()
knn_MSOA <-coordsW %>%
  knearneigh(., k=4)
MSOA_knn <- knn_MSOA %>%
  knn2nb()

par(mfrow=c(1,2))
plot(MSOA_knn, st_geometry(coordsW), col="blue")
plot(MSOA_nb, st_geometry(coordsW), col="red")

MSOA.queens_weight <- MSOA_nb %>%
  nb2listw(., style="C")

MSOA.knn_4_weight <- MSOA_knn %>%
  nb2listw(., style="C")

Queen <- joined_r %>%
  st_drop_geometry()%>%
  dplyr::select(model3resids)%>%
  pull()%>%
  moran.test(., MSOA.queens_weight)%>%
  tidy()

Nearest_neighbour <- joined_r %>%
  st_drop_geometry()%>%
  dplyr::select(model3resids)%>%
  pull()%>%
  moran.test(., MSOA.knn_4_weight)%>%
  tidy()

Queen
Nearest_neighbour