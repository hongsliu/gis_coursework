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
library(spgwr)
library(spatialreg)

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

Houseprice <- read_csv(here::here("data", "poverty_data", "median_house_price_18.csv"),
                        na = c("NA", "n/a")) %>% 
  clean_names() %>%
  mutate(across(.cols = 2, as.numeric))

Population <- read_csv(here::here("data", "population_data", "population.csv"),
                       na = c("NA", "n/a")) %>% 
  clean_names() %>%
  mutate(across(.cols = 2, as.numeric))

Education <-  read_csv(here::here("data", "education_data", "level_4_qualifications_and_above.csv"),
                       na = c("NA", "n/a")) %>% 
  clean_names() %>%
  mutate(across(.cols = 2, as.numeric))

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

GM_joined <- GM_joined %>%
  left_join(.,
            Houseprice,
            by= c("msoa11cd" = "msoa_code"))

GM_joined <- GM_joined %>%
  left_join(.,
            Education,
            by= c("msoa11cd" = "geography_code"))

GM_joined <- GM_joined %>%
  left_join(.,
            Population,
            by= c("msoa11cd" = "area_codes"))

drop.cols <- c('msoa11nmw',
               'msoa_name.x',
               'msoa_name.y',
               'la_code.y',
               'la_name.y')

joined <- GM_joined %>% select(-one_of(drop.cols))

rm(Obesity, Poverty, Income, Active_life, Population, Education, Houseprice)

joined$area <- st_area(joined)
joined <- joined %>%
  mutate(population_density = as.numeric(population/(area/1000000)))%>%
  mutate(pr_higher_education = level_4_qualifications_and_above/population)%>%
  mutate(total_annual_income = total_annual_income/1000)%>%
  mutate(median_house_price_2018 = median_house_price_2018/1000)

joined_f <- joined %>%
  drop_na()
joined_na <- joined[is.na(joined$year6_obese_rate),]


hi <- hist(joined$year6_obese_rate,
     xlab="Obesity prevalence in year 6 children",
     xlim=c(5,35))

ggplot(joined, aes(x=year6_obese_rate)) +
  geom_histogram(color="black", fill="white")+
  labs(x = "Obesity prevalence in year 6 children")
ggsave('Hist.png')

tmap_mode("plot")
tm <- tm_shape(joined) +
  tm_polygons("year6_obese_rate",
              palette='YlOrBr',
              colorNA = 'gray30',
              midpoint=NA,
              textNA = "No data",
              title="Percent")+
  tm_legend(legend.position = c('left','bottom'))+
  tm_layout(frame=TRUE,
            title.position=c('center','top'))+
  tm_scale_bar(width = 0.15, color.dark = 'gray60',
               position = c('right', 'bottom')) +
  tm_compass(color.dark = 'gray60', text.color ='gray60',
             position = c('right','top'))
tm

tmap_save(tm, filename = "Obesity Rate.png")


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
MoranColours<- rev(brewer.pal(8, "RdBu"))



tm1 <- tm_shape(joined) +
  tm_polygons("obese_rate_Iz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              colorNA = 'gray30',
              midpoint=NA,
              textNA = "No data",
              title="Local Moran's I")+
  tm_legend(legend.position = c('left','bottom'))+
  tm_layout(frame=TRUE,
            title.position=c('center','top'))+
  tm_scale_bar(width = 0.15, color.dark = 'gray60',
               position = c('right', 'bottom')) +
  tm_compass(color.dark = 'gray60', text.color ='gray60',
             position = c('right','top'))
tm1

tmap_save(tm1, filename = "Local Moran's I.png")

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

tm2 <- tm_shape(joined) +
  tm_polygons("obese_rate_G",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              colorNA = 'gray30',
              midpoint=NA,
              textNA = "No data",
              title="Gi*")+
  tm_legend(legend.position = c('left','bottom'))+
  tm_layout(frame=TRUE,
            title.position=c('center','top'))+
  tm_scale_bar(width = 0.15, color.dark = 'gray60',
               position = c('right', 'bottom')) +
  tm_compass(color.dark = 'gray60', text.color ='gray60',
             position = c('right','top'))


tm2

tmap_save(tm2, filename = "Gi.png")

#check variables for regression
ggplot(joined, aes(x=year6_obese_rate)) + 
  geom_histogram()

ggplot(joined, aes(x=total_annual_income)) + 
  geom_histogram()

ggplot(joined, aes(x=pr_inactive)) + 
  geom_histogram()

ggplot(joined, aes(x=poverty_ahc_rate)) + 
  geom_histogram()

symbox(~pr_poverty_ahc_rate, 
       joined, 
       na.rm=T,
       powers=seq(-3,3,by=.5))

ggplot(joined, aes(x=log(poverty_ahc_rate))) + 
  geom_histogram()

ggplot(joined, aes(x=median_house_price_2018)) + 
  geom_histogram()

ggplot(joined, aes(x=population_density)) + 
  geom_histogram()

ggplot(joined, aes(x=pr_higher_education)) + 
  geom_histogram()

symbox(~pr_higher_education, 
       joined, 
       na.rm=T,
       powers=seq(-3,3,by=.5))

ggplot(joined, aes(x=log(pr_higher_education))) + 
  geom_histogram()

joined <- joined%>%
  mutate(log_pr_higher_education = log(pr_higher_education))%>%
  mutate(log_poverty_ahc_rate = log(poverty_ahc_rate))

q1 <- qplot(x = `median_house_price_2018`, 
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

q4 <- qplot(x = `population_density`, 
            y = `year6_obese_rate`, 
            data=joined)

q4 + stat_smooth(method="lm", se=FALSE, size=1)

q5 <- qplot(x = `pr_higher_education`, 
            y = `year6_obese_rate`, 
            data=joined)

q5 + stat_smooth(method="lm", se=FALSE, size=1)


ggplot(joined, aes(x=total_annual_income, y= year6_obese_rate)) +
  geom_point(shape=1)+
  geom_smooth(method=lm,se=FALSE)+
  labs(y= "obesity rate", x = "average total annual household income")
ggsave('plot.png')

pairs(~year6_obese_rate+
        total_annual_income+
        population_density+
        median_house_price_2018
        ,data=joined,
      main="Simple Scatterplot Matrix")

Correlation <- joined %>%
  st_drop_geometry()%>%
  dplyr::select(year6_obese_rate,
                total_annual_income,
                median_house_price_2018,
                population_density,
                log_pr_higher_education,
                poverty_ahc_rate
                ) %>%
  correlate() %>%
  focus(-year6_obese_rate, mirror = TRUE) 

rplot(Correlation)

Correlation2 <- joined %>%
  st_drop_geometry()%>%
  dplyr::select(year6_obese_rate,
                total_annual_income,
                median_house_price_2018,
                population_density
  ) %>%
  correlate() %>%
  focus(-year6_obese_rate, mirror = TRUE) 

rplot(Correlation2)


#modelling
Regressiondata<- joined%>%
  dplyr::select(year6_obese_rate, 
                total_annual_income,
                median_house_price_2018,
                population_density)

model0 <- Regressiondata %>%
  lm(year6_obese_rate ~
       population_density,
     data=.)
glance(model0)

model1 <- Regressiondata %>%
  lm(year6_obese_rate ~
       median_house_price_2018,
     data=.)
glance(model1)

model2 <- Regressiondata %>%
  lm(year6_obese_rate ~
       total_annual_income,
     data=.)
glance(model2)

model3 <- Regressiondata %>%
  lm(year6_obese_rate ~
       total_annual_income +
       median_house_price_2018 + 
       population_density,
     data=.)
summary(model3)

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

tm3 <- tm_shape(joined) +
  tm_polygons("model3resids",
              palette='RdYlBu',
              colorNA = 'gray30',
              midpoint=NA,
              textNA = "No data",
              title="Residuals")+
  tm_legend(legend.position = c('left','bottom'))+
  tm_layout(frame=TRUE,
            title.position=c('center','top'))+
  tm_scale_bar(width = 0.15, color.dark = 'gray60',
               position = c('right', 'bottom')) +
  tm_compass(color.dark = 'gray60', text.color ='gray60',
             position = c('right','top'))
tm3
tmap_save(tm3, filename = "Residuals.png")


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

#The spatial lag model
slag_dv_model3_queen <- lagsarlm( year6_obese_rate~ total_annual_income + 
                                   median_house_price_2018+
                                    population_density, 
                                 data = joined_r, 
                                 nb2listw(MSOA_nb, style="C"), 
                                 method = "eigen")
tidy(slag_dv_model3_queen)
glance(slag_dv_model3_queen)
t <- summary(slag_dv_model3_queen)

slag_dv_model3_knn4 <- lagsarlm(year6_obese_rate~ total_annual_income + 
                                  median_house_price_2018+
                                  population_density, 
                                data = joined_r, 
                                nb2listw(MSOA_knn, 
                                         style="C"), 
                                method = "eigen")
tidy(slag_dv_model3_knn4)

joined_r <- joined_r %>%
  mutate(slag_dv_model3_knn_resids = residuals(slag_dv_model3_knn4))

KNN4Moran <- joined_r %>%
  st_drop_geometry()%>%
  dplyr::select(slag_dv_model3_knn_resids)%>%
  pull()%>%
  moran.test(., MSOA.knn_4_weight)%>%
  tidy()

KNN4Moran

#GWR
st_crs(joined_f) = 27700
joined_f_sp <- joined_f %>%
  as(., "Spatial")
st_crs(coordsW) = 27700
coordsWsp <- coordsW %>%
  as(., "Spatial")
coordsWsp

GWRbandwidth <- gwr.sel(year6_obese_rate ~ total_annual_income +
                          median_house_price_2018 + 
                          population_density,
                        data=joined_f_sp,
                        adapt=T)

gwr.model = gwr(year6_obese_rate ~ total_annual_income +
                  median_house_price_2018 + 
                  population_density,
                data = joined_f_sp, 
                coords=coordsWsp, 
                adapt=GWRbandwidth, 
                hatmatrix=TRUE, 
                se.fit=TRUE)
gwr.model

results <- as.data.frame(gwr.model$SDF)
names(results)

joined2 <- joined_f %>%
  mutate(coefannualin = results$total_annual_income,
         coefmedianhou = results$median_house_price_2018,
         coefpopuladen = results$population_density)

tm_shape(joined2) +
  tm_polygons(col = "coefannualin", 
              palette = "RdBu", 
              alpha = 0.5)

tm4 <- tm_shape(joined2) +
  tm_polygons("coefannualin",
              palette='RdBu',
              colorNA = 'gray30',
              midpoint=NA,
              textNA = "No data",
              title="Coefficient")+
  tm_legend(legend.position = c('left','bottom'))+
  tm_layout(frame=TRUE,
            title.position=c('center','top'))+
  tm_scale_bar(width = 0.15, color.dark = 'gray60',
               position = c('right', 'bottom')) +
  tm_compass(color.dark = 'gray60', text.color ='gray60',
             position = c('right','top'))
tm4

tmap_save(tm4, filename = "GWR_Annual_Income.png")